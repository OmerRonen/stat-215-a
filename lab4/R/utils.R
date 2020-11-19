loadData <- function(img_folder, img_names){
  # function to load image data
  # img_folder: folder contains image data; img_names: img txt file names
  # return a data frame contains 3 images data 
  
  imgs <- data.frame()
  for(file in img_names){
    # read in image data
    img <- read.table(paste0(img_folder, '/', file))
    img['img_name'] <- file
    imgs <- rbind(imgs, img)
  }
  # rename columns
  colnames(imgs) <- c('y','x','truth','ndai','sd','corr',
                      'df','cf','bf','af','an','img_name')
  # add levels to truth column
  imgs$truth <- factor(imgs$truth, labels = c('no cloud', 'unlabeled', 'cloud'))
  return(imgs)
}


plotImage <- function(name, data){
  # function to plot expert labeled pixels
  # name: name of plotting image; data: image data
  # return a plot of an image
  
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = as.factor(truth))) +
    scale_fill_viridis(discrete = TRUE) +
    theme_void() +
    labs(fill = 'Expert label', title = gsub('.txt', '', name))
  return(plot)
}

extractFeatureImportance_RF <- function(x, proportion, data, top){
  # function to extract op 3 important features from random forest model
  # x: dummy variable, proportion: proportion of original data to subsampling;
  # data: original data; top: how many top important features to extract
  
  # subsample 80% of the data
  sample_idx <- sample(1:nrow(data), proportion*nrow(data))
  # fit random forest model
  train_features <- c('truth','ndai','sd','corr','df','cf','bf','af','an')
  cv_fit_rf <- randomForest(truth~., data[sample_idx, train_features])
  # obtain feature importance
  cv_rf_importance <- importance(cv_fit_rf)
  # extract top important feature
  important_vars <- rownames(cv_rf_importance)[
    order(cv_rf_importance,decreasing = TRUE)[1:top]]
  return(important_vars)
}


.getSplit <- function(fig_number, data, pct){
  name <- paste('image', fig_number, '.txt',sep = '')
  img.data <- data[data$img_name == name, ]
  crop <- getCrop(img.data, pct)
  
  min.x <- min(crop$x)
  min.y <- min(crop$y)
  max.x <- max(crop$x)
  max.y <- max(crop$y)
  
  x_values <- img.data$x > min.x & img.data$x < max.x
  y_values <- img.data$y > min.y & img.data$y < max.y
  dataset <- ifelse(x_values & y_values, 'test', 'train')
  img.data$dataset <- dataset
  return(img.data)
}

plotSplit <- function(fig_number, data, pct){
  
  set.seed(1)
  img.data <- .getSplit(fig_number, data, pct)
  group.colors.test <- c('cloud' = "snow2", 
                         'no cloud' = "deepskyblue", 
                         'unlabeled' = "darkgrey")
  group.colors.train <- c('cloud' = "snow2", 
                          'no cloud' = "deepskyblue", 
                          'unlabeled' = "darkgrey",
                          'cropped' = "yellow")
  
  
  plot.train <- ggplot(img.data, aes(x,y)) +
    geom_tile(aes(fill = as.factor(truth))) +
    scale_fill_viridis(discrete = TRUE) +
    scale_fill_manual(values = group.colors.train) +
    theme_void() +
    labs(fill = 'Expert label', title = 'train')
  plot.test <- ggplot(crop, aes(x,y)) +
    geom_tile(aes(fill = as.factor(truth))) +
    scale_fill_viridis(discrete = TRUE) +
    scale_fill_manual(values = group.colors.test) +
    theme_void() +
    theme(legend.position = "none")+
    labs(fill = 'Expert label', title = 'test')
  
  g <- grid.arrange(plot.test,plot.train , nrow = 1, widths=c(1,3))
  ggsave(here(paste('figures/split',fig_number,'.png', sep = '')),
         g, width = 3, height = 3)
  
}

getCrop <- function(img, pct){
  
  alpha <- sqrt(pct)
  
  min.x <- min(img$x)
  min.y <- min(img$y)
  max.x <- max(img$x)
  max.y <- max(img$y)
  
  dx <- floor(alpha * (max.x - min.x))
  dy <- floor(alpha * (max.y - min.y))
  
  x.start <- runif(1 ,min.x, max.x-dx)
  y.start <- runif(1, min.y, max.y-dy)
  
  x_vec <- img$x
  y_vec <- img$y
  
  x_values <- x_vec > x.start & x_vec < x.start+dx
  y_values <- y_vec > y.start & y_vec < y.start+dy
  
  crop <- img[x_values & y_values, ]
  return(crop)
  
}
getTrainTest <- function(pct){
  data <- loadData()
  data$dataset <- 'None'
  for (n in c(1:3)) {
    name <- paste('image', n, '.txt',sep = '')
    
    split_ds <- .getSplit(n, data, 0.2)
    
    idx <- data$img_name==name
    data[idx, ] <- split_ds
    
  }
  write.csv(data,here('data/train_test.csv'), row.names = FALSE)
}

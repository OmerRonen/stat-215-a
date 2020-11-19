splitData <- function(data){
  # a function to split data into training, testing and folds for cv
  # data: image dataset
  # return the data frame with indicator columns for the split
  # and save the data into data folder
  
  data <- data %>%
    group_by(img_name) %>%
    # divide the image into 4 smaller image on the y axis
    mutate(sec = cut_interval(y, 4, label = FALSE))
  data$sec <- factor(data$sec, c(4,3,2,1))
  # create a categorical variable with image name and section for cv-folds
  data$img_sec <- sapply(1:nrow(data), FUN = function(x){
    gsub('.txt', paste0('_', data$sec[x]), data$img_name[x])})
  # we will do 2 different test: on a whole image and on random folds
  # select testing folds
  set.seed(1)
  testing_sec <- sample(unique(data$img_sec),3)
  data$dataset1 <- ifelse(data$img_sec %in% testing_sec,'testing','training')
  # choose img2 as testing data
  data$dataset2 <- ifelse(data$img_name == 'image2.txt','testing','training')
  # save train test split data
  write.csv(data, 'data/train_test_split_1.csv', row.names = FALSE)
  return(data)
}

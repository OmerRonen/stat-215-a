library(viridis)
library(ggplot2)
library(dplyr)
library(here)
library(keras)
library(fmsb)

source(here('R/utils.R'))

plotCloud <- function(name, data){
  # function to plot expert labeled pixels
  # name: name of plotting image; data: image data
  # return a plot of an image
  group.colors.test <- c('cloud' = "snow2", 
                         'no cloud' = "deepskyblue", 
                         'unlabeled' = "darkgrey")
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = as.factor(truth))) +
    scale_fill_manual(values = group.colors.test) +
    theme_void() +
    labs(fill = 'Expert label', title = 'Gt')
  return(plot)
}

plotNadi <- function(name, data){
  # function to plot ndai feature
  # name: name of plotting image; data: image data
  # return a plot of an image
  
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = ndai)) +
    scale_fill_viridis() +
    theme_void() +
    labs(title = 'NDAI')
  return(plot)
}

plotCorr <- function(name, data){
  # function to plot expert corr feature
  # name: name of plotting image; data: image data
  # return a plot of an image
  
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = corr)) +
    scale_fill_viridis() +
    theme_void() +
    labs(title = 'CORR')
  return(plot)
}


plotAN <- function(name, data){
  # function to plot expert AN feature
  # name: name of plotting image; data: image data
  # return a plot of an image
  
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = an)) +
    scale_fill_viridis() +
    theme_void() +
    labs(title = 'AN')
  return(plot)
}

plotSd <- function(name, data){
  # function to plot expert corr feature
  # name: name of plotting image; data: image data
  # return a plot of an image
  
  plot <- ggplot(data %>% filter(img_name == name), aes(x,y)) +
    geom_tile(aes(fill = sd)) +
    scale_fill_viridis() +
    theme_void() +
    labs(title = 'SD')
  return(plot)
}

plotPredVsGT <- function(predictions, data, image.name){
  # Produces a plot of the prediction vs ground truth
  # predictions: vector of binary predictions 1 is cloud 0 is no cloud
  # data: the data set which includes the x,y,truth and image_name variables
  # image.name: the image that we want to plot, i.e "image1.txt"
  # this function calls plotImage from the Utils file
  
  predictions.str <- ifelse(predictions==1, 'cloud', 'no_cloud')
  img.test.ind <- data$img_name==image.name
  img.test <- data[img.test.ind, ]
  img.pred <- predictions.str[img.test.ind]
  plot.gt <- plotImage(img.test, img.test$truth,'gt')
  plot.pred <- plotImage(img.test, img.pred, 'prediction') +
    theme(legend.position = "none")
  
  g <- grid.arrange(plot.pred, plot.gt,nrow = 1, widths=c(1,1.5))
  return(g)}

plotClassNdai <- function(name, data){
  # Produces a plot of the gt next to features heat map
  # name: name of plotting image; data: image data

  plot.ndai <- plotNadi(name, data)+ theme(legend.position = "none")
  plot.corr <- plotCorr(name, data)+ theme(legend.position = "none")
  plot.sd <- plotSd(name, data)+ theme(legend.position = "none")
  
  plot.cloud <- plotCloud(name, data) 
  g <- grid.arrange(plot.ndai,plot.corr,plot.sd,plot.cloud ,
                    nrow = 2, widths=c(0.5,0.5,0.5,0.75))
  img.name <- strsplit(name, '.txt')[[1]][1]
  ggsave(here(paste('figures/feature_heatmap_',img.name, '.png', sep='')),
         g, width = 6, height = 3)
  
}

plot.spider <- function(score_obs){
  score_obs = combine_pred_long
  roc <- plotROC_PRcurve(score_obs)
  metrics <- calculateMetrics(0.5, score_obs, roc[[3]])
  metric_labels <- c('Balanced Accuracy', 'F value', 'PR AUC',
                     'Precision', 'Recall', 'ROC AUC')
  metrics$metrics <- factor(metrics$metrics, 
                            labels = metric_labels)
  models <- unique(metrics$model_type)
  metrics.plot <- rbind(rep(1,6), rep(0.8,6))
  for (model in models){
    model.metrics <- metrics[metrics$model_type==model, ]
    metrics.plot <- rbind(metrics.plot, model.metrics$values) 
  }
  metrics.plot <- as.data.frame(metrics.plot)
  colnames(metrics.plot) <- unique(metrics$metrics)
  rownames(metrics.plot)[3:nrow(metrics.plot)] <- as.character(models)
  colors_border <- c( rgb(0.2,0.5,0.5,0.9), 
                   rgb(0.8,0.2,0.5,0.9), 
                   rgb(0.7,0.5,0.1,0.9))
  colors_in <- c( rgb(0.2,0.5,0.5,0.4), 
               rgb(0.8,0.2,0.5,0.4), 
               rgb(0.7,0.5,0.1,0.4))
  
  
  radarchart(metrics.plot,
             #custom polygon
             pcol=colors_border , plwd=2 , plty=1, #  pfcol=colors_in,
             #custom the grid
             cglcol="grey", cglty=1,  cglwd=1,
             #custom labels
             vlcex=0.5
  )
  legend(x=1, y=0.5, legend = rownames(metrics.plot[-c(1,2),]), 
         bty = "n", pch=10 , col=colors_in , cex=1, pt.cex=1.2,
         x.intersp=0.8)

}

plotPairs <- function(data){
  # imgs <- loadData()
  labeled_img <- imgs %>% filter(truth != 'unlabeled')
  labeled_img$truth <- factor(labeled_img$truth, labels = c('no cloud', 'cloud'))
  set.seed(7)
  sample_idx <- sample(1:nrow(labeled_img), 1000)
  eda_imgs <- labeled_img[sample_idx,]
  cloud_labels <- factor(eda_imgs$truth)
  ggpairs(eda_imgs, mapping = ggplot2::aes(color = truth), 
          columns = c('sd','corr','ndai','df','cf','bf','af','an'), 
          lower = list(continuous = wrap('points', alpha = 0.5, size = 0.5),
                       combo = wrap('dot', alpha = 0.4, size = 0.5)),
          upper = list(continuous = wrap("cor", size=1))) +
    theme(axis.ticks=element_blank(),axis.text = element_blank())+
    scale_fill_viridis(discrete = TRUE, begin = 0, end = 0.5) +
    scale_color_viridis(discrete = TRUE, begin = 0, end = 0.5)
}

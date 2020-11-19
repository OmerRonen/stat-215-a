library(doParallel)
library(caret)
# library(DMwR)

trainLogitmodel <- function(train_data, train_features){
  # function to train logistic model
  # train_data: image df, train_features: features used to train model
  # return and save trained model in the end
  cl <- makePSOCKcluster(5)
  registerDoParallel(cl)
  
  n_folds <- length(levels(train_data$fold))
  group_folds_image2 <- lapply(1:n_folds,
                               FUN = function(x){which(train_data$fold == x)})
  folds_name_image2 <- train_data %>%
    distinct(fold, img_sec) %>% select(img_sec)
  names(group_folds_image2) <- folds_name_image2$img_sec
  
  # define train control for train function
  train_control <- trainControl(method = "cv", 
                                index = group_folds_image2,
                                classProbs = TRUE,
                                summaryFunction = twoClassSummary,
                                verboseIter = FALSE,
                                savePredictions = 'final',
                                allowParallel = TRUE)
 
  # train the model on training set
  logit_model_image2 <- caret::train(truth ~ .,
                                     data = train_data[,train_features],
                                     method = "glm",
                                     trControl = train_control,
                                     family = binomial(),
                                     preProc = c('center','scale'),
                                     metric = 'ROC')
  
  # save the model
  stopCluster(cl)
  return(logit_model_image2)
}

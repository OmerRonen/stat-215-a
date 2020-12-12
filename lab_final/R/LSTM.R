library(keras)


.lstmPred <- function(model, x){
  X = array(x, dim=c(length(x),1,1))
  y_pred = model %>% predict(X)
  return(y_pred)
  
}


getLSTMPreds <- function(){
  return(.getPreds(.lstmPred, .getLstmModel))
}

.getLstmModel <- function(data,today, cases=FALSE){
  data <- data[data$date < today, ]
  x <- data$cumdeathstot
  y <- data$cumdeathstot_t1
  if (cases){
    x <- data$cumcasestot
    y <- data$cumcasestot_t1
  }
  lstm = keras_model_sequential() %>% 
    layer_lstm(units=12, input_shape=c(1, 1), activation="relu") %>%
    layer_dense(units=1)  
  
  
  lstm %>% compile(
    loss = "mse",
    optimizer =  "adam")
  X = array(x, dim=c(length(x),1,1))
  
  lstm %>% fit(X, y, epochs = 20,verbose = 1)
  return(lstm)
  
}

testPredsLSTM <- function(){
  data <- .getModel3Data()
  # data <- data[data$date > '2020-11-01', ]
  
  .vizKDysPred(5, data,.lstmPred, .getLstmModel)
  
}
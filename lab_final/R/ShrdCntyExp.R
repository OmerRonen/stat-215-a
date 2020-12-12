library(here)
source(here('R/cleanData.R'))
source(here('R/sanityCheck.R'))


.getModel3Data <- function(){
  # processing the data for the models
  data.fname <- here('data/model3_data.csv')
  if (file.exists(data.fname)){
    data <- read.csv(data.fname)
    data$date <- as.Date(data$date, format="%Y-%m-%d")
    return(data)
  }

  data <- getDataFull()
  dates <- as.Date(data$date, format="%Y-%m-%d")
  model.data <- data.frame(cbind(data$fipscounty, data$cumdeathstot, data$cumcasestot, dates))
  dups <- !duplicated(model.data)
  model.data <- model.data[dups, ]
  colnames(model.data) <- c('fipscounty', 'cumdeathstot', 'cumcasestot', 'date')
  model.data$date <- dates[dups]
  model.data <- model.data[order(model.data$fipscounty, as.Date(model.data$date, format="%Y-%m-%d"), decreasing=TRUE),]
  x <- model.data
  y <- model.data
  for (c in unique(model.data$fipscounty)){
    max.exlude <- x$fipscounty == c & x$date == max(x$date[x$fipscounty==c])
    min.exlude <- y$fipscounty == c & y$date == min(y$date[y$fipscounty==c])

    x <- x[!max.exlude,]

    y <- y[!min.exlude,]

  }

  y$date <- y$date -1

  x$cumdeathstot_t1 <- y$cumdeathstot
  x$cumcasestot_t1 <- y$cumcasestot

  x$more3 <- x$cumdeathstot >= 3
  return (x)
}

.whiten <- function(x){
  # whitening operation
  std <- sqrt(var(x))
  return((x-mean(x))/std)
}

.glmPred <- function(model, x){
  y_hat_glm <- predict(model, type = 'response', newdata = data.frame(x))
  return(y_hat_glm)
}

kDysPred <- function(k, data, f.pred, f.model, cases=FALSE){
  var.name <- 'cumdeathstot'
  if (cases){
    var.name <- 'cumcasestot'
  }
  predictions <- data.frame(cbind(as.Date(data$date), data$fipscounty, NA))
  colnames(predictions) <- c('date','fipscounty', 'pred')
  zero.ind <- !data$more3
  predictions$date <- as.Date(data$date)
  data <- data[data$more3, ]
  first.day <- min(predictions$date)
  dates <- c(unique(predictions$date[!zero.ind]), max(predictions$date[!zero.ind])+1)
  for (day in dates){

    if (day==first.day){
      next
    }
    day.model <- tryCatch(
      {
        f.model(data, day, cases)
      },
      error = function(e){
        print(e)
        return(NA)
      }
    )
    past.ind <- data$date<=day
    past.data <- data[past.ind, ]
    current.step <- past.data[, var.name]
    day.ind <- past.data$date==day
    
  for (i in c(1:k)){
    next.step <- tryCatch(
      {
        pmax(current.step, c(f.pred(day.model, .whiten(log(current.step+1)))))
      },
      error = function(e){
        return(current.step)
      }
    )
    if (sum(is.na(next.step))==0)
      {current.step <- next.step}
  }
    pred.ind <- predictions$date==day & !zero.ind
    predictions$pred[pred.ind] <- current.step[day.ind]}
  predictions$pred[zero.ind] <- 0
  predictions$predict_date <- predictions$date + k
  predictions <- predictions %>% select(-'date')
  predictions$k <- k
  counties <- c(6001 , 6013,  6019,  6039,  
                6041,  6047,  6055,  6075,
                6081,  6085,  6095,  6097,
                6099,  6107, 55075)
  predictions <- predictions %>% filter(fipscounty %in% counties)
  return(predictions)
}

.vizKDysPred <- function(k, data,f.pred, f.model ,county=6107){
  
  pred <- kDysPred(k,data, f.pred, f.model, cases = TRUE)
  data.c <- data[data$fipscounty==county ,]
  pred.c <- pred[pred$fipscounty==county,]
  ggplot(data =pred.c , aes(x=predict_date,y=pred) ) + geom_point(aes(colour='Prediction')) +
    geom_point(data = data.c, aes(x=date+1, y=cumcasestot_t1, colour='Truth'))
}

.getGlmModel <- function(data, today ,cases=FALSE){
  data.past <- data[data$date < today, ]
  x <- data.past$cumdeathstot
  y <- data.past$cumdeathstot_t1
  if (cases){
    x <- data.past$cumcasestot
    y <- data.past$cumcasestot_t1
  }
  x <- .whiten(log(x+1) + rnorm(n=length(x), sd=0.001))
  poiss.glm <- glm(y ~ x, poisson)
return(poiss.glm)}

.getPreds <- function(f.pred, f.model, cases=FALSE){
  # this function returns the prediction k step forward for k = (3,5,7,14)
  # for the glm model the date column is the date on which we make the prediction
  
  data <- .getModel3Data()
  pred.3 <-  kDysPred(3,data, f.pred, f.model, cases)
  pred.5 <-  kDysPred(5,data, f.pred, f.model, cases)
  pred.7 <-  kDysPred(7,data, f.pred, f.model, cases)
  pred.14 <-  kDysPred(14,data, f.pred, f.model, cases)

  return(rbind(pred.3,pred.5,pred.7,pred.14))
}

getPoisPreds <- function(cases=FALSE){
  f.name <- here('data/model3_deaths.csv')
  if (cases){
    f.name <- here('data/model3_cases.csv')
  }
  if (!file.exists(f.name)){
    f.pred <- .glmPred
    f.model <- .getGlmModel
    preds <- .getPreds(.glmPred, .getGlmModel, cases)
    write_csv(preds, f.name)
  }
  preds <- read_csv(f.name)
  preds$predict_date <- as.Date(preds$predict_date)

  return(preds)
}

testPredsPois <- function(){
  model_pred <- getPoisPreds(cases=FALSE)
  pred_type <- 'cumdeathstot'
  sc <- performSanityCheck(model_pred, pred_type)
}




library(here)
library(testit)

.weightsOp <- function(true_pred_date.s, c, mu){
  # y and y_hat should inceasing in time, i,e y[1] is the most recent observation
  t <- length(true_pred_date.s$truth)
  scaled.diff <- abs(sqrt(true_pred_date.s$pred)-sqrt(true_pred_date.s$truth))
  factored.diff <- (mu^true_pred_date.s$time.factor) * scaled.diff
  w <- exp(-c*(1 - mu)*sum(factored.diff))
  return(w)
}

.dayCountyCLEP <- function(day, county.df, today=day, c, mu, noise){
  # calculates clep prdiction for a single day
  # if prediction and gt for past week not found returns na
  # day - the date, i.e "2020-03-29"
  # pred_truth_df - dataframe of the format:
  #   fipscounty pred  model       date truth
  #1        5001    1      1 2020-03-22  0.25
  # contains information for a single county
  # today - the day we make the prediction and the weights 
  # are calculated with respect to it
  #
  models <- unique(county.df$model)
  min.day <- min(unique(county.df$date))
  has.past <- today - min.day >= 7
  if (!has.past){
    w.vec <- rep(1/length(models), length(models))
    w.scaled <- w.vec/sum(w.vec)
  }
  if (has.past){
  past <- today + seq(-1, -7, length.out = 7)
  past.df <- county.df[county.df$date %in% past, ]
  w.vec <- c()
  for (m in models){
  true_pred_date <- past.df[past.df$model==m, ] %>% select('date', 'truth', 'pred')
  true_pred_date.s <- true_pred_date[order(true_pred_date$date), ]
  true_pred_date.s$time.factor <- 7-order(true_pred_date.s$date)
  if (noise){
    size <- nrow(true_pred_date.s)
    nz <- rpois(size, 3) *  sample(c(-1,1), size, replace = TRUE)
    true_pred_date.s$truth  <- true_pred_date.s$truth + nz
  }
  w.vec <- c(w.vec, .weightsOp(true_pred_date.s, c, mu))
  }
  w.scaled <- w.vec/sum(w.vec)}
  clep <- 0
  i=1
  pred_truth_df.day <- county.df[county.df$date==day, ]
  clep.data <- NA
  for (m in models){
    model.pred <- pred_truth_df.day$pred[pred_truth_df.day$model==m]
    model.w <-  w.scaled[i]
    clep <- clep + model.w * model.pred 
    model.data <- data.frame(matrix(c(model.pred, model.w), nrow = 1))
    colnames(model.data) <- c(paste(m,'pred', sep = "."), paste(m,'w', sep = "."))
    if (is.na(clep.data)){
      clep.data <- model.data    
      i = i+ 1
      next
    }
    clep.data <- cbind(clep.data, model.data)
    i = i+ 1
  }
  clep.data$clep <- clep
  return(clep.data)
}

performCLEP <- function(pred_model_df, truth, k, c, mu, noise=FALSE){
  # function to find final prediction by using CLEP to combine preds of different
  # models
  
  # pred_model_df: a df of all predictions make on day t for k days ahead from 
  # different models that need to combine
  # fipscounty   predict_date        pred      model
  # 5001         3/22/2020           4000      2 
  # truth: df of true cumulative cnts on date and county level
  # fipscounty   date         truth      
  # 5001         3/22/2020    4000      
  # return final prediction for cumulative counts on day t for k days ahead
  
  pred_model_df$date = as.Date(pred_model_df$predict_date)
  truth$date = as.Date(truth$date)
  # join prediction and truth values
  pred_truth_df <- pred_model_df %>%
    inner_join(truth, by = c('fipscounty',
                             'predict_date' = 'date'))
  
  m <- unique(pred_truth_df$model)[1]
  models <- unique(pred_model_df$model)
  names.col <- c()

  n.models <- length(models)
  dates <- pred_model_df$date[pred_model_df$model == m]
  counties <- pred_model_df$fipscounty[pred_model_df$model == m]
  all.counties <- unique(counties)
  clep.predictions <- data.frame(cbind(dates,counties))  
  colnames(clep.predictions) <- c('date','county')
  i<-3
  for (mdl in models){
    # R is a stupid language, this loop should be avoided
    pred.col <- paste(mdl,'pred', sep = ".")
    w.col <- paste(mdl,'w', sep = ".")
    clep.predictions$w.col <- NA
    clep.predictions$pred.col <- NA
    colnames(clep.predictions)[i:(i+1)] <- c(pred.col, w.col)
    i <- i+2
    
  }
  clep.predictions$date <- dates
  clep.predictions$clep <- NA
  n.cols.clep.p <- ncol(clep.predictions)
  for (cnty in all.counties){
    county.df <- pred_truth_df %>% filter(fipscounty==cnty)
    county.df.pred <- pred_model_df %>% filter(fipscounty==cnty)
    
    dates.c <- unique(county.df.pred$date)
  for (d in seq_along(dates.c)){
    today <- dates.c[d]-k +1
    day <- dates.c[d]
    # print(d)
    # print(day)
    clep.default.vec <- c()
    has.clep <- day %in% county.df$date
    if (!has.clep){
      w <- 1/length(models)
      
      clep.mean.val <- 0
      
    day.df <- county.df.pred[county.df.pred$predict_date==day,]
    for (mdl2 in models){
      pred.m <- day.df$pred[day.df$model==mdl2]
      clep.default.vec <- c(clep.default.vec, pred.m, w)
      clep.mean.val <- clep.mean.val + pred.m * w
      
    }
    clep.default.vec <- c(clep.default.vec, clep.mean.val)
    
    }
    
    if (has.clep){
      clep.default.vec <- .dayCountyCLEP(day, county.df, today, c, mu, noise)
    }
    
    
    clep.d <- clep.default.vec
    ind <- clep.predictions$date==dates.c[d] & clep.predictions$county == cnty
    clep.predictions[ind, c(3:n.cols.clep.p)] <- clep.d}
  }

  return(clep.predictions)
}

testCLEP <- function(){
  # this is a test case where the weights should be equal 
  # one prediction is predicts 1 another predicts 0 and the truth is sqrt(0.5)
  # we calculate clep only if there a week of past prediction see eq 3.7
  # k is the how many days forward we are predicting
  # the expected result is NA if date-k doesn't have a week long of 
  # history and o.w should be 0.5
  period.len  <- 16
  dates <-as.Date('03-22-2020', format = '%m-%d-%y') + 
    seq(0, period.len-1, length.out = period.len)
  fipscounty <- 5001
  pred1 <- rep(1, period.len)
  pred2 <- rep(0,period.len)
  model1.df <- data.frame(cbind(fipscounty,pred1, dates, 1))
  colnames(model1.df) <- c("fipscounty", "pred", "predict_date", "model")
  model1.df$predict_date <- dates
  
  model2.df <- data.frame(cbind(fipscounty,pred2, dates, 2))
  colnames(model2.df) <- c("fipscounty", "pred", "predict_date", "model")
  model2.df$predict_date <- dates
  pred_model_df.1 <- rbind(model1.df, model2.df)
  truth.1 <- model1.df  %>% select(-'model')
  colnames(truth.1) <- c('fipscounty',   "truth"         ,"date" )
  truth.1$truth = 0.5^2
  
  
  period.len  <- 16
  dates <-as.Date('03-22-2020', format = '%m-%d-%y') + 
    seq(0, period.len-1, length.out = period.len)
  fipscounty <- 5002
  pred1 <- rep(1, period.len)
  pred1[1] <-  0.5
  pred2 <- rep(0,period.len)
  model1.df <- data.frame(cbind(fipscounty,pred1, dates, 1))
  colnames(model1.df) <- c("fipscounty", "pred", "predict_date", "model")
  model1.df$predict_date <- dates
  
  model2.df <- data.frame(cbind(fipscounty,pred2, dates, 2))
  colnames(model2.df) <- c("fipscounty", "pred", "predict_date", "model")
  model2.df$predict_date <- dates
  pred_model_df <- rbind(model1.df, model2.df)
  truth <- model1.df  %>% select(-'model')
  colnames(truth) <- c('fipscounty',   "truth"         ,"date" )
  truth$truth = 0.5^2
  
  truth <- rbind(truth.1, truth)
  pred_model_df <- rbind(pred_model_df.1, pred_model_df)
  
  k=3
  c=1
  mu=0.5
  # set c = 1, mu = 0.5
  clep <- performCLEP(pred_model_df, truth, k, c, mu)
  clep.5001 <- clep %>% filter(county==5001)
  for (i in seq(period.len)){
    clp <- clep.5001$clep[i]
    if (i < 7+k){
      assert(is.na(clp))
    }
    if (i >= 7+k){
      assert(clp==0.5)
    }
    
  }
  # second case, the 1 label get one prediction right and has more 
  # weights in the first prediction

  k=3
  clep.5002 <- clep %>% filter(county==5002)
  for (i in seq(period.len)){
    clp <- clep.5002$clep[i]
    if (i < 7+k){
      assert(is.na(clp))
    }
    if (i==7+k){
      assert(clp>0.5)
    }
    if (i > 7+k){
      assert(clp==0.5)
    }
    
  }
}


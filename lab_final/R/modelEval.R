library(dplyr)
library(sqldf)
library(Rfast)

calcMEPI <- function(pred, truth, k){
  # function to calculate prediction intveral
  # pred: prediction table with FIPScounty, date - the final count for cumulative
  # count (t in y_t) by using all data up to t-k days
  # fipscounty predict_date   k   pred
  #  6003      3/22/2020      3   3500 ===> using all data from Jan/21/2020 to 
  #                                       3/19/2020 to predict cumulative counts
  #                                       for 3 days (3/20, 3/21, 3/22)
  # predict_date start from March 22 up until the latest date in the data
  # truth: true cumulative count 
  # fipscountycounty predict_date        truth
  #  6003      3/22/2020                  3200
  # return a df with prediction interval of cumulative count for k days ahead 
  # from t
  # y_t = E[deaths^c_t|t-k]
  # y_tk = E[deaths^c_(t+k)|t]

  pred$predict_date = as.Date(pred$predict_date)
  truth$date = as.Date(truth$date)
 
  # join prediction and truth table & calculate delta_t
  pred_truth_df <- pred %>% 
    inner_join(truth, by = c('fipscounty',
                             'predict_date' = 'date')) %>%
    rename(y_t = truth,
           y_hat_t = pred) %>%
    rowwise() %>%
    mutate(delta_t = abs(y_t/max(y_hat_t, 1) - 1))
  # calculate delta_max
  max_delta_t <- sqldf('select t.fipscounty
                        , t.predict_date
                        , tj.delta_t
                        from pred t
                        left join pred_truth_df tj
                            on t.fipscounty = tj.fipscounty
                            and tj.predict_date between t.predict_date - 4 and t.predict_date')

  # bring back truth column on the date
  max_delta_t <- max_delta_t %>% 
    group_by(fipscounty, predict_date) %>%
    summarise(max_delta_t = max(delta_t), .groups = 'keep') %>%
    inner_join(truth,  by = c('fipscounty',
                                   'predict_date' = 'date')) %>%
    rename(y_t = truth)
  pi_tk <- sqldf(sprintf('select p.fipscounty
                          , p.predict_date as tk__Date
                          , t.predict_date as t__Date 
                          , p.y_hat_t as y_hat_tk
                          , p.y_t as y_tk
                          , t.y_t
                          , t.max_delta_t
                          , max(p.y_hat_t*(1 - t.max_delta_t),t.y_t) as lower_tk
                          , p.y_hat_t*(1 + t.max_delta_t) as upper_tk
                          from pred_truth_df p
                          left join max_delta_t t
                              on p.fipscounty = t.fipscounty
                              and p.predict_date = t.predict_date + %d
                          where t.predict_date is not null', k),
                 method = 'name__class')
  return(pi_tk)
}

calcMetrics <- function(pred_int, true_value){
  # a function to calculate coverage and normalized length of prediction intervals
  # pred_int: df of min and max of prediction interval
  # fipscounty  predict_date(tk)        lower    upper
  # 6003         03/22/2020             3400     5000
  # true value: an array true cumulative counts of cases/deaths
  # fipscounty  predict_date(tk)       truth
  # 6003         03/22/2020             3800    
  # return a df of coverage and normalized length of prediction interval for 
  # each county
  
  pred_int$predict_date <- as.Date(pred_int$predict_date)
  true_value$date <- as.Date(true_value$date)
  metrics_df <- pred_int %>%
    inner_join(true_value, by = c('fipscounty',
                                  'predict_date' = 'date')) %>%
    mutate(if_cover = ifelse(truth >= lower & truth <= upper, 1, 0),
           normalized_len = (upper - lower)/truth) %>%
    group_by(fipscounty) %>%
    summarise(coverage = sum(if_cover, na.rm = TRUE)/n(),
              normalized_error = sum(normalized_len, na.rm = TRUE)/n(),
              .groups = 'keep')
  
  return(metrics_df)
  
}

calcEmpMetrics <- function(pred, truth){
  # a function to calculate MAPE, raw scale MAE, sqrt scale MAE 
  # pred: an df on county level of cumulative prediction over a period
  # (only counties with more than 10 cumulative counts)
  # fipscounty    predict_date    k    pred
  #  6003         3/22/2020       7    3000
  # truth: an df on county level of true cumulative prediction with same
  # condition as pred
  # fipscounty    predict_date        truth
  #  6003         3/22/2020           3500
  # return a df containa those metrics on date level with percentile
 
  pred_truth_df <- pred %>% 
    inner_join(truth, by = c('fipscounty', 
                             'predict_date' = 'date')) %>%
  # filter out counties with at least 10 cumulative cases 
  filter(truth >= 10) %>%
  mutate(rel_diff = abs(pred - truth)/truth,
         raw_diff = abs(pred - truth),
         sqrt_diff = abs(sqrt(pred) - sqrt(truth))) %>%
  group_by(predict_date, k) %>%
  summarise(MAPE = 100*1/n()*sum(rel_diff),
            rawMAE = 1/n()*sum(raw_diff),
            sqrtMAE = 1/n()*sum(sqrt_diff),
            .groups = 'keep')
 return(pred_truth_df) 
}

calcAllMetrics <- function(combine_df){
  # function calculates error metrics (normalized error, coverage) and 
  # empirical metrics (mape, rawMAE, sqrtMAE)
  # Args: combine_df in long format
  # fipscounty    date          k    model     pred
  # 6001          03-22-202     5   truth     3400
  # 6001          03-22-202     5   arima     3800
  # return a df of metrics values for all model

  colnames(combine_df) <- c('fipscounty', 'date', 'pred', 'model', 'k')
  k <- unique(combine_df$k)
  model_names <- unique(combine_df$model[
    combine_df$model != 'truth'])
  metrics_df <- data.frame()
  for(m in model_names){
    model_pred <- combine_df %>%
      filter(model == m) %>%
      rename(predict_date = date) %>%
      select(-model)
    truth_data <- combine_df %>%
      filter(model == 'truth') %>%
      rename(truth = pred) %>%
      select(-c(model, k))
    PI <- calcMEPI(model_pred, truth_data, k) %>%
      select(fipscounty, tk, lower_tk, upper_tk, y_tk) %>%
      rename(predict_date = tk,
             lower = lower_tk,
             upper = upper_tk)
    pi_metrics <- calcMetrics(PI, truth_data)
    emp_metrics <- calcEmpMetrics(model_pred, truth_data)
    binding_df <- data.frame(model = m,
                             coverage = median(pi_metrics$coverage),
                             normalized_error = median(pi_metrics$normalized_error),
                             mape = median(emp_metrics$MAPE),
                             rawmae = median(emp_metrics$rawMAE),
                             sqrtmape = median(emp_metrics$sqrtMAE))
    metrics_df <- rbind(metrics_df, binding_df)
    
  }
  return(metrics_df)
}


getRadarData <- function(metrics_df){
  # function to put metrics value to radar data format
  # Args:
  # metrics_df:
  # model     coverage      normalized_error      mape            rawmae
  # model3    0.7428571     0.02790099            1.5488229 1     19.43638
  # return a df in format to plot radar plot
  rownames(metrics_df) <- metrics_df$model
  radar_df <- metrics_df %>%
    select(-model)
  return(radar_df)
}


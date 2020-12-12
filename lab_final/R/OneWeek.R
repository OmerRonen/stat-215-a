library(stats)
library(forecast)

getOneWeekPred <- function(predict_time_period, full_data, k, pred_type){
  # function performing CLEP on ARIMA and MA
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return predictions for counties with k interval over a time period
  
  truth_name <- paste0('cum', pred_type, 'tot')
  arima_pred <- getARIMAPred(predict_time_period, full_data, k, pred_type)
  ma_pred <- getMovingAvgPred(predict_time_period, full_data, k, pred_type)
  # cases
  model_arima_ma_all <- arima_pred %>%
    mutate(model = 'arima',
           predict_date = as.Date(predict_date)) %>%
    select(-k) %>%
    rbind(ma_pred %>% ungroup() %>%
            mutate(model = 'ma',
                   predict_date = as.Date(predict_date)) %>%
            select(-k)) %>%
    left_join(county_fips,
              by = c('fipscounty')) %>%
    select(-countyname)
  
  truth_data_all <- full_data %>%
    filter(date %in% predict_time_period) %>%
    select(fipscounty, date, truth_name) %>%
    rename(truth = truth_name) %>%
    distinct() %>%
    left_join(county_fips,
              by = c('fipscounty')) %>%
    select(-countyname)
  
  clep_pred <- performCLEP(model_arima_ma_all,
                           truth_data_all, 5, 2, 0.5)
  return(clep_pred)
}

calcOneWeekMetrics <- function(fips, one_week_pred_long, pred_type_value, 
                               predict_period, m){
  # function to calculate performance of best model over 1 week
  # Args:
  # one_week_pred_long: df 
  # fipscounty     k     predict_date  value_type pred_type  value
  #  <int>       <dbl>     <date>       <chr>      <chr>      <dbl>
  #  6085           5    2020-11-23      pred       cases     30725.
  # fips: fipscounty
  # pred_type: cases or deaths
  # predict_period: sequence of predict date 11-28-2020 to 12-04-2020
  # m: model name
  
  # prepare data
  data <- one_week_pred_long %>%
    filter(fipscounty == fips,
           pred_type == pred_type_value) %>%
    mutate(model = ifelse(value_type == 'pred', m, 'truth')) %>%
    rename(date = predict_date) %>%
    select(fipscounty, date, value, model,k)
  # calculate metrics
  # not using calcAllMetrics because of the time range is not convenient
  # since MEPI need historical data outside of 11-28 while empircal metrics
  # dont need
  colnames(data) <- c('fipscounty', 'date', 'pred', 'model', 'k')
  k <- unique(data$k)
  model_names <- unique(data$model[data$model != 'truth'])
  metrics_df <- data.frame()
  model_pred <- data %>%
    filter(model == m) %>%
    rename(predict_date = date) %>%
    select(-model)
  truth_data <- data %>%
    filter(model == 'truth') %>%
    rename(truth = pred) %>%
    select(-c(model, k))
  PI <- calcMEPI(model_pred, truth_data, k) %>%
    select(fipscounty, tk, lower_tk, upper_tk, y_tk) %>%
    rename(predict_date = tk,
           lower = lower_tk,
           upper = upper_tk)
  pi_metrics <- calcMetrics(PI, truth_data)
  emp_metrics <- calcEmpMetrics(model_pred %>%
                                  filter(predict_date %in% predict_period), 
                                truth_data)
  binding_df <- data.frame(model = m,
                           coverage = median(pi_metrics$coverage),
                           normalized_error = median(pi_metrics$normalized_error),
                           mape = median(emp_metrics$MAPE),
                           rawmae = median(emp_metrics$rawMAE),
                           sqrtmape = median(emp_metrics$sqrtMAE),
                           fipscounty = fips)
  metrics_df <- rbind(metrics_df, binding_df)
  return(metrics_df)
}


calcOneWeekEmpMetrics_Day <- function(fips, one_week_pred_long, pred_type_value, 
                               predict_period, m){
  # Args:
  # one_week_pred_long: df 
  # fipscounty     k     predict_date  value_type pred_type  value
  #  <int>       <dbl>     <date>       <chr>      <chr>      <dbl>
  #  6085           5    2020-11-23      pred       cases     30725.
  # fips: fipscounty
  # pred_type: cases or deaths
  # predict_period: sequence of predict date 11-28-2020 to 12-04-2020
  # m: model name
  
  data <- one_week_pred_long %>%
    filter(fipscounty == fips,
           pred_type == pred_type_value) %>%
    mutate(model = ifelse(value_type == 'pred', m, 'truth')) %>%
    rename(date = predict_date) %>%
    select(fipscounty, date, value, model,k)
  # calculate metrics
  # not using calcAllMetrics because of the time range is not convenient
  # since MEPI need historical data outside of 11-28 while empircal metrics
  # dont need
  colnames(data) <- c('fipscounty', 'date', 'pred', 'model', 'k')
  k <- unique(data$k)
  model_names <- unique(data$model[data$model != 'truth'])
  metrics_df <- data.frame()
  model_pred <- data %>%
    filter(model == m) %>%
    rename(predict_date = date) %>%
    select(-model)
  truth_data <- data %>%
    filter(model == 'truth') %>%
    rename(truth = pred) %>%
    select(-c(model, k))
  emp_metrics <- calcEmpMetrics(model_pred %>%
                                  filter(predict_date %in% predict_period), 
                                truth_data)
  emp_metrics$fipscounty <- fips
  metrics_df <- rbind(metrics_df, emp_metrics)
  return(metrics_df)
}

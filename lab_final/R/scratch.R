library(here)
library(stats)
library(forecast)
library(dyplr)

source(here('R/Clep.R'))
source(here('R/ShrdCntyExp.R'))
source(here('R/linearModel.R'))
source(here('R/timeseries.R'))



.getARIMAFuturePred <- function(predict_time_period, full_data, k, pred_type){
  # function to perform ARIMA on cumulative counts for Dec 10 to Dec 16
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return point and interval predictions for counties with k interval over 
  # a time period
  # predict_time_period = one_week_period
  cnt_col <- paste0('cum', pred_type, 'tot')
  final_ts_pred_df <- data.frame()
  for(i in 1:length(predict_time_period)){
    ts_df <- full_data %>%
      select(fipscounty, date, cnt_col) %>%
      distinct() %>%
      rename(cnt = cnt_col) %>%
      filter(!is.na(cnt_col) &
               date <= (predict_time_period[i] - k)) %>%
      arrange(date)
    prediction <- lapply(unique(ts_df$fipscounty), FUN = function(x){
      if(nrow(ts_df[ts_df$fipscounty == x,'cnt']) == 1){
        # use neighbor data to make prediction
        pred_df <- full_data %>%
          filter(fipscounty %in% unique(full_data[full_data$fipscounty == x, 
                                                  'fipsneighbor']),
                 date <= (predict_time_period[i] - k)) %>%
          rename(cnt = cnt_col) %>%
          select(fipscounty, date, cnt) %>%
          summarise(pred = ifelse(is.na(mean(cnt, na.rm = TRUE)),
                                  0,mean(cnt, na.rm = TRUE)),
                    lower = ifelse(is.na(min(cnt, na.rm = TRUE)),
                                   0,min(cnt, na.rm = TRUE)),
                    upper = ifelse(is.na(max(cnt, na.rm = TRUE)),
                                   0,max(cnt, na.rm = TRUE)),
                    .groups = 'keep') %>%
          mutate(fipscount = x,
                 k = k) %>%
          select(fipscounty, pred, k)
      } else{
        # use ARIMA to make prediction
        cnt_ts <- ts_df[ts_df$fipscounty == x,'cnt']
        ts_model_pred <- forecast(auto.arima(cnt_ts), h = k)
        last_obs <- max(cnt_ts)
        pred <- max(as.numeric(ts_model_pred$mean[k]), last_obs)
        lower <- max(as.numeric(ts_model_pred$lower[k,1]), last_obs)
        upper <- max(as.numeric(ts_model_pred$upper[k,1]), last_obs)
        pred_df <- data.frame(fipscounty = x,
                              pred = pred,
                              lower = lower,
                              upper = upper,
                              k = k)
      }
      return(pred_df)
    })
    ts_pred_df <- do.call(rbind, prediction)
    ts_pred_df$predict_date <- predict_time_period[i]
    final_ts_pred_df <- rbind(final_ts_pred_df, ts_pred_df)
  }
  return(final_ts_pred_df)
}
.getMovingAvgFuturePred <- function(predict_time_period, full_data, k, pred_type){
  # function to perform Moving Average on rate of change of cumulative counts
  # to predict cumulative counts for Dec 10 to Dec 16
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return point and interval predictions for counties with k interval over 
  # a time period
  cnt_col <- paste0('cum', pred_type, 'tot')
  final_ts_pred_df = data.frame()
  for(i in 1:length(predict_time_period)){
    ts_df <- full_data %>%
      select(fipscounty, date, cnt_col) %>%
      distinct() %>%
      rename(cnt = cnt_col) %>%
      filter(!is.na(cnt_col) &
               date <= (predict_time_period[i] - k)) %>%
      arrange(date)
    prediction <- lapply(unique(ts_df$fipscounty), FUN = function(x){
      if(nrow(ts_df[ts_df$fipscounty == x,'cnt']) == 1){
        # use neighbor's data to make prediction
        pred_df <- full_data %>%
          filter(fipscounty %in% unique(full_data[full_data$fipscounty == x, 
                                                  'fipsneighbor']),
                 date <= (predict_time_period[i] - k)) %>%
          rename(cnt = cnt_col) %>%
          select(fipscounty, date, cnt) %>%
          summarise(pred = ifelse(is.na(mean(cnt, na.rm = TRUE)),
                                  0,mean(cnt, na.rm = TRUE)),
                    lower = ifelse(is.na(min(cnt, na.rm = TRUE)),
                                   0,min(cnt, na.rm = TRUE)),
                    upper = ifelse(is.na(max(cnt, na.rm = TRUE)),
                                   0,max(cnt, na.rm = TRUE)),
                    .groups = 'keep') %>%
          mutate(fipscount = x,
                 k = k) %>%
          select(fipscounty, pred, k)
      } else{
        # filter data to county of interest only
        county_df <- ts_df %>% 
          filter(fipscounty == x,
                 between(date, 
                         (predict_time_period[i] - k - 7),
                         (predict_time_period[i] - k))) %>%
          mutate(prev_date = date - 1)
        # calculate rate of change
        rate_of_change_df <- county_df %>%
          inner_join(county_df, by = c('fipscounty',
                                       'prev_date' = 'date'),
                     suffix = c('', '_prev')) %>%
          mutate(rate_of_change = ifelse(cnt_prev == 0, 0.25,
                                         (cnt - cnt_prev)/cnt_prev))
        # calculate rate of change
        rate_of_change <- mean(rate_of_change_df$rate_of_change, na.rm = TRUE)
        lower_rate_of_change <- quantile(rate_of_change_df$rate_of_change, 0.2)
        upper_rate_of_change <- quantile(rate_of_change_df$rate_of_change, 0.8)
        # get last observation
        last_obs <- max(county_df$cnt, na.rm = TRUE)
        # make predictions
        pred <- last_obs*(1 + rate_of_change)^k
        lower <- last_obs*(1 + lower_rate_of_change)^k
        upper <- last_obs*(1 + upper_rate_of_change)^k
        pred_df <- data.frame(fipscounty = x,
                              pred = pred,
                              lower = lower,
                              upper = upper,
                              k = k)
      }
      return(pred_df)
    })
    ts_pred_df <- do.call(rbind, prediction)
    ts_pred_df$predict_date <- predict_time_period[i]
    final_ts_pred_df <- rbind(final_ts_pred_df, ts_pred_df)
  }
  return(final_ts_pred_df)
}

getFuturePred_test <- function(){
  # function performing CLEP on ARIMA and MA
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return predictions for counties with k interval over a time period
  predict_time_period <- seq(as.Date('11-23-2020', format = '%m-%d-%y'),
                             as.Date('12-30-2020', format = '%m-%d-%y'), 1)
  k <- 5
  pred_type <- 'cases'
  truth_name <- paste0('cum', pred_type, 'tot')
  full_data <- getData()
  arima_pred <- .getARIMAFuturePred(predict_time_period, 
                                    full_data, k, pred_type)
  ma_pred <- .getMovingAvgFuturePred(predict_time_period, 
                                     full_data, k, pred_type)
  # cases
  model_arima_ma_all_pred <- arima_pred %>%
    select(fipscounty, predict_date, pred, k) %>%
    mutate(model = 'arima',
           predict_date = as.Date(predict_date)) %>%
    select(-k) %>%
    rbind(ma_pred %>% ungroup() %>%
            select(fipscounty, predict_date, pred, k) %>%
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
  return(list(model_arima_ma_all_pred,
              truth_data_all))
}

preds <- getFuturePred_test()
pred_model_df<-preds[[1]]
truth <- preds[[2]]
k <- 5
c <- 2
mu <- 0.5
clep_pred <- performCLEP(pred_model_df,
                         truth, k, c, mu)

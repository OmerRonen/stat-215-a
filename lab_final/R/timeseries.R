library(stats)
library(forecast)

getARIMAPred <- function(predict_time_period, full_data, k, pred_type){
  # function to perform ARIMA on cumulative counts
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return predictions for counties with k interval over a time period
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
        pred_df <- data.frame(fipscounty = x,
                              pred = pred,
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


getMovingAvgPred <- function(predict_time_period, full_data, k, 
                             pred_type){
  # function to perform Moving Average on rate of change of cumulative counts
  # to predict cumulative counts
  # Args:
  # predict_time_period: Eg. March 22 - June 20
  # full_data
  # k: k interval
  # pred_type: 'cases' or 'deaths'
  # return predictions for counties with k interval over a time period

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
        rate_of_change <- mean(rate_of_change_df$rate_of_change, na.rm = TRUE)
        last_obs <- max(county_df$cnt, na.rm = TRUE)
        pred <- last_obs*(1 + rate_of_change)^k
        pred_df <- data.frame(fipscounty = x,
                              pred = pred,
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





library(tidyr)
library(dplyr)

make_counts <- function(full_data, type, countyFIPS){
  # reorganize the data in cumulative counts df
  # to run linear model
  counts <- list()
  dates <- list()
  if (type == 'case'){
    flag = 'cumcasestot'
  }else{
    flag = 'cumdeathstot'
  }
  
  full_data <- full_data %>% 
    drop_na(flag) %>%
    select(fipscounty, date, cumdeaths4, cumcases4, cumcasestot, cumdeathstot) %>%
    distinct()
  
  for (county in countyFIPS){
    county_df <- full_data[full_data$fipscounty == county, ]
    county_df <- county_df[order(county_df$date), ]
    county_counts <- list(pull(county_df, flag))
    #county_counts <- as.vector(county_df$flag)
    #county_dates <- list(pull(county_df, 'date'))
    county_dates <- as.data.frame(county_df$date)
    counts[[length(counts) + 1]] <- county_counts
    dates[[length(dates) + 1]] <- county_dates
  }
  return (list(counts = counts, dates = dates))
}

linear_model <- function(data_obj, target_day, countyFIPS, full_data, flag){
  # Inputs:
  #   data_obj: list of lists
  #     storing the cumulative counts and dates 
  #   target_day: vector
  #     for each element d in the vector, predict cases/deaths d days from the last day in counts
  #   countyFIPS: vector
  #     unique county IDs
  #   full_data: df
  #     the processed original data
  #   flag: string
  #     indicating which column to look in full_data df
  # Output:
  #   pred : df
  #   predicted cases/deaths for all county, k days ahead as in target_day
  
  counts <- data_obj[['counts']]
  dates <- data_obj[['dates']]
  county_num <- length(counts)
  predicted_counts <- c()
  predicted_date_t <- data.frame()
  predicted_county <- c()
  for (i in c(1:county_num)){
    index <- i
    counts[[i]] <- c(unlist(counts[[i]]))

    train_ts <- counts[[i]]
    county <- countyFIPS[index]

    active_day <- 4
    ptr_end <- active_day
    
    while(ptr_end <= length(train_ts)){
      # use cumulative counts of t, t-1, t-2, t-3
      # to predict t+k
      ptr_start <- ptr_end - active_day + 1
      train_data <- train_ts[ptr_start:ptr_end]
      train_len <- length(train_data)
      
      base_t <- dates[[i]][ptr_start,]

      date_t <- dates[[i]][ptr_end,]

      predicted_date_t <- rbind(predicted_date_t, data.frame(date_t))
      
      predicted_county <- c(predicted_county, county)

      # run linear algorithm
      county_predict <- runLinearAlg(train_data, train_len, target_day, active_day)
      # make it predtot
      #sub_full_data <- full_data %>%
      #  filter(fipscounty == countyFIPS[index] & date == base_t)
      #county_predict <- county_predict + pull(sub_full_data[1, ], flag)
      
      predicted_counts <- rbind(predicted_counts, county_predict)
      
      ptr_end <- ptr_end + 1
    }
  }
  
  pred_df <- formatPred(predicted_counts, target_day, predicted_county, predicted_date_t)
  return (pred_df)
}

rateInterpolate <- function(train_ts, train_len, target_day){
  rate <- max(tail(train_ts, 1) - train_ts[train_len - 1], 0)
  rate_vec <- c()
  for (i in target_day){
    rate_vec <- c(rate_vec, rate * i)
  }
  
  return(rate_vec + tail(train_ts, 1))
}

runLinearAlg <- function(train_data, train_len, target_day, active_day){
  pred_num <- length(target_day)
  
  if (min(train_data) == max(train_data)){
    # corner case 1 : cases remain constant, unable to fit glm
    # -> sol: use previous day cases to predict
    county_predict <- rep(tail(train_data, 1), pred_num)
  }else if (min(diff(train_data)) == max(diff(train_data))){
    # corner case 2
    # -> sol: interpolate with rate
    county_predict <- rateInterpolate(train_data, train_len, target_day)
  }else{
    # run Gaussian glm
    x1 <- seq(0, active_day - 1)
    x2 <- rep(1, active_day)
    model.fit <- glm(train_data ~ x1 + x2,
                     family = gaussian)
    test_x1 <- target_day + active_day - 1
    test_x2 <- rep(1, pred_num)
    county_predict <- predict(model.fit,
                              data.frame(x1 = test_x1, x2 = test_x2))
    
    # if prediction out of bound
    # -> interpolate with rate
    if(any(county_predict < 0)){
      county_predict <- rateInterpolate(train_data, train_len, target_day)
    }
    
  }
  
  return(county_predict)
}

formatPred <- function(predicted_counts, target_day, predicted_county, predicted_dates){
  pred_df <- data.frame(predicted_counts)
  pred_df <- cbind(pred_df, data.frame(predicted_county))
  pred_df <- cbind(pred_df, data.frame(predicted_dates))
  
  new_cols <- c()
  for (t in target_day){
    new_cols <- c(new_cols, paste0('', t))
  }
  
  colnames(pred_df) <- c(new_cols, 'fipscounty', 'date')

  pred_df <- pred_df %>%
    pivot_longer(-c(fipscounty, date),
                 names_to = "k",
                 values_to = "pred") %>%
    group_by(k) %>%
    mutate(date = as.Date(date) + as.numeric(k)) %>%
    rename(predict_date = date)
  
  return(pred_df)
}

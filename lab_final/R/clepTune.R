library(here)
source(here('R/Clep.R'))

runClepTune <- function(data, truth, perm_hyparam){
  result <- data.frame()
  for(i in c(1:nrow(perm_hyparam))){
    clep_pred <- performCLEP(data,
                             truth,
                             5,
                             perm_hyparam[i, 'c'],
                             perm_hyparam[i, 'mu'])
    clep_pred <- clep_pred %>%
      cbind(hyparam = i)
    result <- result %>%
      rbind(clep_pred)
  }
  return(result)
}

joinTunePred <- function(clep_cases_pred, clep_deaths_pred){
  # reorganize the clep pred df
  clep_pred <- clep_cases_pred %>%
    inner_join(clep_deaths_pred, 
               by = c('county','date', 'hyparam'),
               suffix = c('.cases','.deaths')) %>%
    rename(fipscounty = county) %>%
    left_join(county_fips,
              by = c('fipscounty')) %>%
    select(-countyname)
  return(clep_pred)
}

formEvalDF <- function(clep_pred, full_data, set_num, predict_time_period){
  result <- data.frame()
  for(i in c(1:set_num)){
    subset <- clep_pred %>%
      filter(hyparam == i)
    result <- result %>%
      rbind(subset %>% 
              rename(predict_date = date,
                     pred.cases = clep.cases,
                     pred.deaths = clep.deaths) %>%
              filter(!is.na(pred.cases)) %>%
              filter(!is.na(pred.deaths)) %>%
              select(fipscounty, predict_date, pred.cases, pred.deaths, hyparam) %>%
              mutate(model = 'clep'))
  }
  
  result <- result %>%
    rbind(full_data %>%
            select(fipscounty, date, cumcasestot, cumdeathstot) %>%
            rename(predict_date = date,
                   pred.cases = cumcasestot,
                   pred.deaths = cumdeathstot) %>%
            filter(predict_date %in% predict_time_period) %>%
            distinct() %>%
            mutate(model = 'truth',
                   hyparam = 0)) %>%
    mutate(k = 5) %>%
    rename(date = predict_date)
  return(result)
}

runMetrics <- function(perm_hyparam, county_fips, clep_eval_df, flag){
  clep_tune_result <- data.frame()
  for(i in c(1:nrow(perm_hyparam))){
    clep_tune_metrics_df <- do.call(rbind,lapply(county_fips$fipscounty, function(x){
      data <- clep_eval_df %>%
        filter(fipscounty == x) %>%
        filter(hyparam %in% c(0, i)) %>%
        select(-c(flag, 'hyparam'))
        
      metrics <- calcAllMetrics(data) %>%
        mutate(fipscounty = x)
      return(metrics)
    }))
    
    clep_tune_metrics_df <- clep_tune_metrics_df %>%
      select(-model, -fipscounty) %>%
      colMeans() %>%
      t()
    
    clep_tune_result <- clep_tune_result %>%
      rbind(data.frame(clep_tune_metrics_df) %>%
              mutate(hyparam = i))
  }
  return(clep_tune_result)
} 

tuneCLEPOnce <- function(bay_model_arima_ma_cases,
                         bay_model_arima_ma_deaths,
                         south_model_arima_ma_cases,
                         south_model_arima_ma_deaths,
                         bay_truth_data_cases,
                         bay_truth_data_deaths,
                         south_truth_data_cases,
                         south_truth_data_deaths,
                         full_data,
                         predict_time_period,
                         county_fips,
                         c_list = c(1, 1.5, 2), 
                         mu_lst = c(0.5, 0.6, 0.7)){

  perm_hyparam <- expand.grid(c_lst, mu_lst)
  colnames(perm_hyparam) <- c('c', 'mu')
  
  # run clep for different hyperparamers settings
  # bay
  bay_clep_cases_pred <- runClepTune(bay_model_arima_ma_cases, 
                                     bay_truth_data_cases, perm_hyparam)
  bay_clep_deaths_pred <- runClepTune(bay_model_arima_ma_deaths, 
                                      bay_truth_data_deaths, perm_hyparam)
  # south
  south_clep_cases_pred <- runClepTune(south_model_arima_ma_cases, 
                                       south_truth_data_cases, perm_hyparam)
  south_clep_deaths_pred <- runClepTune(south_model_arima_ma_deaths, 
                                        south_truth_data_deaths, perm_hyparam)
  
  # combine cases and deaths into one full df
  bay_clep_pred <- joinTunePred(bay_clep_cases_pred, bay_clep_deaths_pred) 
  south_clep_pred <- joinTunePred(south_clep_cases_pred, south_clep_deaths_pred)
  
  # form eval df
  bay_clep_eval_df <- formEvalDF(bay_clep_pred, full_data, 
                                 nrow(perm_hyparam), predict_time_period)
  south_clep_eval_df <- formEvalDF(south_clep_pred, full_data, 
                                   nrow(perm_hyparam), predict_time_period)
  
  # identify bay and south countyfips
  bay_county_fips <- county_fips %>%
    filter(ifbaycounty == TRUE)
  south_county_fips <- county_fips %>%
    filter(ifbaycounty == FALSE)
  
  # run eval metrics
  # best hyperparam set for bay: 3
  # (c, mu) = (2, 0.5)
  bay_tune_metrics_result_cases <- runMetrics(perm_hyparam, bay_county_fips, 
                                              bay_clep_eval_df, 'pred.deaths')
  bay_tune_metrics_result_deaths <- runMetrics(perm_hyparam, bay_county_fips, 
                                               bay_clep_eval_df, 'pred.cases')
  # best hyperparam set for south: 3
  # (c, mu) = (2, 0.5)
  south_tune_metrics_result_cases <- runMetrics(perm_hyparam, south_county_fips, 
                                                south_clep_eval_df, 'pred.deaths')
  south_tune_metrics_result_deaths <- runMetrics(perm_hyparam, south_county_fips, 
                                                 south_clep_eval_df, 'pred.cases')
  # write eval results into csv
  write.csv(bay_tune_metrics_result_cases, 'other/bay_clep_tune_cases.csv')
  write.csv(bay_tune_metrics_result_deaths, 'other/bay_clep_tune_deaths.csv')
  write.csv(south_tune_metrics_result_cases, 'other/south_clep_tune_cases.csv')
  write.csv(south_tune_metrics_result_deaths, 'other/south_clep_tune_deaths.csv')
}

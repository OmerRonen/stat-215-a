library(here)

source(here('R/Clep.R'))
source(here('R/ShrdCntyExp.R'))
source(here('R/linearModel.R'))
source(here('R/timeseries.R'))

.getLinearPred <- function(cases){
  var.name <- 'cumdeathstot'
  countyFIPS <- unique(full_data$fipscounty)
  
  obj <- make_counts(full_data, 'death', countyFIPS)
  
  if (cases){
    var.name <- 'cumcasestot'
    obj <- make_counts(full_data, 'case', countyFIPS)
    
  }
  full_data <- getData()
  target_day <- c(1, 3, 5, 7, 14)
  #===== Run the linear model on full data =======#
  model2_pred <- linear_model(obj, target_day, countyFIPS, full_data, var.name)
  return(model2_pred)
  
}

.getTruth <- function(cases){
  var.name <- 'cumdeathstot'
  if (cases){
    var.name <- 'cumcasestot'
  }
  full_data <- getData()
  
  truth <- full_data %>%
    select(fipscounty, date, var.name) %>%
    rename(truth = var.name) %>%
    distinct()
  return(truth)
}

.getTruthDataCounty <- function(county, cases){
  truth <- .getTruth(cases)
  truth.c <- truth  %>% filter(fipscounty==county)
  return(truth.c)
  
}

.getClepDataCounty <- function(fipscounty, cases, k.val, mu, c, noise=FALSE){
  clep.data.fname <- here(paste('other/clep_deaths_k',k.val,'.csv', sep = ''))
  if (cases){
    clep.data.fname <-  here(paste('other/clep_cases_k',k.val,'.csv', sep = ''))
  }
  if (!file.exists(clep.data.fname)){
    model3_pred <- getPoisPreds(cases = cases) 
    model2_pred <- .getLinearPred(cases = cases)
    pred_model_df <- model3_pred %>% filter(k == k.val) %>%
      mutate(model = 'model3') %>%
      select(-k) %>%
      rbind(model2_pred %>% ungroup() %>%
              filter(k == k.val) %>%
              mutate(model = 'model2') %>%
              select(-k))
    
    truth <- .getTruth(cases)
    clep <- performCLEP(pred_model_df, truth, k.val, c, mu, noise)
    write_csv(clep, clep.data.fname)
    
  }
  clep <- read.csv(clep.data.fname)
  clep$date <- as.Date(clep$date, format="%Y-%m-%d")
  clep.c <- clep %>% filter(county==fipscounty)
  return(clep.c)
}

vizClepW <- function(fipscounty, cases, k.val, mu, c){
  
  y.lab <- ifelse(cases, 'Cases', 'Deaths')
  clep.c <- .getClepDataCounty(fipscounty, cases, k.val, mu, c)
  truth.c <- .getTruthDataCounty(fipscounty, cases)
  clep.plot.weight <- ggplot() +
    geom_line(data=clep.c,
              aes(as.Date(date), clep, color = model2.w), size=1.2) + 
    
    scale_colour_gradient(low='blue', high='red',limits=c(0,1), 
                          name='Linear Model\nClep Weight') +
    geom_line(data=truth.c, aes(x=date, y=truth), linetype = "dashed", alpha=0.7) +
    ylab(y.lab) + xlab("") + theme_minimal()
  
  return(clep.plot.weight)}

vizClepWUs <- function(fipscounty, cases, k.val, mu, c){
  
  y.lab <- ifelse(cases, 'Cases', 'Deaths')
  clep.c <- getClepDataCountyUs(fipscounty, cases, k.val, mu, c)
  truth.c <- .getTruthDataCounty(fipscounty, cases)
  weight <- colnames(clep.c)[4]
  model.name <- strsplit(weight, '.w')[[1]][1]
  clep.plot.weight <- ggplot() +
    geom_line(data=clep.c,
              aes(as.Date(date), clep, color = arima.w), size=1.2) + 
    
    scale_colour_gradient(low='blue', high='red', limits=c(0,1), 
                          name='Arima \nClep Weight') +
    geom_line(data=truth.c, aes(x=date, y=truth), 
              linetype = "dashed", alpha=0.7) +
    ylab(y.lab) + xlab("") + theme_minimal()
  
  return(clep.plot.weight)}

getClepDataCountyUs  <- function(fipscounty, cases, k.val, mu, c, noise=FALSE){
  full_data <- getData()
  county_fips <- full_data %>%
    select(fipscounty, countyname) %>%
    distinct()
  predict_time_period <- seq(as.Date('08-01-2020', format = '%m-%d-%y'),
                             as.Date('11-17-2020', format = '%m-%d-%y'), 1)
  arima_pred <- getARIMAPred(predict_time_period, full_data, 5, 'deaths')
  ma_pred <- getMovingAvgPred(predict_time_period, full_data, 5, 'deaths')
  
  if (cases){
    arima_pred <- getARIMAPred(predict_time_period, full_data, 5, 'cases')
    ma_pred <- getMovingAvgPred(predict_time_period, full_data, 5, 'cases')
    
  }

  pred_model_df <- arima_pred %>%
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
  var.name <- 'cumdeathstot'
  if (cases){
    var.name <- 'cumcasestot'
  }
  
  truth <- full_data %>%
    filter(date %in% predict_time_period) %>%
    select(fipscounty, date, var.name) %>%
    rename(truth = var.name) %>%
    distinct() %>%
    left_join(county_fips,
              by = c('fipscounty')) %>%
    select(-countyname)
  clep <- performCLEP(pred_model_df,
                                 truth, k.val, c, mu, noise=noise)
  clep.c <- clep %>% filter(county==fipscounty)
  return(clep.c)

}
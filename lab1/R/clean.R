# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!
source("R/load.R")
cleanDatesData <- function(date_df) {
  # a function to clean the redwood dates data
  # 
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
           # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}

.ymdHmsCoverter <- function(result_time){
  # converts result time format to ymd_hms
  time_str <- as.character(result_time)
  time_str <- substr(time_str, 1,19)
  time <- ymd_hms(time_str)
  return (time)
}

.convertToYmdHms <- function(redwood_df){
  # wrapper around `.ymdHmsCoverter`
  redwood_df$result_time <- .ymdHmsCoverter(redwood_df$result_time)
  return(redwood_df)
}


.removeVoltageOutliers <- function(redwood_df){
  # looking online normal voltage levels are 2-4 
  # (http://www-db.ics.uci.edu/pages/research/quasar/MPR-MIB%20Series%20User%20Manual%207430-0021-06_A.pdf)
  normal_voltage = redwood_df$voltage < 4 & redwood_df$voltage > 2
  redwood_df = redwood_df[!normal_voltage, ]
  #print(paste('Numebr of rows after voltage removal: ', nrow(redwood_df)))
  return(redwood_df)
  
}

.removeTemperature <- function(redwood_df){
  # we remove below zero temperatures since those are probably mistakes 
  # https://www.weather-us.com/en/california-usa/sonoma-climate?c,mm,mb,km#climate_text_12
  valid_tmp <- redwood_df$Temperature > 0 & redwood_df$Temperature < 50
  redwood_df = redwood_df[valid_tmp, ]
  #print(paste('Numebr of rows after temperature removal: ', nrow(redwood_df)))
  return(redwood_df)
} 
  
.removeHumid <- function(redwood_df){
  # we remove below zero temperatures since those are probably mistakes 
  # http://www.weather.gov.sg/climate-historical-extremes-humidity/
  valid_humid <- redwood_df$Humidity > 30 & redwood_df$Humidity < 100
  redwood_df = redwood_df[valid_humid, ]
  #print(paste('Numebr of rows after humid removal: ', nrow(redwood_df)))
  return(redwood_df)
} 

removeOutliers1 <- function(redwood_df){
  
  # redwood_df <- .removeHumid(redwood_df)
  redwood_df <- .removeTemperature(redwood_df)
  return (redwood_df)
  
}

# .removeDew <- function(redwood_df){
#   # we remove dew point above 40 as this does not obey our external validation 
#   valid_humid <- redwood_df$dew_point < 40
#   redwood_df = redwood_df[valid_humid, ]
#   #print(paste('Numebr of rows after humid removal: ', nrow(redwood_df)))
#   return(redwood_df)
# } 
# 
# 
# removeOutliers2 <- function(redwood_df){
#   
#   redwood_df <- .removeDew(redwood_df)
#   return (redwood_df)
#   
# }

.removeNonExistingNodes <- function(redwood_df){
  # exclude invalid nodes
  all_nodes = loadMoteLocationData(path = "~/Documents/Phd/215A/stat-215-a/lab1/data/")$ID
  is_valid = redwood_df$nodeid < max(all_nodes)
  redwood_df = redwood_df[is_valid,]
  #print(paste('Numebr of rows after nodes removal: ', nrow(redwood_df)))
  return(redwood_df)
  
}


.removeNas <- function(redwood_df){
  redwood_df <- redwood_df[complete.cases(redwood_df), ]
  #print(paste('Numebr of rows aftel nas removal:', nrow(redwood_df)))
  return(redwood_df)
}

.removeDate <- function(redwood_df){
  
  valid = redwood_df$result_time!=ymd_hms('2004-11-10 14:25:00')
  redwood_df = redwood_df[valid, ]
  #print(paste('Numebr of rows aftel date removal:', nrow(redwood_df)))
  return (redwood_df)
  
}

cleanRedwoodData <- function(redwood_df) {
  # convert result_time to lubridate ymd_hms format
  redwood_df <- .convertToYmdHms(redwood_df)
  # do anything else you feel is necessary
  redwood_df <- .removeNonExistingNodes(redwood_df)
  redwood_df <- .removeNas(redwood_df)
  #redwood_df <- .removeDate(redwood_df)
  #redwood_df <- .removeTmp(redwood_df)
  
  # setting nicer columns names
  colnames(redwood_df) = c('node_id','result_time', 'epoch','parent', 'voltage' ,'depth', 'Humidity', 'Temperature', 
                           'humid_adj', 'Incident_PAR', 'Reflected_PAR', 'Height', 'Tree')

  return(redwood_df)
}

addSensorsInfo <- function(redwood_df, coords_df){
  info = coords_df[, c('ID', 'Height', 'Tree')]
  # changing to match with redwood for merge
  colnames(info) = c('nodeid', 'Height', 'Tree')
  redwood_df_info = merge(x = redwood_df, y = info, by = "nodeid", all.x = TRUE)
  return (redwood_df_info)

}

normPAR <- function(redwood_df){
  # it seems that the units are lux according to various google searches we need to divide by 54
  # ref: https://www.apogeeinstruments.com/conversion-ppfd-to-lux/
  redwood_df$Incident_PAR =  redwood_df$Incident_PAR/54
  redwood_df$Reflected_PAR =  redwood_df$Reflected_PAR/54
  return(redwood_df)
  
}

mergeDates <- function(redwood_df, dates_df){
  # changing to match with redwood for merge
  colnames(dates_df)[5] = 'result_time'
  colnames(dates_df)[1] = 'epoch'

  redwood_merged <- merge(x = redwood_df, y = dates_df[, c('epoch', 'result_time')], by = "epoch", all.x = TRUE)
  time_diff = redwood_merged$`result_time.y` - redwood_merged$`result_time.x`
  redwood_merged$`result_time` = redwood_merged$`result_time.y` #+ max(time_diff)
  redwood_df = redwood_merged %>% select(-c('result_time.y','result_time.x'))
  # removing duplicates
  # assuming that high voltage are mistakes
  redwood_df_sorted_v = redwood_df[order(redwood_df$voltage), ]
  redwood_df = redwood_df_sorted_v[!duplicated(select(redwood_df, c(result_time, epoch, node_id, Temperature, Humidity))), ]
  # restricting ourself to the interior tree
  redwood_df = redwood_df[redwood_df$Tree=='interior',]
  
  
  return (redwood_df)

}


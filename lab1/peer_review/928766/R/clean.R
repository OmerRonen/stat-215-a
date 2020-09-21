# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

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


cleanRedwoodData <- function(redwood_df) {
  # convert result_time to lubridate ymd_hms format
  # do anything else you feel is necessary
  redwood_df <- inner_join(redwood_df, redwood_df, by=colnames(redwood_df)[-c(1, 5)]) %>% 
                  drop_na() %>%
                  filter(result_time.y != result_time.x) %>% 
                  mutate(result_time = strptime(result_time.x, "%Y-%m-%d %H:%M:%S")) %>%
                  filter(result_time > "2004-04-27 05:05:00") %>%
                  filter(result_time < "2004-06-11") %>%
                  mutate(voltage = voltage.y) %>%
                  select(-c(voltage.x, voltage.y, result_time.x, result_time.y)) %>%
                  filter(voltage < 190) %>%
                  distinct() %>%
                  mutate(epoch = as.factor(epoch))
  
  return(redwood_df)
}

cleanRedwoodDataStability <- function(redwood_df) {
  redwood_df <- redwood_df %>% mutate(result_time = strptime(result_time, "%Y-%m-%d %H:%M:%S")) %>%
                  drop_na() %>% distinct() %>% filter(result_time < "2004-06-11")
  return(redwood_df)
}


#####80characters###############################################################

symdiff <- function(x, y) { setdiff( union(x, y), intersect(x, y))}

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
    dplyr::mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    dplyr::select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}


tidyRedwoodData <- function(redwood_df, epoch_df, motes_df) {
  
  # convert result_time to lubridate ymd_hms format
  redwood_df <- redwood_df %>% 
    dplyr::mutate(result_time = lubridate::ymd_hms(result_time))
  # do anything else you feel is necessary
  # ...
  # got it, James...
  
  # rename columns
  redwood_df <- redwood_df %>% 
    dplyr::rename(rel_humidity = humidity, temp_celcius = humid_temp, 
                  PAR_incid_lux = hamatop, PAR_refl_lux = hamabot, 
                  node_id = nodeid) %>%
  # coerce to right datatypes
    dplyr::mutate(node_id = as.factor(node_id)) %>%
  # change units
    dplyr::mutate(PAR_incid_e = PAR_incid_lux/54, 
                  PAR_refl_e = PAR_refl_lux/54) %>%
  # combine with other dfs
    dplyr::full_join(epoch_df, by = "epoch") %>%
    dplyr::full_join(motes_df, by = "node_id") %>%
    arrange(epoch)
    return(redwood_df)
  
}


cleanMoteLocationData <- function(motes_df) {
  
  compass_df = data.frame(compass_direc = 
                            as.factor(c("E", "ESE", "NE", "NW", "S", "SW", "W",
                              "WNW", "WSW")), 
                          angle_direc = c(0, 337.5, 45, 135, 270, 225, 180, 
                                          157.5, 202.5))
  motes_df <- motes_df %>%
    # rename more sensibly
    dplyr::rename(node_id = ID, height_meters = Height, 
                  dist_meters = Dist, compass_direc = Direc, 
                  tree_id = Tree) %>%
    # change tree_id to be more interpretable
    dplyr::mutate(tree_id = 
                    case_when( 
                    tree_id == "interior" ~ "TreeTolle05",
                    tree_id == "edge" ~ "TreeOther"
                    )
                  ) %>% 
    dplyr::mutate(node_id = as.factor(node_id),
                  tree_id = as.factor(tree_id)) %>%
    dplyr::full_join(compass_df, by = "compass_direc") 
  return(motes_df)
}



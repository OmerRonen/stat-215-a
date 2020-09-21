#####80characters###############################################################


plot_all <- function(df){
  
  plt1 = ggplot(dups_all, aes(x = node_id, y = voltage)) + geom_boxplot()
  plt2 = ggplot(dups_all, aes(x = node_id, y = temp_celcius)) + geom_boxplot()
  plt3 = ggplot(dups_all, aes(x = node_id, y = rel_humidity)) + geom_boxplot()
  plt4 = ggplot(dups_all, aes(x = node_id, y = PAR_incid_e)) + geom_boxplot()
  plt5 = ggplot(dups_all, aes(x = node_id, y = PAR_refl_e)) + geom_boxplot()
  
  for(p in list(plt1, plt2, plt3, plt4, plt5)){
    print(p)
  }
}

check_outliers <- function(df){
  
  check_humid <- function(df){
    # based on percents
    mutate(df, humid_ok = (rel_humidity <= 100) & (rel_humidity >= 0))
  }
  check_temp <- function(df){
    # based on earth temperatures
    mutate(df, temp_ok = (temp_celcius <= 44) & (temp_celcius >= -10))
  }
  check_PAR_incid <- function(df){
    # based on very obvious break in the boxplot at 2500
    # also, direct sunlight ~ 2000
    mutate(df, incid_ok = (PAR_incid_e <= 2500) & (PAR_incid_e >= 0))
  }
  check_PAR_refl <- function(df){
    # no obvious breaks, but almost all values are below 200
    mutate(df, refl_ok = (PAR_refl_e <= 200) & (PAR_refl_e >= 0))
  }
  check_volts <- function(df){
    # based on micadot spec sheet
    mutate(df, volt_ok = (voltage <= 5) & (voltage >= 0))
  }
  check_tolle_volts <- function(df){
    # based on Tolle et al
    mutate(df, tolle_ok = (voltage <= 3) & (voltage >= 2.4))
  }
  
  check_batt_low <- function(df){
    # based on Tolle et al
    mutate(df, batt_low = (voltage <= 2.4))
  }
  
  
  df = check_humid(df)
  df = check_temp(df)
  df = check_PAR_incid(df)
  df = check_PAR_refl(df)
  df = check_volts(df)
  df = check_tolle_volts(df)
  df = check_batt_low(df)
  df = mutate(df, num_bad = 
                as.integer(!humid_ok) + 
                as.integer(!temp_ok) + 
                as.integer(!incid_ok) + 
                as.integer(!refl_ok))
  return(df)
}

plot_outliers <- function(df){
}

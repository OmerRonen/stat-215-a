#####80characters###############################################################
library(dplyr)
library(here)
library(rlang)
source(here('R/explore.R'))

remove_sensor_nas <- function(df){
  # remove when all four are NA
  df = filter(df, !(is.na(rel_humidity) & is.na(temp_celcius) & 
                    is.na(PAR_incid_e) & is.na(PAR_refl_e))   
              )
  return(df)
}

push_epochs <- function(df){
  # this function assumes that there are exactly two duplicates left for all the epoch/node_id pairs
  df$key = 1:nrow(df)
  remaining_dups = df %>% group_by(epoch, node_id) %>% filter(n() == 2) %>% arrange(epoch, node_id)
  failed_epochs = c()
  failed_node_ids = c()
  for(i in 1:(nrow(remaining_dups)/2)){
    dup_row = remaining_dups[i*2,]
    current_node = dup_row$node_id
    missing_epoch = dup_row$epoch + 1
    missing_key = dup_row$key
    df_check = filter(df, epoch == missing_epoch, node_id == current_node)
    n = nrow(df_check)
    if(n == 0){
      df[missing_key,]$epoch = missing_epoch
    }
    else{
      failed_epochs = c(failed_epochs, dup_row$epoch)
      failed_node_ids = c(failed_node_ids, dup_row$node_id)
    }
  }
  if(length(failed_epochs) > 0){
    msg = paste("Tried to push node", failed_node_ids, "from epoch", failed_epochs, "to its next epoch, but the next epoch had values")
    print(msg)
  }
  df$key = NULL
  return(df)
}


remove_true_dups <- function(df){
  # set a key
  df$key = 1:nrow(df)
  # sort by voltage
  df = arrange(df, voltage)
  # save the key
  key = df$key
  # erase the key
  df$key = NULL
  # remove duplicates
  if("imputed_voltage" %in% names(df)){
    trusted = select(df,!c(result_time, voltage, data_source, imputed_voltage))
  }
  else{
    trusted = select(df,!c(result_time, voltage, data_source))
  }
  dup_inds = duplicated(trusted)
  df = df[!dup_inds, ]
  key = key[!dup_inds]
  # sort by the key again
  df$key = key
  df = arrange(df, key)
  # remove the key
  df$key = NULL
  return(df)
}

remove_future_values <- function(df, column, node, date){
  col_quo = enquo(column)
  # pull is the easiest way to extract columns from both tibbles and dataframes
  col = df %>% pull(!!col_quo) 
  col[1] = NA
  na = col[1]
  # now na is of the right data_type, and case_when won't break
  df = mutate(df, !!col_quo := 
                case_when(
                  (node_id == node) & (date < epoch_times) ~ na, 
                  TRUE ~ !!col_quo
                )
              )
  return(df)
}

impute_voltages <- function(df){
  low_v = remove_true_dups(filter(df, voltage < 100)) %>% filter(node_id != 29, rel_humidity > - 1000)
  high_v = remove_true_dups(filter(df, voltage >= 100)) %>% filter(node_id != 29, rel_humidity > - 1000) %>% rename(high_volt = voltage)
  
  together_v = inner_join(low_v, high_v, by = c('epoch','node_id')) %>% select(epoch, node_id, voltage, high_volt) %>% filter(high_volt < 1020) #because these values are maxed out
  
  lm_volt = lm(voltage ~ high_volt, data = together_v)
  b0 = coef(lm_volt)[1]
  b1 = coef(lm_volt)[2]
  
  df = mutate(df, imputed_voltage = case_when(
    voltage >= 1020 ~ NA_real_,
    (voltage < 1020) & (voltage > 100) ~ b0 + b1 * voltage, 
    TRUE ~ voltage
  ))
  
}




###########################################
# Nodes to remove data for + columns + times
#########################################

# if humid/temp have different times, take the earliest

# node 78
# 2004-5-08 12:00 humidity
# 2004-5-07 23:00 temp

# node 145
# 2004-5-09 11:00 humidity
# 2004-5-08 17:00 temp

# node 123
# 2004-5-14 18:00 humidity
# 2004-5-14 18:00 temp

# node 141
# 2004-5-28 19:00 humidity
# 2004-5-28 22:00 temp

# node 3
# 2004-5-10 00:00 humidity
# 2004-5-09 21:00 temp

# node 59
# humidity ok
# 2004-5-18 03:00 temp

# node 29
# remove all

# node 107
# 2004-5-05 18:00 temp

# node 64
# 2004-5-06 7:00 temp

# node 40
# remove all par_incid



make_judgment_calls <- function(df){
  df = remove_sensor_nas(df)
  # now remove outliers
  df = filter(df, node_id != 29)
  df = filter(df, rel_humidity > -1000)
  df = impute_voltages(df)
  df = remove_future_values(df, rel_humidity, 78, ymd_h("2004-5-07 23"))
  df = remove_future_values(df, temp_celcius, 78, ymd_h("2004-5-07 23"))
  df = remove_future_values(df, rel_humidity, 145, ymd_h("2004-5-08 17"))
  df = remove_future_values(df, temp_celcius, 145, ymd_h("2004-5-08 17"))
  df = remove_future_values(df, rel_humidity, 123, ymd_h("2004-5-14 18"))
  df = remove_future_values(df, temp_celcius, 123, ymd_h("2004-5-14 18"))
  df = remove_future_values(df, rel_humidity, 141, ymd_h("2004-5-28 19"))
  df = remove_future_values(df, temp_celcius, 141, ymd_h("2004-5-28 19"))
  df = remove_future_values(df, rel_humidity, 3, ymd_h("2004-5-09 21"))
  df = remove_future_values(df, temp_celcius, 3, ymd_h("2004-5-09 21"))
  df = remove_future_values(df, rel_humidity, 59, ymd_h("2004-5-18 03"))
  df = remove_future_values(df, temp_celcius, 59, ymd_h("2004-5-18 03"))
  df = remove_future_values(df, rel_humidity, 107, ymd_h("2004-5-05 18"))
  df = remove_future_values(df, temp_celcius, 107, ymd_h("2004-5-05 18"))
  df = remove_future_values(df, rel_humidity, 64, ymd_h("2004-5-06 7"))
  df = remove_future_values(df, temp_celcius, 64, ymd_h("2004-5-06 7"))
  df = remove_future_values(df, PAR_incid_e, 40, ymd_h("2004-1-1 12"))
  df = mutate(df, rel_humidity = 
                case_when(
                  rel_humidity > 100 ~ 99.99,
                  TRUE ~ rel_humidity
                )
              )
  df = remove_true_dups(df)
  df = push_epochs(df)
  return(df)
}
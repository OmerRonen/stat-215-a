timeDiffEpoch <- function(redwood_df) {
  # A function to manipulate the data to return the max difference in time
  # of recording within the same epoch
  #
  # Arguments: 
  #   redwood_df: data frame of the standard form from loading the data sets
  # Returns:
  #   a data frame object which contains the max difference in time within an epoch
  
  times <- redwood_df %>% mutate(result_time = strptime(result_time, "%Y-%m-%d %H:%M:%S")) %>%
                          filter(result_time < "2004-06-11") %>%
                          select(epoch, result_time)
  
  times_diff <- times %>% drop_na() %>% 
    group_by(epoch) %>%
    mutate(max_result_time = max(result_time), min_result_time = min(result_time)) %>%
    arrange(epoch) %>%
    mutate(gap = max_result_time - min_result_time)
  
  return(times_diff)
}

computeCombined <- function(redwood_df, motes) {
  # A helper function to compute the combined dataframe, used in plotting for the findings 
  # 
  # Arguments: 
  #   redwood_df: data frame of the standard form from loading the data sets
  #   motes: data frame of the different remote sensors
  
  low_motes <- motes %>% filter(Height < 30)
  high_motes <- motes %>% filter(Height > 60)
  
  low_redwood_df <- redwood_df %>% filter(nodeid %in% low_motes$ID)
  high_redwood_df <- redwood_df %>% filter(nodeid %in% high_motes$ID)
  
  low_red_2 <- low_redwood_df %>% group_by(epoch) %>% summarize(avg_humidity = mean(humidity), avg_top = mean(hamatop), avg_bot = mean(hamabot))
  high_red_2 <- high_redwood_df %>% group_by(epoch) %>% summarize(avg_humidity = mean(humidity), avg_top = mean(hamatop), avg_bot = mean(hamabot))
  
  combined <- inner_join(low_red_2, high_red_2, by = "epoch")
  
  return(combined)
}

finding_1 <- function(combined) {
  # A function that returns the ggplot object for the second finding, which is a
  # that the variation in the sunlight ratio at the bottom of the tree of the top photodiode
  # is far less than that of the variation of the sunlight at the top
  # 
  # Arguments: 
  #   combined: a dataframe computed from the function computeCombined
  
  plot1 <- ggplot(data = combined %>% filter(avg_top.x > 0), aes(x = log((1 + avg_top.x)/(1 + avg_bot.x)))) + 
    geom_density(color = "green") + theme_bw() + ylim(0, 0.7) + xlim(-8, 12) +
    labs(y = "Density", title = "Canopy Layer", x = NULL)
  
  plot2 <- ggplot(data = combined %>% filter(avg_bot.y > 0), aes(x = log((1 + avg_top.y)/(1 + avg_bot.y)))) + 
    geom_density(color = "blue")+ theme_bw() + ylim(0, 0.7) + xlim(-8, 12) +
    labs(y = "Density", title = "Peak Layer", x = NULL)
  
  ggarrange(plot1, plot2, ncol = 2, labels = "AUTO") %>% 
    annotate_figure(bottom = text_grob("log(Top Diode + 1)/(Bottom Diode + 1)"),
                    top = text_grob("Probability Density Estimate of Sunlight at Differing Layers"))
}

finding_2 <- function(combined) {
  # A function that returns the ggplot object for the second finding, which is a
  # negative relationship between the log of the average solar radiation at the
  # bottom few motes and the difference between the humidity at the bottom motes
  # and the top motes
  #
  # Arguments: 
  #   combined: a dataframe computed from the function computeCombined
  
  plot1 <- ggplot(data = combined %>% filter(avg_top.x > 0) %>% sample_n(500), aes(x = log(1 + avg_top.x), y = avg_humidity.x - avg_humidity.y)) +
    geom_point(color = "green", size = 0.5) + theme_bw() + 
    geom_smooth(method='lm', formula = y ~ x, color = "red", alpha = 0.5) + ylim(-10, 30) + 
    labs(x = "log(Sunlight at Canopy + 1)", y = NULL)
  
  plot2 <- ggplot(data = combined %>% filter(avg_top.y > 0)  %>% sample_n(500), aes(x = log(1 + avg_top.y), y = avg_humidity.x - avg_humidity.y)) +
    geom_point(color = "blue", size = 0.5) + theme_bw() + 
    geom_smooth(method='lm', formula = y ~ x, color = "red", alpha = 0.5) + ylim(-10, 30) + 
    labs(x = "log(Sunlight at Peak + 1)", y = NULL)
  
  plot3 <- ggplot(data = combined %>% sample_n(500), aes(x = log(1 + avg_top.x), y = avg_humidity.x - avg_humidity.y)) +
    geom_point(color = "green", size = 0.5) + theme_bw() + ylim(-10, 30) +
    labs(x = "log(Sunlight at Canopy + 1)", y = NULL)
  
  plot4 <- ggplot(data = combined %>% sample_n(500), aes(x = log(1 + avg_top.y), y = avg_humidity.x - avg_humidity.y)) +
    geom_point(color = "blue", size = 0.5) + theme_bw() + ylim(-10, 30) + 
    labs(x = "log(Sunlight at Peak + 1)", y = NULL)
  
  ggarrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2, labels = "AUTO") %>% 
    annotate_figure(left = "Humidity at Canopy - Humidity at Peak", top = "Elevation Humidity Variations and Sunlight")
}

finding_3 <- function(redwood_df, motes) {
  # A function that plots for the third finding, a relationship 
  # between the gap of the humidity of the North side and the South side
  # and the average amount of sunlight radiation the tree receives
  
  north_motes <- motes %>% filter(str_detect(Direc, "N"))
  south_motes <- motes %>% filter(Direc == "S")
  
  north_redwood_df <- clean_redwood_df %>% filter(nodeid %in% north_motes$ID)
  south_redwood_df <- clean_redwood_df %>% filter(nodeid %in% south_motes$ID)
  
  north_red_2 <- north_redwood_df %>% group_by(epoch) %>% 
                    summarize(avg_humidity = mean(humidity), avg_top = mean(hamatop), avg_bot = mean(hamabot))
  south_red_2 <- south_redwood_df %>% group_by(epoch) %>% 
                    summarize(avg_humidity = mean(humidity), avg_top = mean(hamatop), avg_bot = mean(hamabot))
  all_red_2 <- clean_redwood_df %>% group_by(epoch) %>% 
                    summarize(avg_humidity = mean(humidity), avg_top = mean(hamatop), avg_bot = mean(hamabot))
  
  combined2 <- inner_join(north_red_2, south_red_2, by = "epoch") %>% 
    select(humid_north = avg_humidity.x, humid_south = avg_humidity.y, epoch = epoch) %>%
    inner_join(all_red_2, by = "epoch")
  
  plot1 <- ggplot(data = combined2 %>% sample_n(250), aes(x = log(1 + avg_top), y = humid_north - humid_south)) + 
              geom_point(color = "skyblue") + theme_bw() + 
              labs(x = "log(Sunlight + 1)", y = "Humidity North - Humidity South", 
                   title = "Cardinal Humidity Variations and Sunlight")
  plot1
  
}
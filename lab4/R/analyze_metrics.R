
plotROC_PRcurve <- function(score_obs){
  # function to plot ROC and PR curve and AUC
  # score_obs: dataframe with truth, and cloud prediction prob coloumns 
  #   truth model_type    cloud_prob
  #   <fct> <chr>           <dbl>
  # 1 cloud aliyah          0.944
  # 2 cloud huong           0.598
  # return a list of ROC, PR plots and data frame of pr_auc, roc_auc 
  # for all models
  
  # obtain roc df
  roc_df <- score_obs %>%
    group_by(model_type) %>%
    roc_curve(truth, cloud_prob)
  # calculate ROC AUC
  roc_auc <- score_obs %>%
    group_by(model_type) %>%
    roc_auc(truth, cloud_prob) %>%
    select(.metric, .estimate, model_type)
  # plotting
  roc_plotting <- roc_df %>%
    arrange(model_type, .threshold) %>% 
    ggplot() +
    geom_path(aes(1- specificity, sensitivity, color = model_type)) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") + 
    labs(title = 'ROC Curve', color = 'Model Type') +
    theme_bw() +
    coord_equal()
  
  # obtain pr df
  pr_df <- score_obs %>%
    group_by(model_type) %>%
    pr_curve(truth, cloud_prob) 
  # calculate PR AUC
  pr_auc <- score_obs %>%
    group_by(model_type) %>%
    pr_auc(truth, cloud_prob) %>%
    select(.metric, .estimate, model_type)
  # plotting
  pr_plotting <- pr_df %>%
    arrange(model_type, .threshold) %>% 
    ggplot() +
    geom_path(aes(recall, precision, color = model_type)) + 
    labs(title = 'PR Curve', color = 'Model Type') +
    theme_bw() +
    coord_equal()
  
  # combine roc_auc and pr_auc into a df
  roc_pr <- rbind(roc_auc, pr_auc)
  colnames(roc_pr) <- c('metrics','values','model_type')
  
  return(list(roc_plot = roc_plotting, 
              pr_plot = pr_plotting, 
              roc_pr_df = roc_pr))
}



calculateMetrics <- function(thres = 0.5, score_obs, roc_pr_df){
  # function to create a df for metrics: precision, recall, balanced_accuracy,
  # F_value, roc_auc, pr_auc
  # thres - threshold to round predicted cloud prob, score_obs - long df of truth
  # and predicted prob, roc_pr_df - long df of roc-auc and pr-auc of different
  # model
  # retun a long df containing all metrics for all models
  
  metrics_df <- score_obs %>% 
    # do all the pre calculation
    mutate(prediction = ifelse(cloud_prob >= thres, 'cloud', 'no_cloud'),
           true_pos = ifelse(truth == 'cloud' & prediction == 'cloud',1,0),
           false_neg = ifelse(truth == 'cloud' & prediction == 'no_cloud',1,0),
           false_pos = ifelse(truth == 'no_cloud' & prediction == 'cloud',1,0),
           true_neg = ifelse(truth == 'no_cloud' & prediction == 'no_cloud',1,0)) %>%
    group_by(model_type) %>%
    # calculate main metrics
    summarise(precision = sum(true_pos)/(sum(true_pos) + sum(false_pos)),
              recall = sum(true_pos)/(sum(true_pos) + sum(false_neg)),
              specificity = sum(true_neg)/(sum(true_neg) + sum(false_pos))) %>%
    # calculate more metrics
    # using balanced accuracy since the data is imbalanced
    mutate(balanced_accuracy = (recall + specificity)/2,
           F = 2*precision*recall/(precision + recall)) %>%
    select(-specificity) %>%
    pivot_longer(-model_type, names_to = 'metrics', values_to = 'values') %>%
    select(metrics, values, model_type)
  # combine with roc auc and pr auc
  metrics_df <- rbind(metrics_df, roc_pr_df) %>%
    arrange(metrics, values)
  return(metrics_df)
}
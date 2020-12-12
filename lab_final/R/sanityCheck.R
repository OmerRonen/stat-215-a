source('R/cleanData.R')
source('R/modelEval.R')
performSanityCheck <- function(model_pred, pred_type, ks, counties){
  # using county 6001 and 6075 and k = 7 as example 
  
  # args:
  # model pred:
  # fipscounty   predict_date   k    pred
  # 6001          03-22-2020    3     300      => use data up until 03-19-2020
  #                                               to predict cumulative cases/
  #                                               # deaths on 03-22-2020
  # pred_type: 'cumcasestot' or 'cumdeathstot'
  
  # return list of sanity check plots:
  # pred_truth_plot, PI_plot, err_metrics_plot, emp_metric_fig6_plot,
  # emp_metric_tab6_plot, table6))
  
  # compare prediction results with truth
  truth_data <- getData()
  truth_data <- truth_data %>%
    ungroup() %>%
    select(fipscounty, date, pred_type) %>%
    rename(truth = pred_type) %>%
    distinct()

  combine_df <- model_pred %>% 
    inner_join(truth_data, by = c('fipscounty',
                                  'predict_date' = 'date'))
  combine_df_long <- combine_df %>% 
    filter(k %in% ks, fipscounty %in% counties) %>%
    pivot_longer(-c(fipscounty, predict_date, k), 
                 names_to = 'prediction', values_to = 'values')
  
  pred_truth_plot <- ggplot(combine_df_long,
         aes(as.Date(predict_date), values, color = prediction)) +
    geom_line() +
    facet_grid(fipscounty ~ k, scale = 'free') +
xlab("") + ylab("Cases") +
    theme_bw()
  
  # plotting MEPI 
  PI <- calcMEPI(model_pred %>% filter(k == ks[1]), truth_data, ks[1]) %>%
    select(fipscounty, tk, lower_tk, upper_tk, y_tk) %>%
    rename(predict_date = tk,
           lower = lower_tk,
           upper = upper_tk,
           truth = y_tk)
  
  PI_plot <- ggplot(PI %>% 
           filter(fipscounty %in% counties) %>%
           pivot_longer(-c(fipscounty, predict_date),
                        names_to = 'bound_type', values_to = 'values'),
         aes(as.Date(predict_date), values, color = bound_type)) +
    geom_line() +
    facet_grid(fipscounty ~., scale = 'free') +
    labs(title = 'Prediction Intervals by MEPI of counties 6001 and 6075 over
         7 day interval') +
    theme_bw()
  
  # plotting error metrics
  pi_metrics <- calcMetrics(PI %>% select(-truth),
                            truth_data)
  err_metrics_plot <- ggplot(pi_metrics %>% 
           pivot_longer(-fipscounty, names_to = 'metrics', values_to = 'values'),
         aes(values)) +
    geom_histogram() +
    facet_grid(.~metrics, scale = 'free') +
    labs(title = 'All counties with 7 day interval. Compare to fig 12 in paper') +
    theme_bw() 
  
  # plotting empirical metrics
  emp_metrics <- calcEmpMetrics(model_pred, truth_data)

  emp_metric_fig6_plot <- ggplot(emp_metrics %>%
           pivot_longer(-c(predict_date, k), 
                        names_to = 'metrics', values_to = 'values'),
         aes(as.Date(predict_date), values)) +
    geom_line() +
    geom_point() +
    facet_grid(metrics ~ k, scales = 'free') +
    labs(title = 'Empirical Metrics, compare to fig 6 in paper') +
    theme_bw()
  
  emp_metric_tab6_plot <- ggplot(emp_metrics %>%
           pivot_longer(-c(predict_date, k), 
                        names_to = 'metrics', values_to = 'values'),
         aes(values)) +
    geom_density() +
    facet_grid(k ~ metrics, scales = 'free') +
    labs(title = 'Empirical Metrics, compare to table 6 in paper') +
    theme_bw()
  
  table6 <- emp_metrics %>%
    pivot_longer(-c(predict_date, k), 
                 names_to = 'metrics', values_to = 'values') %>%
    group_by(metrics, k) %>%
    summarise(p10 = quantile(values, 0.1),
              median = quantile(values, 0.5),
              p90 = quantile(values, 0.9), .groups = 'keep')
  return(list(pred_truth_plot, PI_plot, err_metrics_plot, emp_metric_fig6_plot,
              emp_metric_tab6_plot, table6))
}

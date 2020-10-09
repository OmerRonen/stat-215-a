library(here)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(mltools)
library(data.table)
library(openintro)
source(here('R/clean.R'))

oneHotEncoder <- function(ling_dataset, weighting=FALSE){
  # this function performs one hot encoding for survey responses

  for (q in c(5:71)){
    categorical_answers = as.factor(ling_dataset[, q])
    one_hot <-  as.data.frame(one_hot(as.data.table(categorical_answers)))
    question_number = colnames(ling_dataset)[q]
    answers <- as.numeric(levels(categorical_answers))
    question_index = as.numeric(substr(question_number, 2, 4))
    answers_frequencey <- all.ans[[question_index]]$per
    column_names = paste(question_number, answers, sep = '_A')
    colnames(one_hot) <- column_names
    if (0 %in% answers){
      # we remove the no answer category
      one_hot <- one_hot[, c(2:length(answers))]
    }
    if (weighting)
      # this was not used - please ignore
      {one_hot = exp(1-answers_frequencey) * one_hot}
    if (q==5){
      ling_1h = one_hot
    }
    else{
    ling_1h = cbind(ling_1h, one_hot)}
  }
return(ling_1h)
}


.getLogOdds <- function(ds, q_num, states_vec){
  # calculate the log-odds per state using the contingency table.
  question <- .getQuestionString(q_num)
  ds$STATE <- states_vec
  
  
  observed =ds%>%group_by(STATE) %>% summarise_all(list(sum))
  observed= as.data.frame(observed)
  total_answers <- rowSums(select(observed,contains(question)))
  answers_count <- colSums(select(observed,contains(question)))
  answers_prob <- answers_count/sum(answers_count)
  
  q_observed=select(observed,contains(question))
  
  expected <- total_answers %*% t(answers_prob)
  
  log_odds <- log(q_observed/expected)
  log_odds$STATE <- observed$STATE
  log_odds$region <- tolower(abbr2state(log_odds$STATE))
  return(log_odds)
  
  
}

getQuestion <- function(number){
  # get question string from number
  q <- quest.mat$quest[quest.mat$qnum==number]
  return(q)
  
}

plotLogOdds <- function(q_num, show_legned=FALSE, answers = c()){
  # plots the log odds for two answer of a single question
  ind_1 = 1
  ind_2 = 2
  if (length(answers)>0){
    ind_1 = which(order(all.ans[[q_num]]$per, decreasing = T)==answers[1])
    ind_2 = which(order(all.ans[[q_num]]$per, decreasing = T)==answers[2])
    
    }
    
  p1<-.plotLogOdds(ling_1h, q_num, ling_clean$STATE, ind_1)
  p2 <- .plotLogOdds(ling_1h, q_num, ling_clean$STATE, ind_2)
  p2_l <- .plotLogOdds(ling_1h, q_num, ling_clean$STATE, 2, TRUE)
  # we want a shared legend
  .extract_legend <- function(my_ggp) {
    step1 <- ggplot_gtable(ggplot_build(my_ggp))
    step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
    step3 <- step1$grobs[[step2]]
    return(step3)
  }
  q <- getQuestion(q_num)
  if (show_legned){
  shared_legend <- .extract_legend(p2_l)
  grid.arrange(arrangeGrob(p1, p2, ncol = 2),
               shared_legend, nrow = 2, heights = c(10, 2),
               top=textGrob(q, gp=gpar(fontsize=24,font=8)))
  }
  if (!show_legned){
  grid.arrange(arrangeGrob(p1, p2, ncol = 2),
              nrow = 2, heights = c(10, 2),
               top=textGrob(q, gp=gpar(fontsize=24,font=8)))}
}

.plotLogOdds <- function(ds, q_num, states_vec, freq_order, show_legend=FALSE){
  # plot the log odds for a single answer
  log_odds <- .getLogOdds(ds,q_num,states_vec)
  log_odds <- log_odds[!is.na(log_odds$region), ]
  question <- .getQuestionString(q_num)
  answer_number <- order(all.ans[[q_num]]$per, decreasing = T)[freq_order]
  answer_str <- paste(question, '_A', answer_number, sep='')
  q <- getQuestion(q_num)
  a<- all.ans[[q_num]]$ans[answer_number]
  a_pct <- all.ans[[q_num]]$per[answer_number]
  title = paste( 'A: ',a,' (responces %  = ',a_pct,')', sep = '')
  colnames(log_odds)[ colnames(log_odds)==answer_str]='Log Odds'
  states <- map_data("state")
  map.df <- merge(states,log_odds, by="region", all.x=T)
  map.df <- map.df[order(map.df$order),]
  p<- ggplot(map.df, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=`Log Odds`), show.legend = show_legend)+
    geom_path()+ 
    scale_fill_gradientn(colours=topo.colors(20),na.value = "transparent",
                         breaks=c(-2,0,2),labels=c("Minimum",0,"Maximum"),
                         limits=c(-2,2))+
    coord_map() + theme_void() + ggtitle(title)+
    theme(plot.title = element_text(size = 20, face = "bold"), 
          legend.position="bottom", legend.key.size = unit(3, "cm"),
          legend.text=element_text(size=20),
          legend.title=element_text(size=20))
  return(p)
  
}


.getQuestionString <- function(q_num){
  # get the question string 50 -> `Q050`
  question_numebr <- as.character(q_num)
  if (nchar(question_numebr)<3){
    question_numebr <- paste('0', as.character(question_numebr), sep = '')
  }
  q_str <- paste('Q', question_numebr, sep = '')
  return(q_str)
}

questionMap <- function(ling_dataset, n_q, plot_theme, answers_exclude = c()){
  # scatter plot for each answer of a question on the us map
  answers_exclude = as.character(answers_exclude)
  question  = quest.mat$quest[n_q]
  answers <- all.ans[[n_q]]
  colnames(answers)[4] = 'Answer'
  state_df <- map_data("state")

  q_str <- .getQuestionString(n_q)
  filter = !ling_dataset[, c(q_str)] %in% answers_exclude
  ling_dataset <- ling_dataset[filter, ]
  
  
  # Make the column to join on.  They must be the same type.
  answers[, c(q_str)] <- rownames(answers)
  ling_dataset[, c(q_str)] <- as.character(ling_dataset[, c(q_str)] )
  ling_dataset <- inner_join(ling_dataset, answers, by = q_str)
  
  
  # Plot!
  p<- ggplot(ling_dataset) +
    geom_point(aes(x = long, y = lat, color = Answer), 
               size = 0.1, alpha = 0.5) +
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = state_df, colour = "black", fill = NA) +
    ggtitle(question) +
    plot_theme() +
    theme(plot.title = element_text(size = 8, face = "bold"), axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank())+
     xlab("") + ylab("") 
  return (p)
}

.extractMax<- function(ans, n){
  idx = order(ans$per, decreasing = T)[n]
  value = ans$per[idx]
  return(value)
}

plotHighestResponsesPct  <- function(){
  c_names = c('Percent','Order')
  l <- length(all.ans)

  values_1st <- data.frame(unlist(lapply(all.ans, .extractMax, n=1)), rep('First', l))
  values_2nd <- data.frame(unlist(lapply(all.ans, .extractMax, n=2)), rep('Second', l))
  values_3rd <- data.frame(unlist(lapply(all.ans, .extractMax, n=3)), rep('Third', l))
  
  colnames(values_1st) <- c_names
  colnames(values_2nd) <- c_names
  colnames(values_3rd) <- c_names
  
  df <- rbind(values_1st,values_2nd, values_3rd)
  title <-  'Distribution of the first second and 
  third most common answers accross survey questions'
  # basic histogram
  df %>% ggplot(aes(x=Percent, color=Order)) +
    ggtitle(title)+
    geom_boxplot() + theme_minimal() + 
    theme(plot.title = element_text(size = 8, face = "bold"),
          axis.ticks.y=element_blank(),axis.text.y = element_blank())
}



getPCA <- function(data, filename){
  # returns PCA embedding
  if (!file.exists(filename)){
  pca <- prcomp(data,scale=FALSE)
  pca_df <- data.frame(pca$x)
  write.csv(pca_df, filename,  row.names = FALSE)}
  pca_ling <- read.csv(filename)
  return(pca_ling)
  
  
  }

getClusters <- function(ds, centres){
  # returhns the k-means clusters vector
  k_means <- kmeans(ds, centres,nstart=3)
  return(as.factor(k_means$cluster))
}

getTSNE <- function(data, filename){
  # Returns t-SNE embedding
if (!file.exists(filename)){
  tsne <- Rtsne(ling_1h, dims = 2, perplexity=30,
                verbose=TRUE, max_iter = 500,
                check_duplicates=FALSE, pca=FALSE)
  tsne_embed <- as.data.frame(tsne$Y)
  colnames(tsne_embed)<- c('T1', 'T2')
  write.csv(tsne_embed,filename, row.names = FALSE)}

tsne_embed <- read.csv(filename)

return(tsne_embed)}

getAE <- function(data, filename){
  #Returns Auto Encoder embedding
  if (!file.exists(filename)){
    x_train <- as.matrix(ling_1h)
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 100, activation = "tanh", input_shape = ncol(x_train)) %>%
      
      layer_dense(units = 100, activation = "tanh") %>%
      layer_dense(units = 10, activation = "tanh",  name = "bottleneck") %>%
      
      layer_dense(units = 100, activation = "tanh") %>%
      
      layer_dense(units = ncol(x_train))
    # view model layers

    model %>% compile(
      loss = "mean_squared_error",
      optimizer = "adam"
    )
    # fit model
    model %>% fit(
      x = x_train,
      y = x_train,
      epochs = 20,
      verbose = 0,
      validation_split = 0.2
    )
    
    ae_model <- keras_model(inputs = model$input,
                            outputs = get_layer(model, "bottleneck")$output)
    
    ae_embed <- predict(ae_model, x_train)
    
    write.csv(ae_embed,filename,  row.names = FALSE)}
  
  
  ae_embed <- read.csv(filename)
  return(ae_embed)
}


plotCluster <- function(ds, n_clusters, dr_method){
  #Plot the cluster on the us map
  Clusters <- getClusters(ds,n_clusters)
  ling_clean%>%ggplot(aes(x = long, y = lat, color=Clusters)) +
    
    geom_point(size = 0.2) +
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = state_df, colour = "black", fill = NA)+
    theme_void() + ggtitle(dr_method) + scale_color_manual(values=c('blue', 'red', 'green', 'yellow', 'purple'))+
    scale_alpha_manual(values=c(0.5, 0.5, 0.5, 0.7))+
    theme(legend.position = "none")
}
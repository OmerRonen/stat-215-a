library(here)
library(foreach)
library(doParallel)
library(parallel)
library(logger)

source(here('R/utils.R'))

.innerLoopStep<- function(X, m, k){
  # performs the inner loop step and returns a similarity score
  N=nrow(X)
  sample.size <-  round(N*m)
  sample_1 <- sort(sample(c(1:N), sample.size))
  sample_2 <- sort(sample(c(1:N), sample.size))

  l_1 <- subSampleCluster(X, sample_1, k)

  l_2 <- subSampleCluster(X, sample_2, k)

  intersection <- intersect(sample_1, sample_2)

  
  l_1 <- l_1[rownames(l_1) %in% intersection, ]
  
  l_2 <- l_2[rownames(l_2) %in% intersection, ]
  return(similarityRcpp(l_1, l_2))
}

.innerLoop <- function(X, n, m, k){
  # wrapper around the .innerLoopStep function
  # returns a vector of size n of similarity scores
  # for a given data(X), subsample parameter(m) and 
  # numebr of cluster (k)
  similarities <- c()
  for (i in c(1:n)){
    similarities <- c(similarities, .innerLoopStep(X, m, k))
  }
  return(similarities)
}

main <- function(){
  # main function of this module
  
  # we have a cli for this module 
  args <- commandArgs(trailingOnly = TRUE)
  outfile <- args[1] # name of output file
  m <- as.numeric(args[2])   # sub sample ratio
  N <- as.numeric(args[3]) # number of iterations for each cluster
  X <- getX() 
  # config
  nCores <- 3
  nSub <- 10
  registerDoParallel(nCores) 
  # sending the jobs, different job for each cluster
  result <- foreach(i = 1:nSub) %dopar% {
    start.msg <- paste('Starting ', i, 'th job', sep = '')
    log_info(start.msg)
    output <- .innerLoop(X, N, m, i)
    end.msg <- paste('Finishing ', i, 'th job', sep = '')
    log_info(end.msg)
    output # this will become part of the out object
  }
  log_info('Got result')
  result <- data.frame(matrix(unlist(result),
                              ncol = length(result)))
  write.csv(x=result, file=here(outfile))
  save.msg <- paste(here(outfile), ' saved')
  log_info(save.msg)
  log_info('Job Done!')
  }

main()
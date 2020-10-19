library(here)
library('Rcpp')
library(foreach)
library(doParallel)
library(parallel)
library(logger)

source(here('R/utils.R'))



.innerLoopStep<- function(X, m, k){
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
  similarities <- c()
  for (i in c(1:n)){
    similarities <- c(similarities, .innerLoopStep(X, m, k))
  }
  return(similarities)
}



main <- function(){
  args <- commandArgs(trailingOnly = TRUE)
  outfile <- args[1]
  m <- as.numeric(args[2])
  N <- as.numeric(args[3])
  X <- getX()
  
  nCores <- 3
  nSub <- 10
  registerDoParallel(nCores) 
# ptm <- proc.time()  # start timer
  result <- foreach(i = 1:nSub) %dopar% {
    start.msg <- paste('Starting ', i, 'th job', sep = '')
    log_info(start.msg)
    output <- .innerLoop(X, N, m, i)
    end.msg <- paste('Finishing ', i, 'th job', sep = '')
    log_info(end.msg)
    output # this will become part of the out object
  }
  # proc.time() - ptm  # compute time elapsed
  log_info('Got result')
  result <- data.frame(matrix(unlist(result), ncol = length(result)))
  write.csv(x=result, file=here(outfile))
  save.msg <- paste(here(outfile), ' saved')
  log_info(save.msg)
  log_info('Job Done!')
  }

main()
library(latex2exp)
library(ggplot2)
library(dplyr)


.E <- function(X, p, mu_0, mu_1){
  c.0 <- dpois(X, lambda = mu_0)
  c.1 <- dpois(X, lambda = mu_1)
  t <- (p* c.0)/(p* c.0 + (1-p)*c.1)
  return(t)
}


.M.pi <- function(X, t){
  return(mean(t))
}
.M.mu.0 <- function(X, t){
  return(mean(t*X))
}
.M.mu.1 <- function(X, t){
  return(mean((1-t)*X))
}

EM.pois <- function(X, p, mu_0, mu_1, n){
  for (i in seq(1,n)){
    t_i <- .E(X, p, mu_0, mu_1)
    pi <- .M.pi(X, t_i)
    mu_0 <- .M.mu.0(X, t_i)
    mu_1 <- .M.mu.1(X, t_i)
  }
  return(t_i)
}
acc <- c()
mu_ones <- seq(1, 20)
for (mu_1 in mu_ones){
mu_0 = 1
Z <- rbinom(100,size = 1, prob = 0.5)
X<- ifelse(Z==1, rpois(100, mu_0), rpois(100, mu_1))
t <- EM.pois(X, 0.4, 1,1, 1000)
acc <- c(acc, mean(round(t)==Z))
}

df <- data.frame(mu_ones, acc)

df %>% ggplot(aes(x=mu_ones, y=acc)) + geom_point()+
  xlab(expression(mu[1])) + ylab('Accuracy') +
  ggtitle(expression(paste('Accuracy for ',mu[0], ' = 1 (poisson data)')))+
  ggsave('pois.png')


acc <- c()
pi_ones <- seq(1, 20)/20
pi_0 <- 0.05
for (pi_1 in pi_ones){
  Z <- rbinom(100,size = 1, prob = 0.5)
  X<- ifelse(Z==1, rbinom(100, 100,  pi_0), rbinom(100,100,  pi_1))
  t <- EM.pois(X, 0.4, 0.3,0.3, 1000)
  acc <- c(acc, mean(round(t)==Z))
}

df <- data.frame(pi_ones, acc)

df %>% ggplot(aes(x=pi_ones, y=acc)) + geom_point()+
  xlab(expression(pi[1])) + ylab('Accuracy') +
  ggtitle(expression(paste('Accuracy for ',pi[0], ' = 0.05 (binomial data)')))+
  ggsave('binom.png')

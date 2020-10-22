library(here)
library(ggplot2)
library(dplyr)
library(gridExtra)

plot.benhur <- function(){
# this function produces the plot from benhur paper
sim.data <- read.csv(here("results/sim.csv"))[ ,-1]
# sorting so it'll be easier to calculate quantiles
sim.sorted <- apply(sim.data,2,sort,decreasing=F)
cumulative <- rep(c(1:100)/100, 10)
clusters <- as.factor(rep(1:10, each = 100))
sim.plot <- data.frame(as.vector(sim.sorted),
                       cumulative, clusters)
colnames(sim.plot) <- c('similarity', 'cumulative', 'clusters')

p <- sim.plot %>% ggplot(aes(x=similarity,
                             y=cumulative,
                             color=clusters)) +
  geom_point(size=0.2, alpha=0.7, show.legend = FALSE) +
  theme_minimal()
# adding a bit of text
for (i in c(1:9)){
  quantile = 0.35
  margin <- 0.015
  x.vec <- sim.sorted[quantile*100, ]-margin
  p <- p+annotate('text', y=quantile, x=x.vec[i],
                  label=paste("k=", i, sep = ''), size=1.5)
}
# for readibility since k=9 and k=10 are close
quantile=0.75
x.vec <- sim.sorted[quantile*100, ]-margin

p <- p+annotate('text', y=quantile, x=x.vec[10],
                label=paste("k=", 10, sep = ''), size=1.5)+
  ggtitle('B')

p
p2 <- ggplot(sim.plot, aes(x=similarity)) +
  geom_histogram(color='blue', bins=40) + 
  facet_wrap(~ clusters) +theme_minimal()+ 
  theme(
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(), 
         axis.title.y = element_blank())+
  ggtitle('A')

grid.arrange(p2, p, ncol=2)

}


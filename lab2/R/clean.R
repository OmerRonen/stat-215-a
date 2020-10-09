library(here)
library(ggplot2)
library(dplyr)
library(naniar)
library(reshape)

ling_data <- read.table(here('data/lingData.txt'), header = T)
ling_location <- read.table(here('data/lingLocation.txt'), header = T)
# question_data contains three objects: quest.mat, quest.use, all.ans
load(here("data/question_data.RData"))

cleanLingData <- function(dataset){
  dataset <-.cleanNas(dataset)
  dataset <- .cleanGeo(dataset)
  return(dataset)
}

visualizeNas <- function(dataset){
  na_cols = colSums(is.na(dataset))> 0
  if (sum(na_cols)==0){return()}
  dataset_nas <- dataset[, na_cols]
  
  vis_miss(dataset_nas) + theme_minimal()+ ylab('Respondent') +ggsave(here('data/nas.png'))
}


# So we have a few zip code missing we'll simply remove those, due to the low number of such zip codes.

# Next we'll take a look the responses to see if any outliers 
meltData <- melt(ling_data[, 5:71])
p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")


.cleanGeo <- function(dataset){
  dataset <- dataset[dataset$long > -125, ]
  return(dataset)
}
.cleanNas <- function(dataset){
  dataset <- dataset[rowSums(is.na(dataset))==0, ]
  return(dataset)
}

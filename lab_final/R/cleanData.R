library(tidyverse)
library(sqldf)

checkCounty <- function(countyname, counties){
  # a function to check if county in adj_counties df is Bay Area and South
  # Central Valley counties list
  # countyname: from adj_counties df, counties: list of Bay Area and South
  # Central Valley counties
  # return a logical value if the countyname is in counties list
  
  flag <- FALSE
  for(c in counties){
    flag <- grepl(c, countyname)
    if(flag){break}
  }
  return(flag)
}


filterAdjCounties <- function(adj_counties, counties){
  # a function filters adj counties df to only Bay Area and South Central 
  # Valley counties
  # adj_counties: df from county_adjacency2010.csv
  # return a filtered data frame
  
  filtered_adj_counties <- adj_counties %>%
    rowwise() %>%
    mutate(ifcounty = checkCounty(countyname, counties)) %>%
    filter(ifcounty == TRUE) %>%
    select(countyname, fipscounty, neighborname, fipsneighbor)
  return(filtered_adj_counties)
}

.calcCumCnts <- function(counts_df, filtered_adj_counties){
  # function to calculate cumulative counts of cases and deaths on 3, 5, 7, 14 
  # days ahead
  # counts: df from counts.csv
  # return a df of cumulative cnts of cases and deaths on county and dates level
  
  counts.fname <- here('data/cum_cnts.csv')
  if (file.exists(counts.fname)){
    data <- read.csv(counts.fname)
    return(data[, -1])
  }
  
  counts <- counts_df %>%
    filter(countyFIPS %in% unique(filtered_adj_counties$fipsneighbor)) %>%
    mutate(date = as.Date(date))
  
  cum_cnts <- sqldf('select c.countyFIPS
        , c.date as date__Date
        , c1.n_cases as cumcases3
        , c1.n_deaths as cumdeaths3
        , c2.n_cases as cumcases4
        , c2.n_deaths as cumdeaths4
        , c3.n_cases as cumcases5
        , c3.n_deaths as cumdeaths5
        , c4.n_cases as cumcases7
        , c4.n_deaths as cumdeaths7
        , c5.n_cases as cumcases14
        , c5.n_deaths as cumdeaths14
        from counts c
          left join counts c1
            on c.countyFIPS = c1.countyFIPS
            and c.date = c1.date - 3
        left join counts c2
            on c.countyFIPS = c2.countyFIPS
            and c.date = c2.date - 4
        left join counts c3
            on c.countyFIPS = c3.countyFIPS
            and c.date = c3.date - 5
        left join counts c4
            on c.countyFIPS = c4.countyFIPS
            and c.date = c4.date - 7
        left join counts c5
            on c.countyFIPS = c5.countyFIPS
            and c.date = c5.date - 14', method = 'name__class')
  
  return(cum_cnts)
}

makeFullData <- function(filtered_adj_counties, counts, abridged){
  # a function to join all datasets: adj_counties, counts, abridged (self and
  # neighbor) to 1 full dataset
  # filtered_adj_counties: filtered adj_counties to only specified counties,
  # cum_cnts: df of cumulative counts of cases and deaths
  # abridge: df from county_data_abridged.csv
  # return a full dataframe with all metrics of individual county and its 
  # neighbor counties (_neighbor at the end of column names)
  
  counts$date <- as.Date(counts$date)
  cum_cnts <- .calcCumCnts(counts, filtered_adj_counties)
  cum_cnts$date <- as.Date(cum_cnts$date)
  full_data <- filtered_adj_counties %>% 
    filter(fipscounty != fipsneighbor) %>%
    left_join(counts, by = c('fipscounty' = 'countyFIPS')) %>%
    rename(cumcasestot = n_cases,
           cumdeathstot = n_deaths) %>%
    left_join(cum_cnts,
              by = c('fipscounty' = 'countyFIPS',
                     'date' = 'date')) %>%
    left_join(cum_cnts, 
              by = c('fipsneighbor' = 'countyFIPS',
                     'date' = 'date'),
              suffix = c('','_neighbor')) %>%
    left_join(abridged,
              by = c('fipscounty' = 'countyFIPS')) %>%
    left_join(abridged,
              by = c('fipsneighbor' = 'countyFIPS'),
              suffix = c('','_neighbor')) %>% 
    group_by(fipscounty) %>% 
    mutate(max_date = max(as.Date(date))) %>%
    filter(date != max_date)
  return(full_data)
}


getData <- function(){
  bay_counties <- c('Alameda', 'Contra Costa', 'Marin ', 'Napa', 'San Francisco', 
                    'San Mateo', 'Santa Clara', 'Solano', 'Sonoma')
  south_counties <- c('Fresno', 'Madera', 'Merced', 'Tulare', 'Stanislaus')
  counties <- c(bay_counties, south_counties)
  # load data
  adj_counties <- read.csv(here('data/county_adjacency2010.csv'))
  counts <- read.csv(here('data/counts.csv'))
  abridged <- read.csv(here('data/county_data_abridged.csv'))
  # clean data
  filtered_adj_counties <- filterAdjCounties(adj_counties, counties)
  full_data <- makeFullData(filtered_adj_counties, counts, abridged)
  return(full_data)
  
}


getDataFull <- function(){

  # load data
  adj_counties <- read.csv(here('data/county_adjacency2010.csv'))
  counts <- read.csv(here('data/counts.csv'))
  abridged <- read.csv(here('data/county_data_abridged.csv'))
  # clean data
  full_data <- makeFullData(adj_counties, counts, abridged)
  return(full_data)
  
}

plot_km <- function(data, x, y, km, title='', smp){
  data <- data[smp,]
  p <- data %>% ggplot() + 
    geom_point(aes(x=data[,x], y=data[,y], 
                   color=as.factor(km$cluster[smp])),
               shape='o', alpha=0.5) + 
    theme_minimal()+
    theme(legend.title=element_blank(), plot.title=element_text(title),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = 'none')
  return(p)
}

plot_km_geo <- function(km, loc, title='', smp){
  loc2 <- loc[smp,]
  clust <- km$cluster[smp]
  p <- loc2 %>% filter(long > -125) %>% ggplot() +
    geom_polygon(data=states, aes(x=long,y=lat, group=group), 
                 fill='white', color='black') +
    geom_point(aes(x=long, y=lat, 
                   color=as.factor(clust[which(long>-125)])),
               shape='x', alpha=0.5) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = 'none')
  return(p)
}
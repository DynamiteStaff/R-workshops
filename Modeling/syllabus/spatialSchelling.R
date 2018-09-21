library(sf)
library(ggplot2)
library(dplyr)



load("cityTractsWithPopAndVacancy.RData")

mytoronto <-  toronto_tr

ptor  <-  ggplot(mytoronto) +
  geom_sf()
ptor


nbgroups <-  4
groupValues <-  seq.int(from = 1, to=nbgroups)
colorMapping <-  brewer.pal(nbgroups,"Dark2")
pctList <- c(0.25, 0.3, 0.3, 0.15)
pctList %>%  sum



tractIDS <-  unique(mytoronto$GeoUID)
nbTracts <-  length(tractIDS)

popSize <-  10000







tractgroupPopValues <- sample(groupValues, size = tract$Households,replace = T, prob = pctList)


tract <-  (mytoronto %>% filter(GeoUID=="5350102.02"))
tract$popByGroup <- list(data.frame(pop=tract$Population * pctList, groupValue = groupValues  ))

tract$popByGroup %>% 
  
  
  isUnhappyInTract <-  function(tract, value , tolerance){
    
    
    
    return()
  }


moveOne <- function(tor,tractFromID, tractToID){
  
  toto <-  tor %>% filter(GeoUID == tractFromID)
  frfr <-  tor %>%  filter(GeoUID == tractToID)
  
  
  
  
  
  return(tor)
}


isHappybyTract <-  function(tract, )
  
  
  
  
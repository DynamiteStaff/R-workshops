library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)


load("cityTractsWithPopAndVacancy.RData")

mytoronto <-  na.omit(toronto_tr)
tractIDS <-  unique(mytoronto$GeoUID)

ptor  <-  ggplot(mytoronto) +
  geom_sf()
ptor


nbgroups <-  4
groupValues <-  seq.int(from = 1, to=nbgroups)
colorMapping <-  brewer.pal(nbgroups,"Dark2")
pctList <- c(0.25, 0.3, 0.3, 0.15)
pctList %>%  sum



popSize <-  10000


genSchellingTractState <-  function(popSize,  mytoronto){
  tractIDS <-  unique(mytoronto$GeoUID)
  popTotal <-  sum(mytoronto$Population)
  
  #value are sampled according to percentage
  value <- sample(groupValues, size = popSize,replace = T, prob = pctList)
  
  # localisation is sample according to tracts populations
  localisation <-  sample(tractIDS, size= popSize , replace = T , prob= mytoronto$Population / popTotal )
  ID <-  seq.int(popSize)
  happy <-  NA

  res <- data.frame(ID,localisation,value, happy)
return(res)
  }


s <-  genSchellingTractState(10000,mytoronto)


isHappyInTract <-  function (ID, state, tolerance){
  hhLoc <-  state[ID,"localisation"]
  hhValue <-  state[ID, "value"]
  
  neigh <-  state %>%  filter(localisation == hhLoc) 
  
  numberOfDiff <-sum(hhValue!=neigh$value, na.rm = T)  
  happy <-  (numberOfDiff / nrow(neigh) ) < tolerance
  return(happy)  
}

tolerance <-  0.3

updateHappiness <- function(state, tolerance){
  state$happy <-  sapply(state$ID, FUN = isHappyInTract, state=state, tolerance = 0.3)
  return(state)
}


getUnhappy <-  function(state){
  return(filter(state, !happy))
}




moveOne <-  function(state, tolerance, tractIDS){
  #take one unhappy householder 
  unhappyHHs <-  getUnhappy(state)
  uhhhID <- sample_n(unhappyHHs$ID, 1)
  oneTract <-  sample_n(tractIDS, 1)
  
  
  #State is now the localtion of householders,  so the localisation change
  
    state[uhhhID,"localisation"] <-  oneTract
    #origin cell is no longer happy or unhappy since it's empty
  state[uhhhID,"happy"] <- NA
  state <-  updateHappiness(state, tolerance )
  return(state)
}




simulate <-  function(steps, state, tolerance){
  for (i in 1:steps){
    if (i %% 200 ==0) {
      cat("step : ", i , "\n")
    }
    unhappy <-  getUnhappy(state)
    if (nrow(unhappy) == 0){
      cat("Everybody is fine with their location\n")
      break 
    }
    state <- moveOne(state,tolerance)
  }
  return(state)
}











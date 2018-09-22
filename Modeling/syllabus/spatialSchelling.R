library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ineq)

load("cityTractsWithPopAndVacancy.RData")

mytoronto <-  na.omit(toronto_tr)
tractIDS <-  as.character(unique(mytoronto$GeoUID))

ptor  <-  ggplot(mytoronto) +
  geom_sf()
ptor


nbgroups <-  4
groupValues <-  seq.int(from = 1, to=nbgroups)
colorMapping <-  brewer.pal(nbgroups,"Dark2")
pctList <- c(0.6, 0.1, 0.2, 0.1)
pctList %>%  sum



popSize <-  10000


genSchellingTractState <-  function(popSize,  mytoronto){
  tractIDS <-  unique(mytoronto$GeoUID)
  popTotal <-  sum(mytoronto$Population)
  
  #value are sampled according to percentage
  value <- sample(groupValues, size = popSize,replace = T, prob = pctList)
  
  # localisation is sample according to tracts populations
  localisation <-  as.character(sample(tractIDS, size= popSize , replace = T , prob= mytoronto$Population / popTotal ))
  ID <-  seq.int(popSize)
  happy <-  NA

  res <- data.frame(ID,localisation,value, happy, stringsAsFactors = F)
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

#only 2 tracts int he whole state are affected by househoulders movings 
updateHappinessOftract <- function(tract, state, tolerance){
  
  state[state$localisation == tract,]$happy <-  sapply(state[state$localisation == tract,]$ID, FUN = isHappyInTract, state=state, tolerance = 0.3)
  return(state)
}




getUnhappy <-  function(state){
  return(filter(state, !happy))
}




moveOne <-  function(state, tolerance, tractIDS){
  if (nrow(getUnhappy(state))==0){
    cat("evryone happy \n")
    return(state)
  }
    
    #take one unhappy householder 
  unhappyHHs <-  getUnhappy(state)
  uhhhID <- sample(unhappyHHs$ID, 1)
  destiTract <-  sample(tractIDS, 1)
  origTract <-  state[uhhhID,"localisation"]
  

  
  #cat("ORIGIN: ", origTract ,"\n")
  #cat("DESTINATION: " , destiTract, "\n")
  
  
#the localisation change
state[uhhhID,"localisation"] <-  destiTract
   
  
    state <-  updateHappinessOftract(origTract,state, tolerance )
     state <-  updateHappinessOftract(destiTract,state, tolerance )
    
  #state <-  updateHappiness(state,tolerance)
  if(isHappyInTract(uhhhID,state, tolerance)){
    #cat("happy !!! \n")
  }
    
      return(state)
}




simulate <-  function(steps, state, tolerance){
  for (i in 1:steps){
    if (i %% 200 ==0) {
      cat("step : ", i , "\n")
    }
    
    if (nrow(getUnhappy(state)) == 0){
      cat("Everybody is fine with their location\n")
      break 
    }
    state <- moveOne(state,tolerance, tractIDS)
  }
  return(state)
}



tolerance <-  0.3
s <-  genSchellingTractState(10000,mytoronto)
s <- updateHappiness(s,tolerance )
nrow(getUnhappy(s))
s <-  simulate(1000,s, tolerance)
nrow(getUnhappy(s))


#measures 


pctUnHappybyTract <-  function(tract, state){
  nbHH <- nrow(state[state$localisation == tract,]) 
  nbUHHH <-  sum(state[state$localisation == tract,"happy"], na.rm = T)
  return(nbUHHH / nbHH)
}

theilEntropyBytract <-  function(tract,state){
  return(ineq(state[state$localisation == tract, "value"] , type = "Theil", na.rm = T))
}


 

mytoronto$pctUnhappy <- sapply(tractIDS, FUN = pctUnHappybyTract, state=s )
mytoronto$theil <-  sapply(tractIDS, FUN = theilEntropyBytract, state=s )  


updateMeasures <-  function(mytoronto, currentState){
  mytoronto$pctUnhappy <- sapply(tractIDS, FUN = pctUnHappybyTract, state=currentState )
  mytoronto$theil <-  sapply(tractIDS, FUN = theilEntropyBytract, state=currentState )  
return(mytoronto)
}

displayTheil <-  function(mytoronto, state){
  mytoronto <-  updateMeasures(mytoronto, state)
  ptor  <-  ggplot(mytoronto) +
    geom_sf(aes(fill=theil))
  return(ptor)
}
displayPctUnhappy <-  function(mytoronto, state){
  mytoronto <-  updateMeasures(mytoronto, state)
  ptor  <-  ggplot(mytoronto) +
    geom_sf(aes(fill=pctUnhappy))
  return(ptor)
}



displayTheil(mytoronto , s)
displayPctUnhappy(mytoronto,s)
s <-  simulate(1000,s, tolerance)
nrow(getUnhappy(s))
displayPctUnhappy(mytoronto,s)

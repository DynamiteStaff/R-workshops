library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ineq)
library(cancensus)
library(cancensusHelpers)

load("cityTractsWithPopAndVacancy.RData")

mytoronto <-  na.omit(toronto_tr)
tractIDS <-  as.character(unique(mytoronto$GeoUID))
mytoronto$popSchelling <-  NA
vacancypct <-0.03 
mytoronto$emptyHouseholds <-   round(vacancypct   * mytoronto$Households)


nbgroups <-  4
groupValues <-  seq.int(from = 1, to=nbgroups)
colorMapping <-  brewer.pal(nbgroups,"Dark2")
pctList <- c(0.6, 0.1, 0.2, 0.1)
pctList %>%  sum




genSchellingTractState <-  function(popSize,  mytoronto, vacancypct){
  tractIDS <-  unique(mytoronto$GeoUID)
  popTotal <-  sum(mytoronto$Population)
  
  mytoronto$emptyHouseholds <-   round(vacancypct   * mytoronto$Households)
  
  #value are sampled according to percentage
  value <- sample(groupValues, size = popSize,replace = T, prob = pctList)
  
  # localisation is sample according to tracts populations
  localisation <-  as.character(sample(tractIDS, size= popSize , replace = T , prob= mytoronto$Population / popTotal ))
  ID <-  seq.int(popSize)
  happy <-  NA
  
  res <- data.frame(ID,localisation,value, happy, stringsAsFactors = F)
  return(res)
}


isHappyInTract <-  function (ID, state, tolerance){
  hhLoc <-  state[ID,"localisation"]
  hhValue <-  state[ID, "value"]
  
  neigh <-  state %>%  filter(localisation == hhLoc) 
  
  
  
  numberOfDiff <-sum(hhValue!=neigh$value, na.rm = T)  
  happy <-  (numberOfDiff / nrow(neigh) ) < tolerance
  return(happy)  
}


updateHappiness <- function(state, tolerance){
  state$happy <-  sapply(state$ID, FUN = isHappyInTract, state=state, tolerance = 0.3)
  return(state)
}

#only 2 tracts in the whole state are affected by househoulders movings 
updateHappinessOftract <- function(tract, state, tolerance){
  state[state$localisation == tract,]$happy <-  sapply(state[state$localisation == tract,]$ID, FUN = isHappyInTract, state=state, tolerance = 0.3)
  return(state)
}




getUnhappy <-  function(state){
  return(filter(state, !happy))
}




moveOne <-  function(state, tolerance, mytoronto){
  if (nrow(getUnhappy(state))==0){
    cat("evryone happy \n")
    return(state)
  }
  
  #take one unhappy householder 
  unhappyHHs <-  getUnhappy(state)
  uhhhID <- sample(unhappyHHs$ID, 1)
  
  #take one tract with free space 
  tractIDSfree <-  mytoronto[mytoronto$emptyHouseholds >0,"GeoUID" ]  
  destiTract <-  sample(tractIDSfree$GeoUID, 1)
  
  
  origTract <-  state[uhhhID,"localisation"]
  
  
  #the localisation change
  state[uhhhID,"localisation"] <-  destiTract
  #number of empty housholds have to be updated
  mytoronto[mytoronto$GeoUID==destiTract,]$emptyHouseholds <-    mytoronto[mytoronto$GeoUID==destiTract,]$emptyHouseholds -1
  mytoronto[mytoronto$GeoUID==origTract,]$emptyHouseholds <-  mytoronto[mytoronto$GeoUID==origTract,]$emptyHouseholds +1
  
  
  #happiness update in concerned tracts
  state <-  updateHappinessOftract(origTract,state, tolerance )
  state <-  updateHappinessOftract(destiTract,state, tolerance )
  
  
  return(state)
}




simulate <-  function(steps, state, tolerance, mytoronto, logMeasures=F){
  for (i in 1:steps){
    if (i %% 200 ==0) {
      cat("step : ", i , "\n")
    }
    
    if (nrow(getUnhappy(state)) == 0){
      cat("Everybody is fine with their location\n")
      break 
    }
    state <- moveOne(state,tolerance,mytoronto)
    if (logMeasures){
      
      mytoronto$theil <-  sapply(tractIDS, FUN = theilEntropyBytract, state )  
      theilE <- theilEntropy(state)
      mytoronto$theilGap <- theilE - mytoronto$theil
      #notice the tricky super affectation <<- to acces global variable outside the scope of the function
      stepsACC <<-  append(stepsACC, i)
      NbunhappyAcc <<-  append( NbunhappyAcc, nrow(getUnhappy(state)))
      absTheilGapSumAcc <<-  append(absTheilGapSumAcc, sum(abs(mytoronto$theilGap),na.rm = T))
    }
    cat(nrow(getUnhappy(state)), "\n")
    
  }
  return(state)
}

#setup function

setupState <-  function(popSize, mytoronto, vacancypct){
  s <-  genSchellingTractState(10000,mytoronto,vacancypct)
  s <- updateHappiness(s,tolerance )
  mytoronto <-  updateMeasuresToronto(mytoronto)
  return(s)  
}


## measures accumulators
stepsACC <-  c()
NbunhappyAcc <-  c()
absTheilGapSumAcc <-  c()

resetAccumulators <-  function(){
  stepsACC <<-  c()
  NbunhappyAcc <<-  c()
  absTheilGapSumAcc <<-  c()
}



pctUnHappybyTract <-  function(tract, state){
  nbHH <- nrow(state[state$localisation == tract,]) 
  nbUHHH <-  sum(state[state$localisation == tract,"happy"], na.rm = T)
  return(nbUHHH / nbHH)
}

theilEntropyBytract <-  function(tract,state){
  return(ineq(state[state$localisation == tract, "value"] , type = "Theil", na.rm = T))
}


theilEntropy <-  function(state){
  return(ineq(state$value, type = "Theil", na.rm = T))
}




updateTractPop <-  function(tract, state){
  return(nrow(state[state$localisation==tract,  ]) )
}


updateMeasuresToronto <-  function(mytoronto, currentState){
  mytoronto$pctUnhappy <- sapply(tractIDS, FUN = pctUnHappybyTract, state=currentState )
  mytoronto$theil <-  sapply(tractIDS, FUN = theilEntropyBytract, state=currentState )  
  theilE <- theilEntropy(currentState)
  mytoronto$theilGap <- theilE - mytoronto$theil
  return(mytoronto)
}

displayTheil <-  function(mytoronto, state){
  mytoronto <-  updateMeasuresToronto(mytoronto, state)
  ptor  <-  ggplot(mytoronto) +
    geom_sf(aes(fill=theil))
  return(ptor)
}
displayPctUnhappy <-  function(mytoronto, state){
  mytoronto <-  updateMeasuresToronto(mytoronto, state)
  ptor  <-  ggplot(mytoronto) +
    geom_sf(aes(fill=pctUnhappy))
  return(ptor)
}
displayTheilGap <-  function(mytoronto, state){
  mytoronto <-  updateMeasuresToronto(mytoronto, state)
  ptor  <-  ggplot(mytoronto) +
    geom_sf(aes(fill=theilGap))+
    scale_fill_gradient2()
  return(ptor)
}


s <-  setupState(100000,mytoronto,vacancypct)
nrow(getUnhappy(s))
displayPctUnhappy(mytoronto,s)
s <-  simulate(1000,s, tolerance)
nrow(getUnhappy(s))
displayPctUnhappy(mytoronto,s)
displayTheilGap(mytoronto,s)



library(reshape2)
library(ggplot2)
library(seg)
library(dplyr)
library(sf)
library(shiny)
library(RColorBrewer)
library(ineq)

matrixSchellingState <-  function(size, pctOccupied=.5, pctRed=0.5, pctBlue=1-pctRed ){
  
  matrixState <- matrix(NA,size,size)
  nbAgents <- floor(pctOccupied * size^2)
  householdCells <- sample(size ^ 2, nbAgents)
  matrixState[householdCells] <-  sample(c(1,2), nbAgents, replace = T, prob = c(pctRed,pctBlue))
  return(matrixState)
}



colorMapping <- c("1"="red", "2"="blue") 


fancyColors <- c("1"="bisque", "2"="aquamarine") 



#save as a function for later use
displayState <- function(mystate, dotSize=1){
  myggplot2 <- ggplot(mystate, aes(x=X, y=Y)) +
    geom_point(size=dotSize, aes(colour=factor(value)))+ #size has to be adjusted regarding the size of the state
    scale_color_manual( values=fancyColors, name="Group" )+
    theme_void()+
    coord_fixed(ratio = 1) + 
    ggtitle("Schelling state")
  return(myggplot2) 
}
#to ensure the call to filter function is calling the dplyr version
filter = dplyr::filter
#same function but for a state in the long format
meAndMyNeighbors <- function (state,i,j) {
  #this version doesn't suppose a square world, but a 9 Moore neighborhod
  neigh <-   filter(state, between(X,i-1,i+1) & between(Y,j-1,j+1) & !is.na(value))
  return(neigh)
}

meAndMyNeighborsByID <- function (id,state) {
  #only if id = nrow
  i <-  state[id,"X"]
  j <-  state[id,"Y"]
  #this version doesn't suppose a square world, but a 9 Moore neighborhod
  #neigh <-   filter(state, between(X,i-1,i+1) & between(Y,j-1,j+1) & !is.na(value))
  neigh <-   filter(state, between(X,i-1,i+1) & between(Y,j-1,j+1))
  
  return(neigh)
}



getneighbors <- function(state, i,j){
  neighborhood <- meAndMyNeighbors(state, i, j)
  #exclude the caller cell
  neighborhood <- neighborhood[!(neighborhood$X==i & neighborhood$Y==j),]
  return(neighborhood)
}

getneighborsByID <- function(id, state){
  neighborhood <- meAndMyNeighborsByID(id, state)
  #exclude the caller cell
  neighborhood <- neighborhood[neighborhood$ID !=id,]
  return(neighborhood)
}



isHappy <- function(state, i , j, tolerance){
  cell <-  filter(state, X==i & Y==j)
  if (is.na(cell$value)){return(NA)}
  neigh <- getneighbors(state,i,j) 
  numberOfDiff <-sum(cell$value!=neigh$value, na.rm = T)  
  happy <-  (numberOfDiff / nrow(neigh) ) < tolerance
  return(happy)  
} 


isHappybyID <-  function(id, state, tolerance){
  cell <-  filter(state, ID==id)
  if (is.na(cell$value)){return(NA)}
  neigh <- getneighborsByID(id,state) 
  numberOfDiff <-sum(cell$value!=neigh$value, na.rm = T)  
  happy <-  (numberOfDiff / nrow(neigh) ) < tolerance
  return(happy)  
}


updateHappiness <- function(state, tolerance){
  state$happy <-  sapply(state$ID, FUN = isHappybyID, state=state, tolerance = 0.3)
  return(state)
}

getUnhappy <-  function(state){
  return(filter(state, !happy))
}


# we update the display function code to display unhappiness
displayStateUnhappy <- function(meltedState, dotsize = 5){
  if(nrow(getUnhappy(meltedState)) ==0){
    pp <- displayState(meltedState, dotSize = dotsize)  
    return(pp)
  }
  pp <- displayState(meltedState, dotSize = dotsize) + 
    geom_text(data = subset(meltedState, !happy ), aes(X,Y,label = ":-|"), angle=-90, size= dotsize) 
  return(pp)
}



# save as function for later



#melted state constructor
createMeltedSchellingState <- function(size, pctOccupied=.5, pctRed=0.5, pctBlue=1-pctRed, tolerance){
  matState <-  matrixSchellingState(size, pctOccupied, pctRed, pctBlue)
  meltedState <-  melt(matState,varnames = c("X","Y"))
  # we add an explicit ID of cells (row number), it will be useful
  meltedState$ID <- seq.int(nrow(meltedState))
  
  meltedState <-  updateHappiness(meltedState, tolerance)
  return(meltedState)
}




getEmptyCells <-  function(state){
  return(state %>%filter(is.na(value))  )
}




moveOne <-  function(state, tolerance, followMover=F){
  #take one unhappy householder 
  unhappyHHs <-  getUnhappy(state)
  uhhh <- sample_n(unhappyHHs, 1)
  uhhhID <-  uhhh$ID
  uhhhvalue <-  uhhh$value
  emptyCells <-  getEmptyCells(state)
  oneEmptyCell <-  sample_n(emptyCells,1)
  emptyID <- oneEmptyCell$ID
  
  
  if (followMover){
    pp <-  displayStateUnhappy(state)+
      geom_point(data= uhhh, aes(X,Y),color="red", size = 8, alpha= 0.3)+
      geom_point(data = oneEmptyCell, aes(X,Y),color="green", size = 8, alpha=0.3)
    
    print(pp)
  }
  
  #State has to be  by swapping values of these two cells , ID
  state[emptyID,"value"] <- uhhhvalue
  state[uhhhID,"value"] <- NA
  
  #origin cell is no longer happy or unhappy since it's empty
  state[uhhhID,"happy"] <- NA
  
  #moving changes happiness of the destination neighborhood, so we update it
  destNeigh <-  meAndMyNeighborsByID(id = emptyID,state)
  state[destNeigh$ID,"happy"] <-  sapply(destNeigh$ID, FUN = isHappybyID, state=state, tolerance)
  
  #state <-  updateHappiness(state, tolerance )
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




matrixStateMultiGroups <-  function(size, pctOccupied=.5, pctList, groupValues ){
  matrixState <- matrix(NA,size,size)
  nbAgents <- floor(pctOccupied * size^2)
  householdCells <- sample(size ^ 2, nbAgents)
  matrixState[householdCells] <-  sample(groupValues, nbAgents, replace = T, prob = pctList)
  return(matrixState)
}



createMultiGroupState <- function(size, pctOccupied=.5, pctList, groupValues , tolerance){
  matState <-  matrixStateMultiGroups(size, pctOccupied, pctList, groupValues)
  meltedState <-  melt(matState,varnames = c("X","Y"))
  # we add an explicit ID of cells (row number), it will be useful
  meltedState$ID <- seq.int(nrow(meltedState))
  
  meltedState <-  updateHappiness(meltedState, tolerance)
  return(meltedState)
}


displayStateMG <- function(mystate, dotSize=1, colorList){
  myggplot2 <- ggplot(mystate, aes(x=X, y=Y)) +
    geom_point(size=dotSize, aes(colour=factor(value)))+ #size has to be adjusted regarding the size of the state
    scale_color_manual( values=colorList, name="Group" )+
    theme_void()+
    coord_fixed(ratio = 1) + 
    ggtitle("Schelling state")
  return(myggplot2) 
}

displayStateUnhappyMG <- function(meltedState, dotsize = 5, colorList){
  if(nrow(getUnhappy(meltedState)) ==0){
    pp <- displayStateMG(meltedState, dotsize, colorList )  
    return(pp)
  }
  pp <- displayStateMG(meltedState, dotsize, colorList) + 
    geom_text(data = subset(meltedState, !happy ), aes(X,Y,label = ":-|"), angle=-90, size= dotsize) 
  return(pp)
}





###### measures

getSimilarNeighborsNumber <-  function(id , state){
  cell <-  filter(state, ID==id)
  if (is.na(cell$value)){return(NA)}
  neigh <- getneighborsByID(id,state) 
  numberOfSimilar <-sum(cell$value==neigh$value, na.rm = T)  
  return(numberOfSimilar)
}


globalSimilarityPct <-  function(state){
  countingNeigh <- function(id,state){
    return(nrow(getneighborsByID(id, state)))
  }
  nbSimilar <-  sapply(state$ID, FUN = getSimilarNeighborsNumber , state=state)
  nbNeigh <-  sapply(state$ID, FUN= countingNeigh , state=state)
  
  
  return(sum(nbSimilar, na.rm = T) / sum(nbNeigh, na.rm = T))
}



globalunHappyPct <-  function(state){
  populationSize  <-  nrow(state %>%  filter(!is.na(value)))
  sads <- nrow(state %>%  filter(!happy))
  return(sads/populationSize )
}


theilEntropy <-  function(state){
  return(ineq(state$value, type = "Theil", na.rm = T))
}

theilMano <-  function(state){
  state <-  na.omit(state)
  mu <-  mean(state$value)
  TE <-  sum(state$value / mu  * log(state$value / mu ))
  
  
  return(TE / nrow(state))
} 



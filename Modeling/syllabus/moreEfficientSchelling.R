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


matrixState <-  matrixSchellingState(40,0.5,0.5, 0.5)
meltedState <- melt(matrixState, varnames = c("X","Y")) 
head(meltedState,10)  #to see what it looks like and get the column names


colorMapping <- c("1"="red", "2"="blue") 

myggplot <- ggplot(meltedState, aes(x=X, y=Y)) +
  geom_raster(aes(fill=factor(value)))+
  scale_fill_manual(values = colorMapping, na.value="lightgrey")+
  theme_void()
myggplot 


fancyColors <- c("1"="bisque", "2"="aquamarine") 

myggplot2 <- ggplot(meltedState, aes(x=X, y=Y)) +
  geom_point(size=2, aes(colour=factor(value)))+ #size has to be adjusted regarding the size of the state
  scale_color_manual( values=fancyColors, name="Group" )+
  theme_void()+
  coord_fixed(ratio = 1) + 
  ggtitle("Schelling state")

myggplot2            

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


tolerance <- 0.3  

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

testEnv <-  matrixSchellingState(20,pctOccupied = 0.7)
meltTestEnv <-  melt(testEnv,varnames = c("X","Y"))
displayState(meltTestEnv, dotSize = 3)


updateHappiness <- function(state, tolerance){
  state$happy <-  sapply(state$ID, FUN = isHappybyID, state=state, tolerance = 0.3)
  return(state)
}

meltTestEnv$ID <-  seq.int(nrow(meltTestEnv))
meltTestEnv <-  updateHappiness(meltTestEnv, tolerance)

str(meltTestEnv)

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
displayStateUnhappy(meltTestEnv)



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
emptyCells <-  getEmptyCells(meltTestEnv)

#we add a symbol layer  to check emptyness correction  visually
pp <-  displayStateUnhappy(meltTestEnv) +
  geom_text(data = subset(meltTestEnv, is.na(value) ), aes(X,Y,label = sprintf("\U2205")), angle=-90)  
pp


#we create another test environemment and get rid of the previous
rm(meltTestEnv)
stateEnv <- createMeltedSchellingState(10,pctOccupied = 0.8, tolerance=0.3)

displayStateUnhappy(stateEnv)
unhappy <-  getUnhappy(stateEnv)
numberOfUnHappy <-  nrow(unhappy)
emptyCells <-  getEmptyCells(stateEnv)
numberOfEmpty <-  nrow(emptyCells)
cat(numberOfEmpty , "\n") # has to be 20% of state size squared since pctOccupied is 80%



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



rm(stateEnv)
rm(s)
s <- createMeltedSchellingState(30,pctOccupied = 0.8, tolerance=0.3)
displayStateUnhappy(s, dotsize = 4)
s<- simulate(2000,s, 0.3)
cat(nrow(getUnhappy(s)))
displayStateUnhappy(s, dotsize = 4)



############## extension for several groups



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




### testing mulit group dynamics

nbgroups <-  4
groupValues <-  seq.int(from = 1, to=nbgroups)
colorMapping <-  brewer.pal(nbgroups,"Dark2")
pctList <- c(0.25, 0.3, 0.3, 0.15)
pctList %>%  sum




tolerance <-  0.4
s <- createMultiGroupState(40, 0.6, pctList, groupValues, tolerance)
displayStateMG(s, dotSize = 2,colorMapping)
s <- simulate(4000, s, 0.3)
cat(nrow(getUnhappy(s)))
displayStateUnhappyMG(s, dotsize = 4, colorMapping)



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
globalSimilarityPct(s)



globalunHappyPct <-  function(state){
  populationSize  <-  nrow(state %>%  filter(!is.na(value)))
  sads <- nrow(state %>%  filter(!happy))
  return(sads/populationSize )
}
globalunHappyPct(s)


theilEntropy <-  function(state){
  return(ineq(state$value, type = "Theil", na.rm = T))
}

theilMano <-  function(state){
  state <-  na.omit(state)
  mu <-  mean(state$value)
  TE <-  sum(state$value / mu  * log(state$value / mu ))
  
  
   return(TE / nrow(state))
} 



# test measures on dumb examples

pctListPerfectEqual <-  c(1,0,0,0)
sPerfectEqual <-  createMultiGroupState(50, 0.8, pctListPerfectEqual,groupValues,tolerance = 0.3)
displayStateUnhappyMG(sPerfectEqual,dotsize = 1,colorMapping)

theilEntropy(sPerfectEqual)
theilMano(sPerfectEqual)


pctListHomogen <- c(0.25,0.25,0.25,0.25)
sHomogen <-  createMultiGroupState(50,0.8,pctListHomogen, groupValues , tolerance = 0.3)
displayStateUnhappyMG(sHomogen, dotsize=1, colorMapping)

theilEntropy(sHomogen)
theilMano(sHomogen)




















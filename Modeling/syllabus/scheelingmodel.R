library(reshape2)
library(ggplot2)
library(seg)
library(dplyr)
library(sf)
library(shiny)

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


getneighbors <- function(state, i,j){
  neighborhood <- meAndMyNeighbors(state, i, j)
  #exclude the caller cell
  neighborhood <- neighborhood[!(neighborhood$X==i & neighborhood$Y==j),]
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



testEnv <-  matrixSchellingState(5,pctOccupied = 0.8)
meltTestEnv <-  melt(testEnv,varnames = c("X","Y"))
displayState(meltTestEnv, dotSize = 5)



#we apply the function isHappy to create a newcolumn of happiness in the state
meltTestEnv <- meltTestEnv %>% rowwise() %>% mutate(happy= isHappy(., X, Y, tolerance = 0.3))


# we update the display function code to display unhappiness
displayStateUnhappy <- function(meltedState, dotsize = 5){
  pp <- displayState(meltedState, dotSize = dotsize) + 
    geom_text(data = subset(meltedState, !happy ), aes(X,Y,label = ":-|"), angle=-90, size= dotsize) 
  return(pp)
}
displayStateUnhappy(meltTestEnv)




# save as function for later
updateHappiness <- function(state, tolerance){
  state <- state %>% rowwise() %>% mutate(happy= isHappy(., X, Y, tolerance ))
}

getUnhappy <-  function(state){
  return(filter(state, !happy))
}
#melted state constructor
createMeltedSchellingState <- function(size, pctOccupied=.5, pctRed=0.5, pctBlue=1-pctRed, tolerance){
  matState <-  matrixSchellingState(size, pctOccupied, pctRed, pctBlue)
  meltedState <-  melt(matState,varnames = c("X","Y"))
  meltedState <-  updateHappiness(meltedState, tolerance  )
  # we add an explicit ID of cells (row number), it will be useful
  meltedState$ID <- seq.int(nrow(meltedState))
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
  uhhhvalue = uhhh$value
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
  state[state$ID==emptyID,"value"] <- uhhhvalue
  state[state$ID==uhhhID,"value"] <- NA
  
  #moving changes happiness, so we update it
  state <-  updateHappiness(state, tolerance )
  
  return(state)
}


step <- function(state, tolerance){
  unhappy <-  getUnhappy(state)
  if (nrow(unhappy) == 0){
    cat("Everybody is fine with their location\n")
    return(state)
  }
  state <- moveOne(state,tolerance)
  return(state)
}


simulate <-  function(steps, state, tolerance){
  for (i in 1:steps){
    cat("step : ", i , "\n")
    state <-  step(state, tolerance)
  }
  return(state)
}



rm(stateEnv)
rm(s)
s <- createMeltedSchellingState(30,pctOccupied = 0.6, tolerance=0.3)
str(s)
displayStateUnhappy(s, dotsize = 2)
s<- simulate(10,s, 0.3)
cat(nrow(getUnhappy(s)))
displayStateUnhappy(s)




shinyApp(
  ui = fluidPage(
    actionButton("go", "one step"),
    actionButton("go100", "100 step"),
    
    plotOutput("plot")
  ),
  
  server = function(input, output) {
    tolerance <-  0.3

    state <-createMeltedSchellingState(20, 0.8, 0.5, 0.5,tolerance)  
    currentState <-  reactiveValues()
    
    observeEvent(input$go, {
      currentState$data <- step(state,tolerance)
    })
    observeEvent(input$go100, {
      currentState$data <-  simulate(100, state, tolerance)
    })
    
    
    
    output$plot <- renderPlot({
      currentplot <-  displayStateUnhappy(currentState())
      currentplot
    })  }
  ,
  options = list(height = 500)
)



getSimilarNeighborsNumber <-  function(state, i, j){
  #cell <-  filter(state, X==i & Y==j)
  cell <-  state[state$X==i & state$Y==j, ]
  neigh <- getneighbors(state,i,j) 
  numberOfSimilar <-sum(cell$value==neigh$value, na.rm = T)  
  return(numberOfSimilar)
}


globalSimilarityPct <-  function(state){
  #res <-  state %>% rowwise() %>% transmute(ratioSim = nrow(getneighbors(.,X,Y)) / nrow(getSimilarNeighbors(.,X,Y)))
  res <-  state %>% rowwise() %>% transmute(nbSimNeigh = getSimilarNeighborsNumber(.,X,Y) , nbNeigh = nrow(getneighbors(.,X,Y)))
  
  return(sum(res$nbSimNeigh) / sum(res$nbNeigh))
}
globalSimilarityPct(stateEnv)





globalunHappyPct <-  function(state){
populationSize  <-  nrow(state %>%  filter(!is.na(value)))
sads <- nrow(state %>%  filter(!happy))
return(sads/populationSize * 100)
}
globalunHappyPct(stateEnv)






#Duncan segregation index

nG1 <-  stateEnv %>% filter(value==1) %>%  nrow 
nG2 <-  stateEnv %>% filter(value==2) %>%  nrow 

mysum <- 0
for (cellIterator in stateEnv$ID){
currentcellvalue <-  stateEnv$value[stateEnv$ID == cellIterator]
if( !is.na(currentcellvalue)){
mysum <- mysum + abs( ifelse(currentcellvalue==1  , 1/nG1, 1/nG2)) 
}
}
myDSI <-  mysum * 0.5




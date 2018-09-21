
library(fitdistrplus)
library(dplyr)
library(cancensusHelpers)
library(sf)
#library(MLID)


options(cancensus.api_key = "CensusMapper_5d949dee7f3e2546720b77d6ef08072e")

#Code for CSD in census
ct <-  "35535"
cm  <-  "24462"
cv  <-  "59933"

#Code for central municipality in census
cmt <-  "3520005"
cmv <- "5915022"
cmm <- "2466023"

#Coordinates of city hall
cht  <-  c(-79.5257363, 43.6148863)
chv  <-  c(-123.2309412, 49.2219987)
chm  <-  c(-73.7171664, 45.4726094)

#Critical distance for AirBnb
bdtv <- 10000
bdm <-  12000


list_census_vectors("CA16")


## incomes appear to be the v_CA16_2201 vector 

incomesTor <-  get_census("CA16", regions=list(CSD=ct), 
           vectors = "v_CA16_2201", level = "CT",
           labels = "short", geo_format = "sf")

names(incomesTor) <- c("Shape Area","Type","Dwellings","Households","GeoUID","Population","Adjusted Population (previous Census)","PR_UID","CMA_UID","CSD_UID","CD_UID","Region Name","Area (sq km)","income","geometry") 


incomesTor <-  incomesTor %>% na.omit()


incomes <-  incomesTor$income


#fit the distribution
fittingLogNorm <- fitdist(data = incomes, distr = "lnorm")
# assess goodnees of fot 
cdfcomp(fittingLogNorm)
dev.off()


meanLog <-  fittingLogNorm$estimate[1]
sdLog <-  fittingLogNorm$estimate[2]


censusIncomeSampler <-  function(n, incomes){
  fittingLogNorm <- fitdist(data = incomes, distr = "lnorm")
  meanLog <-  fittingLogNorm$estimate[1]
  sdLog <-  fittingLogNorm$estimate[2]
  mysample <-  round(rlnorm(n,meanLog, sdLog))
  return(mysample)
}







############################



desiredMean <- 7
desiredSd <- 75
location <- log(desiredMean^2 / sqrt(desiredSd^2 + desiredMean^2))
shape <- sqrt(log(1 + (desiredSd^2 / desiredMean^2)))

lognormalDraws <- rlnorm(n=10000, location, shape)








library(dplyr)
library(sf)
library(ggplot2)
library(ineq)
library(cancensusHelpers)

load("cityTractsWithPopAndVacancy.RData")

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



## Identifying the vectors for visible Minority status
parent_vector <- "v_CA16_3954"
minorities <- list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_3954") %>% 
  child_census_vectors(leaves_only = TRUE) %>% 
  pull(vector)

minority_vectors <- c(parent_vector, minorities)

mino <-  get_census("CA16", regions=list(CSD=ct), 
                          vectors = minority_vectors, level = "CT",
                          labels = "long", geo_format = "sf")




#tract toronto 
cma.ct <- get_census("CA16", regions=list(CMA=cmt), 
                     vectors = minority_vectors, level = "CT",
                     labels = "long", geo_format = NA)


# Calculating diversity (Theil's E)
# For every variable, divide by v_CA16_3999 and multiply times the logged inverse proportion, then
# take the sum for each tract. With 14 different groups, the max entropy is ln(14) = 2.64
base_pop <- quo(v_CA16_3954)
cma.ei <- cma.ct %>% 
  group_by(GeoUID) %>% 
  mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
  select(GeoUID, ends_with("_E")) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
  mutate(Ei = rowSums(select(.,-1), na.rm = FALSE)) %>% 
  select(GeoUID, Ei)

cma.eicsd <- cma.csd %>% 
  group_by(GeoUID) %>% 
  mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
  select(GeoUID, ends_with("_E")) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
  mutate(Ei = rowSums(select(.,-1), na.rm = FALSE)) %>% 
  select(GeoUID, Ei)

# Join with geography

cma.geo <- get_census_geometry("CA16", regions=list(CMA=cma), 
                               level = "CT", geo_format = "sf")

cma.csd.geo <- get_census_geometry("CA16", regions=list(CMA=cma), 
                                   level = "CSD", geo_format = "sf")

# Which Census Tracts are in which CSD

cma.ct <- cma.geo %>% 
  left_join(cma.ei) %>% 
  mutate()

cma.csd <- cma.csd.geo %>% 
  left_join(cma.eicsd) %>% 
  mutate()







probRoom <-  function(distCityHall){
  nbListing <-  exp((-1.85 * log(distCityHall)+ 11.47) )
  if(nbListing > 0){
   return( 1 / nbListing)
  }
  return(0)
}

priceFixer <-  function(tract){
  
}



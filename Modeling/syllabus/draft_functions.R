setwd("~/GitHub/R-workshops/Modeling/syllabus")

load("cityTractsWithPopAndVacancy.RData")



library(githubinstall)
library(rmapzen)
library(leaflet)
library(hrbrthemes)
library(devtools)
library(ggrepel)
library(sf)
library(cancensus)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(ggplot2)
library(scales)
library(segregation)
library(seg)
devtools::install_github("profrichharris/MLID")
install_github("mountainMath/cancensusHelpers")


library(cancensusHelpers)
library(MLID)



options(cancensus.api_key = "CensusMapper_5d949dee7f3e2546720b77d6ef08072e")
ct = "35535"
cm = "24462"
cv = "59933"

cht = c(-79.5257363, 43.6148863)
chm = c(-123.2309412, 49.2219987)
chv = c(-73.7171664, 45.4726094)

f = list.files('insideairbnb/')

mont <- read.csv(paste0('insideairbnb/', f[1]))
toro <- read.csv(paste0('insideairbnb/', f[2]))
vanc <- read.csv(paste0('insideairbnb/', f[3]))

toronto = unique(toro$city)
vancouver = unique(vanc$city)
montreal = unique(mont$city)

df = rbind(mont, toro, vanc)

head(toro)



l <- levels(df$property_type)
lookup = data.frame('type' = 1:length(l))
lookup$type <- as.factor(l)
lookup$property_group <- c(
  # [1] "Aparthotel"             "Apartment"              "Bed and breakfast"      "Boat"                   "Boutique hotel"         "Bungalow"               "Cabin"                 
  'hotel', 'home', 'hotel', 'other', 'hotel', 'home', 'other',
  #  [8] "Camper/RV"              "Campsite"               "Casa particular (Cuba)" "Cave"                   "Chalet"                 "Condominium"            "Cottage"               
  'other', 'other', 'home', 'other', 'home', 'home', 'home',
  # [15] "Farm stay"              "Guest suite"            "Guesthouse"             "Hostel"                 "Hotel"                  "House"                  "Houseboat"             
  'home', 'home', 'hotel', 'hotel', 'hotel', 'home', 'home',
  # [22] "Hut"                    "Loft"                   "Nature lodge"           "Other"                  "Serviced apartment"     "Tent"                   "Timeshare"             
  'other', 'home', 'other', 'other', 'hotel', 'other', 'other',
  # [29] "Tiny house"             "Townhouse"              "Villa"                  "Barn"                   "Castle"                 "Dorm"                   "Earth house"           
  'home', 'home', 'home', 'home', 'home', 'hotel', 'other',
  # [36] "In-law"                 "Parking Space"          "Treehouse"              "Resort"            
  'home', 'other', 'other', 'hotel'
)

df = data.frame(df,lookup[match(df$property_type, lookup$type),] )
# dfh = subset(df, property_group == 'home' & as.character(df$host_neighbourhood) == as.character(df$neighbourhood) & df$room_type != "Shared room")
dfh = subset(df, property_group == 'home' & as.character(df$host_neighbourhood) == as.character(df$neighbourhood))
dfh$property_group <- NULL
dfhu = dfh[!duplicated(dfh$host_id),]
dim(dfh)[1] - dim(df)[1]
dim(dfhu)[1] - dim(dfh)[1]


dfhu$year = as.numeric(substr(dfhu$last_review, 1, 4))
dfhun = subset(dfhu, year >= 2017)
dim(dfhun)[1] - dim(dfhu)[1]


dfhun$numPrice <- as.numeric(gsub("[$]",'',dfhun$price))
summary(dfhun$room_type)
final = subset(dfhun, room_type != "Shared room")

final$rooms = ifelse(final$bedrooms == 0, 1, final$bedrooms)
final$priceperroom = as.numeric(ifelse(final$room_type == T,  final$numPrice,  final$numPrice / final$rooms))


dim(final) / dim(df)



city = "toronto"


if(city == "toronto"){
citynames = toronto
censuscode = ct
cityhall = cht
}
if(city == "vancouver"){
citynames = vancouver
censuscode = cv
cityhall = chv
}
if(city == "montreal"){
citynames = montreal
censuscode = cm
cityhall = chm
}

#############aggregate airbnb into vancouver tracts.
csd.csd.geo <- get_census_geometry("CA16", regions=list(CSD=cv), 
                                   level = "CSD", geo_format = "sf")
csd.geo <- get_census_geometry("CA16", regions=list(CSD=cv), 
                               level = "CT", geo_format = "sf")
unique(csd.csd.geo$name)
geoid = as.character(csd.csd.geo[csd.csd.geo$name == "Vancouver (CY)","GeoUID"])[1]
vancouver_tr = csd.geo[csd.geo$CSD_UID == geoid,]
head(vancouver_tr)
vancouver_tr$vacancy = (vancouver_tr$Dwellings - vancouver_tr$Households) / vancouver_tr$Dwellings
summary(vancouver_tr$vacancy)  
plot(vancouver_tr)
####################################################



city_data = subset(final, city %in% citynames)

## Identifying the vectors for visible Minority status
parent_vector <- "v_CA16_3954"
minorities <- list_census_vectors("CA16") %>% 
filter(vector == "v_CA16_3954") %>% 
child_census_vectors(leaves_only = TRUE) %>% 
pull(vector)

NotVisibleMinority = "v_CA16_3996"

minority_vectors <- c(parent_vector, minorities)


cma.ct <- get_census("CA16", regions=list(CMA=censuscode), 
vectors = minority_vectors, level = "CT",
labels = "short", geo_format = NA)
cma.csd <- get_census("CA16", regions=list(CMA=censuscode), 
vectors = minority_vectors, level = "CSD",
labels = "short", geo_format = NA)

csd.geo <- get_census_geometry("CA16", regions=list(CSD=censuscode), 
level = "CT", geo_format = "sf")

csd.csd.geo <- get_census_geometry("CA16", regions=list(CSD=censuscode), 
level = "CSD", geo_format = "sf")


pal <- colorQuantile(
palette =  'Blues',
domain = city_data$priceperroom,
n = 10
)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
addPolygons(
data = csd.csd.geo,
color = 'black',
fill = F,
weight = 0.7,
opacity = 0.9
) %>% addPolygons(
data = csd.geo,
color = 'grey',
fill = F,
weight = 0.4
) %>%
addCircleMarkers(
data = city_data,
radius = ~ sqrt(4 * rooms),
lat = ~ latitude,
fillColor = ~ pal(priceperroom),
color = 'black',
stroke = T,
fillOpacity = 0.5,
weight = 0.1,
layerId = ~ id,
lng = ~ longitude
) %>% 
addLegend(pal = pal, position = 'topleft', values = city_data$priceperroom)

map


diversity_index <- function(cma) {

cma.ct <- get_census("CA16", regions=list(CMA=cma), 
vectors = minority_vectors, level = "CT",
labels = "short", geo_format = NA)

# Calculating diversity (Theil's E)
# For every variable, divide by v_CA16_3999 and multiply times the logged inverse proportion, then
# take the sum for each tract. With 14 different groups, the max entropy is ln(14) = 2.64
base_pop <- quo(v_CA16_3954)
cma.ei <- cma.ct %>% 
  group_by(GeoUID) %>% 
  mutate_at(minorities, funs(S = ./!!base_pop)) %>%
  mutate_at(vars(ends_with("_S")), funs(E = -.*(log(.)))) %>%
  select(GeoUID, ends_with("_S_E")) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("_S_E")), funs(ifelse(is.nan(.),0,.))) %>% 
  mutate(Ei = rowSums(select(.,-1), na.rm = FALSE)) %>% 
  select(GeoUID, Ei)

# Join with geography

cma.geo <- get_census_geometry("CA16", regions=list(CMA=cma), 
                               level = "CT", geo_format = "sf")
cma.ct <- cma.geo %>% 
  left_join(cma.ei) %>% 
  mutate()

return(cma.ct)
}

tractTable <- diversity_index(censuscode)


pal <- colorQuantile(
  palette =  'Blues',
  domain = city_data$priceperroom,
  n = 10
)
pal2 <- colorQuantile(
  palette =  'Reds',
  domain = tractTable$Ei,
  n = 10)

map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = csd.csd.geo,
    color = 'black',
    fill = F,
    weight = 0.7,
    opacity = 0.9
  ) %>% addPolygons(
    data = tractTable,
    color =  ~ pal2(Ei),
    fill = ~ pal2(Ei),
    weight = 0.4
  ) %>%
  #addCircleMarkers(
  #   data = city_data,
  #       radius = ~ sqrt(4 * rooms),
  #   lat = ~ latitude,
  #   fillColor = ~ pal(priceperroom),
  #   color = 'black',
  #   stroke = T,
  #   fillOpacity = 0.5,
  #  weight = 0.1,
  #   layerId = ~ id,
  #   lng = ~ longitude
# ) %>% 
# addLegend(pal = pal, position = 'topleft', values = city_data$priceperroom)%>% 
addLegend(pal = pal2, position = 'topleft', values = tractTable$Ei)

map



cma.eict <- tractTable

cma.csd <- get_census("CA16", regions=list(CMA=censuscode), 
                      vectors = minority_vectors, level = "CSD",
                      labels = "short", geo_format = NA)

cma.eicsd <- cma.csd %>%
  group_by(GeoUID,`Region Name`, Population) %>%
  mutate_at(minorities, funs(S = ./!!base_pop)) %>%
  mutate_at(vars(ends_with("_S")), funs(E = -.*(log(.)))) %>%
  select(GeoUID, `Region Name`, Population, ends_with("_S_E")) %>% 
  ungroup() %>%
  mutate_at(vars(ends_with("_S_E")), funs(ifelse(is.nan(.),0,.))) %>%
  mutate(Ei = rowSums(select(.,-c(1,2,3)), na.rm = FALSE)) %>%
  mutate(CMA = censuscode) %>%
  select(CMA, GeoUID, `Region Name`, Population, Ei)

dfseg <- data.frame(cma.eict, cma.eicsd[match(cma.eict$CSD_UID, cma.eicsd$GeoUID),])

cma.h <- dfseg  %>%
  select(GeoUID, CSD_UID, name = `Region.Name`, ctpop = Population,
         csdpop = Population.1, ctei = Ei, csdei = Ei.1)  %>%
  filter(csdpop > 1000) %>%
  group_by(CSD_UID,GeoUID) %>%
  #  mutate_at(csdei, funs(ifelse(is.nan(.),0,.))) %>%
  mutate(smallh = ifelse(csdei == 0, NA, (ctpop*(csdei - ctei))/(csdei*csdpop)) ) %>%
  ungroup() %>%
  group_by(CSD_UID, csdei, name, csdpop) %>%
  mutate(smallhp = ifelse(is.na(smallh), 0, smallh)) %>%
  summarise(H = sum(smallhp)) %>% 
  mutate(cma = censuscode)

cma.csd <- get_census("CA16", regions=list(CMA=censuscode), 
                      vectors = minority_vectors, level = "CSD",
                      labels = "short", geo_format = NA)
# 
#    segdata <- cma.csd %>% 
#     left_join(cma.h,  by = c("CSD_UID"="GeoUID")) %>% 
#     mutate()
#   


pal <- colorQuantile(
  palette =  'Blues',
  domain = city_data$priceperroom,
  n = 10
)
pal2 <- colorQuantile(
  palette =  'Reds',
  domain = cma.h$H,
  n = 10)

#    map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
#       addPolygons(
#       data = csd.csd.geo,
#       color = 'black',
#       fill = F,
#       weight = 0.7,
#       opacity = 0.9
#     ) %>% addPolygons(
#       data = cma.h,
#       color =  ~ pal2(H),
#       fill = ~ pal2(H),
#        weight = 0.4
#     ) %>%
#   #addCircleMarkers(
#   #   data = city_data,
#   #       radius = ~ sqrt(4 * rooms),
#   #   lat = ~ latitude,
#   #   fillColor = ~ pal(priceperroom),
#   #   color = 'black',
#   #   stroke = T,
#   #   fillOpacity = 0.5,
#   #  weight = 0.1,
#   #   layerId = ~ id,
#   #   lng = ~ longitude
#   # ) %>% 
# # addLegend(pal = pal, position = 'topleft', values = city_data$priceperroom)%>% 
#   addLegend(pal = pal2, position = 'topleft', values = cma.h$H,n = 10)
#  
#    map


###### income from https://github.com/mountainMath/doodles/blob/master/content/posts/2017-09-14-income-a-first-look.Rmarkdown
dataset='CA16'
level="CSD"

labels=list("v_CA16_2207"="Individual Total",
            "v_CA16_2213"="Individual After Tax",
            "v_CA16_2219"="Individual Market",
            "v_CA16_2231"="Individual Employment",
            "v_CA16_2397"="Household Total",
            "v_CA16_2398"="Household After Tax",
            "v_CA16_2400"="One person Household Total",
            "v_CA16_2401"="One person Household After Tax",
            "v_CA16_2403"="Two+ person Household Total",
            "v_CA16_2404"="Two+ person Household After Tax",
            "v_CA16_2447"="Family Total",
            "v_CA16_2448"="Family After Tax",
            "v_CA16_2451"="Couples w/o Children Total",
            "v_CA16_2452"="Couples w/o Children After Tax",
            "v_CA16_2455"="Couples with Children Total",
            "v_CA16_2456"="Couples with Children After Tax",
            "v_CA16_2459"="Lone Parent Total",
            "v_CA16_2460"="Lone Parent After Tax",
            "v_CA16_2465"="Unattached Total",
            "v_CA16_2468"="Unattached After Tax"
)
currency_format <- function(x){return(paste0("$",format(x,big.mark = ",")))}
currency_format_short <- function(d){return(paste0("$",d/1000,"k"))}
data_for <- function(region,vector){
  return(filter(data,grepl(region,`Region Name`))[[vector]] %>% currency_format)
}


median_income_vectors <- list_census_vectors(dataset, quiet=TRUE) %>% 
  filter(type=="Total",grepl("Median",label),grepl("income",label)) %>% pull("vector") 
regions <- list_census_regions(dataset) %>% filter(level==!!level) %>% top_n(10,pop) %>% as_census_region_list

data <- get_census(dataset = 'CA16',
                   level="Regions",
                   vectors=median_income_vectors , 
                   regions=regions, 
                   geo_format = NA,
                   labels='short')

plot_data <- data %>% select(c("Region Name",median_income_vectors)) %>% 
  reshape2::melt(id="Region Name") %>%
  mutate(`Region Name` = factor(`Region Name`, 
                                levels = data %>% arrange(desc(Population)) %>% pull("Region Name"),
                                ordered=TRUE)) 

plot_data$var <- factor(as.character(labels[plot_data$variable]),levels=as.character(labels),ordered=TRUE)

ggplot(plot_data , aes(x = `Region Name`, y = value, fill=`Region Name`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~var) +
  scale_y_continuous(labels=currency_format_short) +
  labs(fill = paste0("Top 10 ",level,"s by Population"), 
       y = "Median Income",
       x="",
       title="Median Income 2015 Various Statistics",
       caption="Canada Census 2016 via cancensus & CensusMapper.ca") +
  theme_bw() + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  coord_flip()

######### distribution 

show_col(viridis_pal()(10))

scale_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                 direction = 1, option = "D", aesthetics = "fill") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}
year = 2016
graph_distributions_for_year <- function(year) {
  level="CSD"
  topn=15
  dataset=paste0('CA',substr(paste0(year),3,4))
  search_string_list=list('CA06'="Household income in 2005 of private households",
                          'CA11'="Household total income in 2010 of private households",
                          'CA16' = "Total - Household total income groups in 2015 for private households - 100% data")
  options_list=list('CA06'='magma','CA11'='plasma','CA16'='viridis')
  search_string=search_string_list[dataset]
  hh_distribution <- search_census_vectors(search_string,dataset, quiet=TRUE) %>% 
    child_census_vectors(leaves_only=TRUE)
  
  data <- get_census(dataset = dataset,
                     level=level,
                     vectors=hh_distribution %>% pull("vector"), 
                     regions=list(CMA="59933"),
                     geo_format = NA,
                     labels='short',
                     quiet=TRUE) %>% top_n(topn,Population)
  
  # Rename census variables with the more detailed names above
  categories <- label_vectors(data)
  names(data)[grep("v_", names(data))] <- categories$Detail
  
  categories <- hh_distribution %>% pull("label")
  region_list <- data %>% arrange(UQ(as.name(categories[length(categories)]))/Households) %>% pull("Region Name")
  
  cat_list <- factor(categories, ordered = TRUE)
  
  
  # We want to use ggplot to plot this, and ggplot requires data in a long format. Reshape and gather data using tidyr::gather
  plot_data <- data %>% tidyr::gather(key = `Income Bracket`, value = Count, as.character(cat_list))
  
  # set correct order on factors 
  plot_data$`Income Bracket` <- factor(plot_data$`Income Bracket`,levels=rev(cat_list), ordered = TRUE) 
  
  plot_data <- plot_data %>% select("Region Name","Income Bracket","Count")
  plot_data$`Region Name` <- factor(plot_data$`Region Name`,levels=rev(region_list), ordered=TRUE)
  
  ggplot(plot_data %>% arrange(`Region Name`), aes(x = `Region Name`, y = Count, group = `Income Bracket`, fill=`Income Bracket`)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = scales::percent) + 
    labs(x = paste0(topn," largest ",level," in Metro Vancouver"), y = "Household Total Income", title="Household Total Income Distribution",caption=paste0("Canada Census ",year," via cancensus & censusmapper.ca")) +
   #scale_fill_brewer(palette = "Blues") +
    scale_fill_viridis_d("Income Bracket",option=as.character(options_list[dataset])) +
    coord_flip() +
    theme_bw()
  #ggsave(paste0("~/Desktop/income_dist_",level,"_",year,"_vancouver.png"),height=7,width=10)
}
graph_distributions_for_year(2016)


data(segdata)
grd <- GridTopology(cellcentre.offset=c(0.5,0.5),
                    cellsize=c(1,1), cells.dim=c(10,10))
grd.sp <- as.SpatialPolygons.GridTopology(grd)
test.df <- segdata[,1:2]

head(test.df)
# no spatial smoothing
xx1 <- spseg(grd.sp, data = test.df)
print(xx1, digits = 3)

#############aggregate airbnb into toronto tracts.
geoid = as.character(csd.csd.geo[csd.csd.geo$name == "Toronto (C)","GeoUID"])[1]
toronto_tr = csd.geo[csd.geo$CSD_UID == geoid,]
head(toronto_tr)
toronto_tr$vacancy = (toronto_tr$Dwellings - toronto_tr$Households) / toronto_tr$Dwellings
summary(toronto_tr$vacancy)  

#save(toronto_tr, vancouver_tr, file="cityTractsWithPopAndVacancy.RData")

library(rgdal)
rbnb_pts <- city_data 
coordinates(rbnb_pts) <- ~longitude+latitude
toronto_tracts <- as_Spatial(toronto_tr)
rbnb_pts@proj4string <-CRS("+proj=longlat +datum=WGS84")
toronto_tracts@proj4string <-CRS("+proj=longlat +datum=WGS84")
plot(toronto_tracts, border = "grey")
points(rbnb_pts, col = "red", cex = 0.2, pch = 16)

class(toronto_tracts)
class(rbnb_pts)

library(Hmisc)
library(rgeos)
PTS <- as(rbnb_pts, "sf")
POLY <- as(toronto_tracts, "sf")
idata <- st_intersection(PTS, POLY)
colnames(idata)
rbnbPerTract <- idata %>%
  #group_by(CSD_UID) %>%
  count(GeoUID, sort = TRUE) %>%
  select(GeoUID, n)
listingPerCapita <- 1

listingCounts = as.data.frame(rbnbPerTract[,1:2])
toronto_tracts@data <- data.frame(toronto_tracts@data,
         listingCounts[match(toronto_tracts@data$GeoUID,
                             listingCounts$GeoUID),])
toronto_tracts@data <- data.frame(toronto_tracts@data,
         tractTable[match(toronto_tracts@data$GeoUID,
                          tractTable$GeoUID),])

Mino = as.data.frame(cma.ct[,c("GeoUID", minorities)])

toronto_tracts@data <- data.frame(toronto_tracts@data,
                                  Mino[match(toronto_tracts@data$GeoUID,
                                             Mino$GeoUID),])

toronto_tracts@data$ListingPerCapita = toronto_tracts@data$n / toronto_tracts@data$Population
toronto_tracts@data <- toronto_tracts@data %>% 
  mutate_at(minorities, funs(Share = ./ Population) )

summary(toronto_tracts@data)
centroidsToronto = gCentroid(toronto_tracts,byid=TRUE)
plot(centroidsToronto, add = T, pch = 16, cex = 0.6)

listMinoShares = paste0(minorities, "_Share")
 
EstimatorAirbnbPresence = cbind(toronto_tracts@data[,c("GeoUID", "ListingPerCapita", "Ei", 
                                                       listMinoShares)],
                    coordinates(centroidsToronto))

head(EstimatorAirbnbPresence)
library(geosphere)

DistCityHall <- distm(EstimatorAirbnbPresence[,c('x','y')], 
                      cityhall, fun=distVincentyEllipsoid)
EstimatorAirbnbPresence = cbind(EstimatorAirbnbPresence, DistCityHall)

head(EstimatorAirbnbPresence)

hist(EstimatorAirbnbPresence$ListingPerCapita)


ggplot(EstimatorAirbnbPresence, aes(x = DistCityHall, y = ListingPerCapita)) +
  geom_point() + geom_smooth() + scale_x_log10() + scale_y_log10() +
  geom_vline(xintercept = 10000, col = "orange", cex = 1)

# mean value of listing per capita of tracts between 0 & 10 km: 
central <- EstimatorAirbnbPresence %>%
  filter(DistCityHall <= 10000, !is.na(ListingPerCapita)) %>%
  select(ListingPerCapita, DistCityHall, Ei, listMinoShares)

central %>%
  summarise(mean(ListingPerCapita))


# regression listing per capita of tracts above 10 km: 
peripheral <- EstimatorAirbnbPresence %>%
  filter(DistCityHall > 10000, !is.na(ListingPerCapita)) %>%
  select(ListingPerCapita, DistCityHall, Ei, listMinoShares)

summary(lm(log(ListingPerCapita) ~ log(DistCityHall), data = peripheral))


lab = as.data.frame (list_census_vectors('CA16') %>% 
                       filter(vector %in% minorities) %>% 
                       select(vector, label))



for (sample in c("all", "central", "peripheral")){
  if (sample == "all") df = EstimatorAirbnbPresence
  if (sample == "central") df = central
  if (sample == "peripheral") df = peripheral
  
  f = paste0("log(ListingPerCapita) ~ log(DistCityHall) + ",
             paste(listMinoShares, collapse = " + "))
  print(paste0("obs = ", dim(df)[1]))
  print(summary(lm(formula(f), data = df)))
}


library(lme4)
#### TO DO
# add names of variable to regression results
# report tract val to airbnb table
# regress value of listing with variable (lmer)
# segregation indices : moran/duncan/entropy for minorities
# reardon for airbnb
# markdown ok
# slides


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
library(rgdal)
library(Hmisc)
library(rgeos)
library(geosphere)
library(gridExtra)
#library(segregation)
#library(seg)
#devtools::install_github("profrichharris/MLID")
install_github("mountainMath/cancensusHelpers")
library(cancensusHelpers)
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

f  <-  list.files('insideairbnb/')

mont <- read.csv(paste0('insideairbnb/', f[1]))
toro <- read.csv(paste0('insideairbnb/', f[2]))
vanc <- read.csv(paste0('insideairbnb/', f[3]))

toronto  <-  unique(toro$city)
vancouver <-  unique(vanc$city)
montreal  <-  unique(mont$city)

df <-  rbind(mont, toro, vanc)


l <- levels(df$property_type)
lookup  <-  data.frame('type' = 1:length(l))
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

df <-  data.frame(df,lookup[match(df$property_type, lookup$type),] )
# dfh = subset(df, property_group == 'home' & as.character(df$host_neighbourhood) == as.character(df$neighbourhood) & df$room_type != "Shared room")
dfh  <-  subset(df, property_group == 'home' & as.character(df$host_neighbourhood) == as.character(df$neighbourhood))
dfh$property_group <- NULL
dfhu  <-  dfh[!duplicated(dfh$host_id),]
dim(dfh)[1] - dim(df)[1]
dim(dfhu)[1] - dim(dfh)[1]


dfhu$year  <-  as.numeric(substr(dfhu$last_review, 1, 4))
dfhun <-  subset(dfhu, year >= 2017)
dim(dfhun)[1] - dim(dfhu)[1]


dfhun$numPrice <- as.numeric(gsub("[$]",'',dfhun$price))
summary(dfhun$room_type)
final  <-  subset(dfhun, room_type != "Shared room")

final$rooms  <-  ifelse(final$bedrooms == 0, 1, final$bedrooms)
final$priceperroom  <-  as.numeric(ifelse(final$room_type == T,  final$numPrice,  final$numPrice / final$rooms))


dim(final) / dim(df)


############ Census data of all Canadians Metros


diversity_csd_map <- function(csd) {
  
  csd.ct <- get_census("CA16", regions=list(CSD=csd), 
                       vectors = minority_vectors, level = "CT",
                       labels = "short", geo_format = NA)
  
  csd.csd <- get_census("CA16", regions=list(CSD=csd), 
                        vectors = minority_vectors, level = "CSD",
                        labels = "short", geo_format = NA)
  
  # Calculating diversity (Theil's E)
  # For every variable, divide by v_CA16_3999 and multiply times the logged inverse proportion, then
  # take the sum for each tract. With 14 different groups, the max entropy is ln(14) = 2.64
  base_pop <- quo(v_CA16_3954)
  csd.ei <- csd.ct %>% 
    group_by(GeoUID) %>% 
    mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
    select(GeoUID, ends_with("_E")) %>% 
    ungroup() %>% 
    mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
    mutate(Ei = rowSums(select(.,-1), na.rm = FALSE)) %>% 
    select(GeoUID, Ei)
  
  csd.eicsd <- csd.csd %>% 
    group_by(GeoUID) %>% 
    mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
    select(GeoUID, ends_with("_E")) %>% 
    ungroup() %>% 
    mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
    mutate(Ei = rowSums(select(.,-1), na.rm = FALSE)) %>% 
    select(GeoUID, Ei)
  
  # Join with geography
  
  csd.geo <- get_census_geometry("CA16", regions=list(CSD=csd), 
                                 level = "CT", geo_format = "sf")
  
  csd.csd.geo <- get_census_geometry("CA16", regions=list(CSD=csd), 
                                     level = "CSD", geo_format = "sf")
  
  # Which Census Tracts are in which CSD
  
  csd.ct <- csd.geo %>% 
    left_join(csd.ei) %>% 
    mutate()
  
  csd.csd <- csd.csd.geo %>% 
    left_join(csd.eicsd) %>% 
    mutate()
  
  # Adding map detail
  mz_set_tile_host_nextzen()
  get_vector_tiles <- function(bbox){
    mz_set_tile_host_nextzen(getOption("nextzen_API_key"))
    mx_box=mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)
    mz_vector_tiles(mx_box, height = 1000, width = 1000)
  }
  
  bbox <- st_bbox(csd.ct)
  vector_tiles <- get_vector_tiles(bbox)
  
  if(length(vector_tiles$water$features) > 0) {
    water <- as_sf(vector_tiles$water)
  } 
  
  if(length(vector_tiles$roads$features) > 0) {
    roads <- as_sf(vector_tiles$roads) %>% 
      filter(kind == "highway")
  } else {roads <- water}
  
  if(length(vector_tiles$transit$features) > 0) {
    transit <- as_sf(vector_tiles$transit) %>% filter(kind == "subway")
  } else {transit <- water}
  
  ct_div_plot <- ggplot(csd.ct) + 
    geom_sf(aes(fill = Ei, colour = Ei)) +  
    geom_sf(data = csd.csd.geo, fill = NA, colour = "white") +
    geom_sf(data = transit, size = 0.2, colour = "grey24") +
    geom_sf(data = roads, size = 0.2, colour = "grey36") +
    geom_sf(data = water, fill = "lightblue", colour = NA) + 
    coord_sf(datum = NA) +
    scale_fill_viridis_c("Diversity Entropy Index",
                         option = 3, breaks = c(0,0.5,1,1.5,2), 
                         limits = c(0,2),
                         labels = c("Less\nDiverse","","","","More\nDiverse"),
                         guide = guide_legend(
                           direction = "horizontal",
                           title.position = "top",
                           label.position = "bottom",
                           keywidth = unit(2,"line"))) +
    scale_colour_viridis_c(option = 3, guide = "none",limits = c(0,2)) +
    theme(panel.background = element_blank(),
          legend.position = c(0.2,0.9),
          legend.background = element_blank(),
          legend.key = element_rect(color = NA)) + 
    labs(caption = "Dmitry Shkolnik @dshkol | Data: Census 2016, Statistics Canada")
  return(ct_div_plot)
}

# A function to calculate diversity scores only for CSD
diversity_csd <- function(cma) {
  cma.csd <- get_census("CA16", regions=list(CMA=cma), 
                        vectors = minority_vectors, level = "CSD",
                        labels = "short", geo_format = NA)
  
  # Calculating diversity (Theil's E)
  # For every variable, divide by v_CA16_3999 and multiply times the logged inverse proportion, then
  # take the sum for each tract. With 14 different groups, the max entropy is ln(14) = 2.64
  base_pop <- quo(v_CA16_3954)
  cma.eicsd <- cma.csd %>% 
    group_by(GeoUID,`Region Name`, Population) %>% 
    mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
    select(GeoUID, `Region Name`, Population, ends_with("_E")) %>% 
    ungroup() %>% 
    mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
    mutate(Ei = rowSums(select(.,-c(1,2,3)), na.rm = FALSE)) %>% 
    mutate(CMA = cma) %>% 
    select(CMA, GeoUID, `Region Name`, Population, Ei)
  return(cma.eicsd)
}

# Apply function to get EI for all CSD for all Canadian CMAS
cmas <- list_census_regions("CA16") %>% filter(level == "CMA") %>% select(region, name, pop)

cma_ei <- purrr::map_df(cmas$region, .f = diversity_csd) %>% 
  left_join(cmas, by = c("CMA"="region")) %>% 
  select(`Region Name`, `CMA Name`=name, CMA, GeoUID, Population, `CMA Population` = pop, Ei)


calc_h <- function(cma_obj) {
  cth <- cma_obj$ct %>% 
    select(GeoUID, CSD_UID, Population, Ei)
  st_geometry(cth) <- NULL
  
  cth <- cth %>%
    left_join(cma_obj$csd, by = c("CSD_UID"="GeoUID")) %>%
    select(GeoUID, CSD_UID, name, ctpop = Population.x,
           csdpop = Population.y, ctei = Ei.x, csdei = Ei.y) %>%
    group_by(GeoUID, CSD_UID) %>%
    filter(csdpop > 1000) %>%
    mutate(smallh = (ctpop*(csdei - ctei))/(csdei*csdpop)) %>%
    ungroup()
  
  csdh <- cth %>%
    group_by(CSD_UID, csdei) %>%
    summarise(H = sum(smallh, na.rm = TRUE)) %>% 
    right_join(cma_obj$csd,by = c("CSD_UID"="GeoUID"))
  
  return(csdh)
}

segregation_csd <- function(cma) {
  cma.ct <- get_census("CA16", regions=list(CMA=cma), 
                       vectors = minority_vectors, level = "CT",
                       labels = "short", geo_format = "sf")
  st_geometry(cma.ct) <- NULL
  
  cma.csd <- get_census("CA16", regions=list(CMA=cma), 
                        vectors = minority_vectors, level = "CSD",
                        labels = "short", geo_format = NA)
  
  base_pop <- quo(v_CA16_3954)
  
  cma.eict <- cma.ct %>% 
    group_by(GeoUID,`Region Name`, Population) %>% 
    mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
    ungroup() %>% 
    select(GeoUID, CSD_UID, Population, ends_with("_E")) %>% 
    mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>% 
    mutate(Ei = rowSums(select(.,-c(1,2,3)), na.rm = FALSE)) %>% 
    ungroup() %>% 
    select(GeoUID, CSD_UID, Population, Ei)
  
  cma.eicsd <- cma.csd %>%
    group_by(GeoUID,`Region Name`, Population) %>%
    mutate_at(minorities, funs(E = (./!!(base_pop))*(log(!!(base_pop)/.)))) %>%
    ungroup() %>%
    select(GeoUID, `Region Name`, Population, ends_with("_E")) %>%
    mutate_at(vars(ends_with("_E")), funs(ifelse(is.nan(.),0,.))) %>%
    mutate(Ei = rowSums(select(.,-c(1,2,3)), na.rm = FALSE)) %>%
    mutate(CMA = cma) %>%
    select(CMA, GeoUID, `Region Name`, Population, Ei)
  
  cma.h <- cma.eict %>%
    left_join(cma.eicsd, by = c("CSD_UID"="GeoUID")) %>%
    select(GeoUID, CSD_UID, name = `Region Name`, ctpop = Population.x,
           csdpop = Population.y, ctei = Ei.x, csdei = Ei.y) %>%
    filter(csdpop > 1000) %>%
    group_by(GeoUID, CSD_UID) %>%
    mutate(smallh = (ctpop*(csdei - ctei))/(csdei*csdpop)) %>%
    ungroup() %>%
    group_by(CSD_UID, csdei, name, csdpop) %>%
    summarise(H = sum(smallh, na.rm = TRUE)) %>% 
    mutate(cma = cma)
  return(cma.h)
}




############# compute segregation for all metros and plot comparison of index

cma_seg <- purrr::map_df(cmas$region, .f = segregation_csd) %>% 
  left_join(cmas, by = c("cma"="region")) %>% 
  select(`Region Name`=name.x, `CMA Name`=name.y, CMA = cma,Population = csdpop, `CMA Population` = pop, Ei = csdei, H) %>% 
  ungroup()


clean_names <- function (dfr) {
  dfr <- dfr %>% mutate(name = as.character(name))
  replacement <- dfr %>% mutate(name = gsub(" \\(.*\\)", 
                                            "", name)) %>% pull(name)
  duplicated_rows <- c(which(duplicated(replacement, fromLast = TRUE)), 
                       which(duplicated(replacement, fromLast = FALSE)))
  replacement[duplicated_rows] <- dfr$name[duplicated_rows]
  dfr$name <- factor(replacement)
  dfr
}

clean_names2 <- function (dfr) {
  dfr <- dfr %>% mutate(`Region Name` = as.character(`Region Name`))
  replacement <- dfr %>% mutate(`Region Name` = gsub(" \\(.*\\)", 
                                                     "", `Region Name`)) %>% pull(`Region Name`)
  duplicated_rows <- c(which(duplicated(replacement, fromLast = TRUE)), 
                       which(duplicated(replacement, fromLast = FALSE)))
  replacement[duplicated_rows] <- dfr$`Region Name`[duplicated_rows]
  dfr$`Region Name` <- factor(replacement)
  dfr
}


ggplot(bind_rows(cma_seg %>% 
                   filter(Population > 100000) %>% 
                   clean_names2 %>% 
                   top_n(10, -H), 
                 cma_seg %>% 
                   filter(Population > 100000) %>% 
                   clean_names2 %>% 
                   top_n(10, H)), 
       aes(y = H, x = reorder(`Region Name`, H), size = Population)) + 
  #geom_bar(stat = "identity") +
  geom_point(colour = "#7e008c") + 
  coord_flip() + 
  theme_ipsum() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        legend.position = "none") +
  labs(y = "Segregation entropy index", x = "", 
       #title = "The most and the least segregated large cities in Canada",
       caption = "Segregation index of visible minorities\n@dshkol | Data: Statistics Canada, Census 2016")


ggplot(cma_seg%>% filter(Population > 100000) %>% clean_names2 %>% 
         mutate(big_cma = ifelse(`CMA Population` > 1000000, `CMA Name`,"Other")),
       aes(y= H, x = Ei, size = Population^1.5, colour = big_cma)) +
  #geom_label_repel(aes(label = `Region Name`)) +
  geom_text_repel(aes(label = `Region Name`)) +
  scale_size_continuous(guide = FALSE) + 
  scale_colour_ipsum("", guide = FALSE) +
  theme_ipsum() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) + 
  labs(x = "More diverse \u2192", y = "More segregated \u2192",
       caption = "Entropy index based calculations of diversity and segregation\nof visible minority groups in cities with population over 100,000\n@dshkol | Data: Statistics Canada, Census 2016")

####################################### 
#Start city specific analysis


city <-  "toronto"


if(city == "toronto"){
  citynames  <-  toronto
  censuscode  <-  ct
  cityhall  <-  cht
  censusCodeMuni  <- cmt 
  breakDist <-  bdtv
}
if(city == "vancouver"){
  citynames  <-  vancouver
  censuscode  <-  cv
  cityhall  <-  chv
  censusCodeMuni  <-  cmv 
  breakDist  <-  bdtv
}
if(city == "montreal"){
  citynames  <-  montreal
  censuscode  <-  cm
  cityhall  <-  chm
  censusCodeMuni <-  cmm
  breakDist  <-  bdm
}


city_data <-  subset(final, city %in% citynames)

## Identifying the vectors for visible Minority status
parent_vector <- "v_CA16_3954"
minorities <- list_census_vectors("CA16") %>% 
  filter(vector == "v_CA16_3954") %>% 
  child_census_vectors(leaves_only = TRUE) %>% 
  pull(vector)

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



############### map concentration of minorities

long.ct <- get_census("CA16", regions=list(CSD=censusCodeMuni), 
                      vectors = minority_vectors, level = "CT",
                      labels = "detailed", geo_format = "sf")
names(long.ct)[14:27] <- c("Total","White","South Asian","Chinese","Black","Filipino",
                           "Latin American","Arab","SE Asian","West Asian","Korean",
                           "Japanese","Other","Multiple")

long.ct.tidy <- long.ct %>% 
  select(-White) %>% 
  tidyr::gather(Group, Count, `South Asian`:Multiple) %>% 
  mutate(Proportion = Count/Total)

#head(as.data.frame(long.ct.tidy))

ggplot(long.ct.tidy) + geom_sf(aes(fill = Proportion^(1/2), colour = Proportion^(1/2))) + 
  scale_fill_viridis_c(option = 3, guide = FALSE) + 
  scale_colour_viridis_c(option = 3, guide = FALSE) + 
  theme_void() + 
  coord_sf(datum = NA) +
  facet_wrap(~Group, ncol = 4)  +
  labs(caption = "Visible minority groups by square-root proportion of Census Tract population\n@dshkol | Data: Statistics Canada, Census 2016")



#############aggregate airbnb into city tracts.
city_tr  <-  csd.geo[csd.geo$CSD_UID == censusCodeMuni,]
#city_tr$vacancy = (city_tr$Dwellings - city_tr$Households) / city_tr$Dwellings
#save(toronto_tr, vancouver_tr, file="cityTractsWithPopAndVacancy.RData")


rbnb_pts <- city_data 
coordinates(rbnb_pts) <- ~longitude+latitude
city_tracts <- as_Spatial(city_tr)
rbnb_pts@proj4string <-CRS("+proj=longlat +datum=WGS84")
city_tracts@proj4string <-CRS("+proj=longlat +datum=WGS84")
plot(city_tracts, border = "grey")
points(rbnb_pts, col = "red", cex = 0.2, pch = 16)

#class(city_tracts)
#class(rbnb_pts)

PTS <- as(rbnb_pts, "sf")
POLY <- as(city_tracts, "sf")
idata <- st_intersection(PTS, POLY)

#head(idata)
idata$distribPrice <- cut2(idata$priceperroom, g = 10)
intervals <- levels(unique(idata$distribPrice))

rbnbPerTract <- as.data.frame(idata %>%
                                #group_by(CSD_UID) %>%
                                count(GeoUID, sort = TRUE) %>%
                                select(GeoUID, n))[,1:2]

rbnbDistributionPerTract <- as.data.frame(idata %>%
                                            #group_by(CSD_UID) %>%
                                            count(GeoUID, distribPrice) %>%
                                            select(GeoUID, distribPrice, n))[,1:3]


tractDistrib <- dcast(rbnbDistributionPerTract, GeoUID ~ distribPrice)
tractDistrib[is.na(tractDistrib)] <- 0
tractDistrib <- tractDistrib[,c("GeoUID", intervals)]
colnames(tractDistrib)[2:11] <- paste0("G", 1:10)

lookupDistrib <-  data.frame('name' = paste0("G", 1:10), 'interval' = intervals)

listingCounts <-  as.data.frame(rbnbPerTract[,1:2])
city_tracts@data <- data.frame(city_tracts@data,
                                  listingCounts[match(city_tracts@data$GeoUID,
                                                      listingCounts$GeoUID),])
city_tracts@data <- data.frame(city_tracts@data,
                                  tractDistrib[match(city_tracts@data$GeoUID,
                                                     tractDistrib$GeoUID),])
city_tracts@data <- data.frame(city_tracts@data,
                                  tractTable[match(city_tracts@data$GeoUID,
                                                   tractTable$GeoUID),])

Mino <-  as.data.frame(cma.ct[,c("GeoUID", minorities)])

city_tracts@data <- data.frame(city_tracts@data,
                                  Mino[match(city_tracts@data$GeoUID,
                                             Mino$GeoUID),])

city_tracts@data$ListingPerCapita <-  city_tracts@data$n / city_tracts@data$Population
city_tracts@data <- city_tracts@data %>% 
  mutate_at(minorities, funs(Share =./ Population) )

#summary(city_tracts@data)
centroidsCity <-  gCentroid(city_tracts,byid=TRUE)
plot(centroidsCity, add = T, pch = 16, cex = 0.6)

listMinoShares  <-  paste0(minorities, "_Share")

EstimatorAirbnbPresence <-  cbind(city_tracts@data[,c("GeoUID", "ListingPerCapita", "Ei", 
                                                       listMinoShares)],
                                coordinates(centroidsCity))

#head(EstimatorAirbnbPresence)

DistCityHall <- distm(EstimatorAirbnbPresence[,c('x','y')], 
                      cityhall, fun=distVincentyEllipsoid)
EstimatorAirbnbPresence <-  cbind(EstimatorAirbnbPresence, DistCityHall)

#head(EstimatorAirbnbPresence)

hist(EstimatorAirbnbPresence$DistCityHall)



ggplot(EstimatorAirbnbPresence, aes(x = DistCityHall, y = ListingPerCapita)) +
  geom_point() + geom_smooth() + scale_x_log10() + scale_y_log10() +
  geom_vline(xintercept = breakDist, col = "orange", cex = 1)

# mean value of listing per capita of tracts between 0 & 10 km: 
central <- EstimatorAirbnbPresence %>%
  filter(DistCityHall <= breakDist, !is.na(ListingPerCapita)) %>%
  select(ListingPerCapita, DistCityHall, Ei, listMinoShares)

# central %>%
#   summarise(mean(ListingPerCapita))


# regression listing per capita of tracts above 10 km: 
peripheral <- EstimatorAirbnbPresence %>%
  filter(DistCityHall > breakDist, !is.na(ListingPerCapita)) %>%
  select(ListingPerCapita, DistCityHall, Ei, listMinoShares)

#summary(lm(log(ListingPerCapita) ~ log(DistCityHall), data = peripheral))


lab  <-  as.data.frame (list_census_vectors('CA16') %>% 
                       filter(vector %in% minorities) %>% 
                       select(vector, label))


for (sample in c("all", "central", "peripheral")){
  if (sample == "all") df <-  EstimatorAirbnbPresence
  if (sample == "central") df <-  central
  if (sample == "peripheral") df  <-  peripheral
  
  f <- paste0("scale(log(ListingPerCapita)) ~ scale(log(DistCityHall)) + ",
             paste0("scale(",listMinoShares[-1], ")", collapse = " + "))
  model <- lm(formula(f), data = df)
  print(paste0("obs = ", dim(df)[1]))
  print(summary(model))
  
  coeffs <-  as.data.frame(summary(model)$coefficients)
  coeffs$label <- substr(rownames(coeffs), 7, 17)
  res  <-  data.frame(coeffs, lab[match(coeffs$label, lab$vector),])
  res[,c("label", "vector")] <- NULL
  assign(paste0("Results_", sample), res)
}

#str(Results_all)
reg_total  <-  cbind(Results_all,Results_central, Results_peripheral)
reg_total[,c("t.value", "Std..Error", "t.value.1", "Std..Error.1", "label.1.1", "t.value.2", "Std..Error.2", "label.1.2")] <- NULL
colnames(reg_total)  <-  c("est_all", "pval_all",  "minority",  "est_central", "pval_central", 
                        "est_peripheral", "pval_peripheral")
reg_total$variables  <-  rownames(reg_total)
reg_total$var  <-  ifelse(is.na(reg_total$minority), as.character(reg_total$variables), as.character(reg_total$minority))


pall <- ggplot(reg_total, aes(x = var)) + 
  geom_bar(aes(y = est_all, fill = ifelse(pval_all < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") + 
  # scale_y_continuous(limits = c(-0.55, 0.55)) +
  coord_flip() + theme(legend.title=element_blank()) 
pcent <- ggplot(reg_total, aes(x = var)) + 
  geom_bar(aes(y = est_central, fill = ifelse(pval_central < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") +  
  # scale_y_continuous(limits = c(-0.55, 0.55)) + 
  coord_flip() +   theme(legend.title=element_blank() ) 

pper <- ggplot(reg_total, aes(x = var)) + 
  geom_bar(aes(y = est_peripheral, fill = ifelse(pval_peripheral < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") +
  #scale_y_continuous(limits = c(-0.55, 0.55)) + 
  coord_flip() +   theme(legend.title=element_blank()) 

#grid.arrange(pall, pcent, pper)


EstimatorAirbnbPrice <- data.frame(idata, EstimatorAirbnbPresence[match(idata$GeoUID, 
                                                                        EstimatorAirbnbPresence$GeoUID),])
# head(EstimatorAirbnbPrice)
# hist(EstimatorAirbnbPrice$priceperroom)


EstimatorAirbnbPrice_all <- EstimatorAirbnbPrice %>%
  filter(!is.na(priceperroom), priceperroom > 0) 
# mean value of listing per capita of tracts between 0 & 10 km: 
central_P <- EstimatorAirbnbPrice_all %>%
  filter(DistCityHall <= breakDist) #%>%   select(ListingPerCapita, DistCityHall, Ei, listMinoShares)

peripheral_P <- EstimatorAirbnbPrice_all %>%
  filter(DistCityHall > breakDist) #%>%   select(ListingPerCapita, DistCityHall, Ei, listMinoShares)



for (sample in c("all", "central", "peripheral")){
  if (sample == "all") df <-  EstimatorAirbnbPrice_all
  if (sample == "central") df  <-  central_P
  if (sample == "peripheral") df  <-  peripheral_P
  
  f  <-  paste0("scale(priceperroom) ~ scale(log(DistCityHall)) + ",
             paste0("scale(",listMinoShares[-1], ")", collapse = " + "))
  model <- lm(formula(f), data = df)
  
  print(paste0("obs = ", dim(df)[1]))
  print(summary(model))
  
  coeffs  <-  as.data.frame(summary(model)$coefficients)
  coeffs$label <- substr(rownames(coeffs), 7, 17)
  res  <-  data.frame(coeffs, lab[match(coeffs$label, lab$vector),])
  res[,c("label", "vector")] <- NULL
  assign(paste0("Results_", sample, "Price"), res)
}


reg_total2  <-  cbind(Results_allPrice,Results_centralPrice, Results_peripheralPrice)
reg_total2[,c("t.value", "Std..Error", "t.value.1", "Std..Error.1", "label.1.1", "t.value.2", "Std..Error.2", "label.1.2")] <- NULL
colnames(reg_total2) <-  c("est_all", "pval_all",  "minority",  "est_central", "pval_central", 
                         "est_peripheral", "pval_peripheral")
reg_total2$variables <-  rownames(reg_total2)
reg_total2$var  <-  ifelse(is.na(reg_total2$minority), as.character(reg_total2$variables), 
                        as.character(reg_total2$minority))

pall2 <- ggplot(reg_total2, aes(x = var)) + 
  geom_bar(aes(y = est_all, fill = ifelse(pval_all < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") + 
  # scale_y_continuous(limits = c(-0.55, 0.55)) +
  coord_flip() + theme(legend.title=element_blank()) 
pcent2 <- ggplot(reg_total2, aes(x = var)) + 
  geom_bar(aes(y = est_central, fill = ifelse(pval_central < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") +  
  # scale_y_continuous(limits = c(-0.55, 0.55)) + 
  coord_flip() + theme(legend.title=element_blank() ) 
pper2 <- ggplot(reg_total2, aes(x = var)) + 
  geom_bar(aes(y = est_peripheral, fill = ifelse(pval_peripheral < 0.05, "pval < 0.05", "pval >= 0.05")), stat="identity") +
  #scale_y_continuous(limits = c(-0.55, 0.55)) +
  coord_flip() + 
  theme(legend.title=element_blank()) 

grid.arrange(pall, pall2, 
             pcent, pcent2, 
             pper, pper2, 
             ncol = 2)



############# Reardon segregation for AirBnB price per room
cumulativeFrequency  <-  function(distribution){
  relativeDistribution  <-  distribution / sum(distribution)
  iterator <-  length(distribution) - 1
  cumulativeRelative  <-  seq(1:iterator)
  for (i in 1:iterator){
    if (i == 1) cumulativeRelative[[i]] <-  relativeDistribution[[i]]
    if (i != 1) {
      cumulativeRelative[[i]]  <-  cumulativeRelative[[i-1]] +  relativeDistribution[[i]]
    }}
  return(cumulativeRelative)
}
segFunction2 <-  function(distribution){
  ordinalVariationIndexR <-  4 * distribution * ( 1 - distribution)
  return(ordinalVariationIndexR)
}
segIndex10  <-  function(tabOfSpatialUnits, distributionColNames, K = 10){
  DistribCluster  <-  colSums(tabOfSpatialUnits[,distributionColNames], na.rm = T)
  Tcluster <-  sum(DistribCluster)
  cumulativeDistributionCluster <-  cumulativeFrequency(DistribCluster)
  tabOfSpatialUnits$t <-  rowSums(tabOfSpatialUnits[,distributionColNames], na.rm = T)
  distributionColName <-  distributionColNames[1:(K-1)]
  ReldistributionColNames  <-  paste("Rel",distributionColName, sep="")
  CumdistributionColNames  <-  paste("Cum",distributionColName, sep="")
  MSeg_Cols <-  paste("Seg", distributionColName, sep="")
  tabOfSpatialUnits[,ReldistributionColNames] <-  tabOfSpatialUnits[,distributionColName] / tabOfSpatialUnits$t
  tabOfSpatialUnits[,CumdistributionColNames[1]] <- tabOfSpatialUnits[,ReldistributionColNames[1]]
  tabOfSpatialUnits[,CumdistributionColNames[2]]  <-  tabOfSpatialUnits[,ReldistributionColNames[2]] + tabOfSpatialUnits[,CumdistributionColNames[1]]
  tabOfSpatialUnits[,CumdistributionColNames[3]] <-  tabOfSpatialUnits[,ReldistributionColNames[3]] + tabOfSpatialUnits[,CumdistributionColNames[2]]
  tabOfSpatialUnits[,CumdistributionColNames[4]]  <-  tabOfSpatialUnits[,ReldistributionColNames[4]] + tabOfSpatialUnits[,CumdistributionColNames[3]]
  tabOfSpatialUnits[,CumdistributionColNames[5]]  <-  tabOfSpatialUnits[,ReldistributionColNames[5]] + tabOfSpatialUnits[,CumdistributionColNames[4]]
  tabOfSpatialUnits[,CumdistributionColNames[6]]  <-  tabOfSpatialUnits[,ReldistributionColNames[6]] + tabOfSpatialUnits[,CumdistributionColNames[5]]
  tabOfSpatialUnits[,CumdistributionColNames[7]] <-  tabOfSpatialUnits[,ReldistributionColNames[7]] + tabOfSpatialUnits[,CumdistributionColNames[6]]
  tabOfSpatialUnits[,CumdistributionColNames[8]] <-  tabOfSpatialUnits[,ReldistributionColNames[8]] + tabOfSpatialUnits[,CumdistributionColNames[7]]
  tabOfSpatialUnits[,CumdistributionColNames[9]] <- tabOfSpatialUnits[,ReldistributionColNames[9]] + tabOfSpatialUnits[,CumdistributionColNames[8]]
  tabOfSpatialUnits[,MSeg_Cols] <-  segFunction2(tabOfSpatialUnits[,CumdistributionColNames])
  tabOfSpatialUnits$v <-  (1 / (K - 1)) * rowSums(tabOfSpatialUnits[,MSeg_Cols], na.rm = T)
  Vcluster <- (1 / (K - 1)) * sum(segFunction2(cumulativeDistributionCluster))
  tabOfSpatialUnits$seg  <-  (tabOfSpatialUnits$t / (Tcluster * Vcluster)) * (Vcluster - tabOfSpatialUnits$v)
  segIndex <- sum(tabOfSpatialUnits$seg, na.rm = T)
  return(segIndex)
}

ReardonSegregationAirbnbPrice <- segIndex10(tabOfSpatialUnits = city_tracts@data, distributionColNames = paste0("G", 1:10))
EntropySegregationMinorities  <-  cma_seg[cma_seg$CSD_UID == censusCodeMuni,"H"]

ReardonSegregationAirbnbPrice
EntropySegregationMinorities






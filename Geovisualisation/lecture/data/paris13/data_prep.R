library(sf)

######## INSEE IGN Extract
# http://professionnels.ign.fr/contoursiris
iris_fra <- st_read("../data-raw/CONTOURS-IRIS_2-1__SHP__FRA_2017-06-30/CONTOURS-IRIS/1_DONNEES_LIVRAISON_2016/CONTOURS-IRIS_2-1_SHP_LAMB93_FE-2016/CONTOURS-IRIS.shp", stringsAsFactors = F)
# https://www.insee.fr/fr/statistiques/3137409
iris_data <- read.csv(file = "../data-raw/base-ic-evol-struct-pop-2014.csv", stringsAsFactors = F)
iris_fra <- st_set_crs(iris_fra, 2154)

# iris_75 <- iris_fra[substr(iris_fra$INSEE_COM, 1,2)=="75",]
# iris_75 <- merge(iris_75, iris_data, by.x = "CODE_IRIS", by.y = "IRIS", all.x = T)
# iris_75 <- iris_75[,c("CODE_IRIS", "P14_POP")]

iris_p13 <- iris_fra[iris_fra$INSEE_COM=="75113",]
iris_p13 <- merge(iris_p13, iris_data, by.x = "CODE_IRIS", by.y = "IRIS", all.x = T)
iris_p13 <- iris_p13[,c("CODE_IRIS", "P14_POP")]

# iris_p14 <- iris_fra[iris_fra$INSEE_COM=="75114",]
# iris_p14 <- merge(iris_p14, iris_data, by.x = "CODE_IRIS", by.y = "IRIS", all.x = T)
# iris_p14 <- iris_p14[,c("CODE_IRIS", "P14_POP")]

iris_31 <- iris_fra[substr(iris_fra$INSEE_COM, 1,2)=="31",]
iris_31 <- merge(iris_31, iris_data, by.x = "CODE_IRIS", by.y = "IRIS", all.x = T)
iris_31 <- iris_31[,c("CODE_IRIS", "P14_POP","INSEE_COM")]
iris_31[which(iris_31$INSEE_COM=="31307"),"INSEE_COM"] <- "31412" #erreur de code commune dans la table iris (fusion non prise en compte)


# iris_tlse <- iris_fra[iris_fra$INSEE_COM=="31555",]
# iris_tlse <- merge(iris_tlse, iris_data, by.x = "CODE_IRIS", by.y = "IRIS", all.x = T)
# iris_tlse <- iris_tlse[,c("CODE_IRIS", "P14_POP")]


####### SIRENE extract
# http://data.cquest.org/geo_sirene/last/


# cc <- 75101:75120
# l <- vector("list", 20)
# for (i in 1:20){
#   . <- read.csv(paste0('../data-raw/geo-sirene_',cc[i],'.csv'), stringsAsFactors = F)
#   . <- .[substr(.$APET700,1,4)==5610, c("SIREN","L1_NORMALISEE", 
#                                         "L2_NORMALISEE","APET700", "LIBAPET",
#                                         "longitude", "latitude")]
#   l[[i]] <- .
# }
# 
# sir <- do.call( "rbind", l)
# sir <- st_as_sf(sir, coords=c("longitude", "latitude"), crs = 4326, 
#                 stringsAsFactors = FALSE, remove = FALSE)
# sir <- st_transform(sir, 2154)
# sir <- st_jitter(sir, 1)


sir_p13 <- read.csv('../data-raw/geo-sirene_75113.csv', stringsAsFactors = F)
sir_p13 <- sir_p13[substr(sir_p13$APET700,1,4)==5610,]
sir_p13 <- st_as_sf(sir_p13, coords=c("longitude", "latitude"), crs = 4326, 
                    stringsAsFactors = FALSE)
sir_p13 <- st_transform(sir_p13, 2154)
sir_p13 <- st_jitter(sir_p13, 1)


# sir_p14 <- read.csv('../data-raw/geo-sirene_75114.csv', stringsAsFactors = F)
# sir_p14 <- sir_p14[substr(sir_p14$APET700,1,4)==5610,]
# sir_p14 <- st_as_sf(sir_p14, coords=c("longitude", "latitude"), crs = 4326, 
#                     stringsAsFactors = FALSE)
# sir_p14 <- st_transform(sir_p14, 2154)
# sir_p14 <- st_jitter(sir_p14, 1)


sir_31 <- read.csv('../data-raw/geo-sirene_31.csv', stringsAsFactors = F, colClasses=c("COMET"="character"))
sir_31 <- sir_31[substr(sir_31$APET700,1,4)==5610,]
sir_31 <- st_as_sf(sir_31, coords=c("longitude", "latitude"), crs = 4326, 
                   stringsAsFactors = FALSE)
sir_31 <- st_transform(sir_31, 2154)
sir_31 <- st_jitter(sir_31, 1)
sir_tlse <- sir_31[sir_31$COMET=="555",]


####### OSM extact
library(osmdata)

#osm (75)
# q <- opq(bbox=st_bbox(st_transform(iris_75, 4326)))
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "restaurant" )
# res <- osmdata_sf(q1)
# restau1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# restau2 <- res$osm_polygons
# st_geometry(restau2) <- st_centroid(st_geometry(restau2))
# restau3 <- res$osm_multipolygons
# st_geometry(restau3) <- st_centroid(st_geometry(restau3), 
#                                     of_largest_polygon = TRUE)
# 
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "fast_food" )
# res <- osmdata_sf(q1)
# fast1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# fast2 <- res$osm_polygons
# st_geometry(fast2) <- st_centroid(st_geometry(fast2))
# 
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "cafe" )
# res <- osmdata_sf(q1)
# cafe1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# cafe2 <- res$osm_polygons
# st_geometry(cafe2) <- st_centroid(st_geometry(cafe2))
# osm <- rbind(restau1[, c("osm_id", "name")], restau2[, c("osm_id", "name")], 
#              restau3[, c("osm_id", "name")], fast1[, c("osm_id", "name")], 
#              fast2[, c("osm_id", "name")], 
#              cafe1[, c("osm_id", "name")], cafe2[, c("osm_id", "name")])
# osm <- st_transform(osm, 2154)
# osm <- st_intersection(osm, iris_75)
# osm <- st_jitter(osm, 1)


#osm_p13
q <- opq(bbox=st_bbox(st_transform(iris_p13, 4326)))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "restaurant" )
res <- osmdata_sf(q1)
restau1  <- res$osm_points[!is.na(res$osm_points$amenity),]
restau2 <- res$osm_polygons
st_geometry(restau2) <- st_centroid(st_geometry(restau2))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "fast_food" )
res <- osmdata_sf(q1)
fast1  <- res$osm_points[!is.na(res$osm_points$amenity),]
fast2 <- res$osm_polygons
st_geometry(fast2) <- st_centroid(st_geometry(fast2))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "cafe" )
res <- osmdata_sf(q1)
cafe1  <- res$osm_points[!is.na(res$osm_points$amenity),]
cafe2 <- res$osm_polygons
st_geometry(cafe2) <- st_centroid(st_geometry(cafe2))
osm_p13 <- rbind(restau1[, c("osm_id", "name")], restau2[, c("osm_id", "name")], 
                 fast1[, c("osm_id", "name")], fast2[, c("osm_id", "name")], 
                 cafe1[, c("osm_id", "name")], cafe2[, c("osm_id", "name")])
osm_p13 <- st_transform(osm_p13, 2154)
osm_p13 <- st_intersection(osm_p13, iris_p13)
oms_p13 <- st_jitter(osm_p13, 1)


#osm_31
q <- opq(bbox=st_bbox(st_transform(iris_31, 4326)))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "restaurant" )
res <- osmdata_sf(q1)
restau1  <- res$osm_points[!is.na(res$osm_points$amenity),]
restau2 <- res$osm_polygons
st_geometry(restau2) <- st_centroid(st_geometry(restau2))
restau3 <- res$osm_multipolygons
st_geometry(restau3) <- st_centroid(st_geometry(restau3), 
                                    of_largest_polygon = TRUE)

q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "fast_food" )
res <- osmdata_sf(q1)
fast1  <- res$osm_points[!is.na(res$osm_points$amenity),]
fast2 <- res$osm_polygons
st_geometry(fast2) <- st_centroid(st_geometry(fast2))

q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "cafe" )
res <- osmdata_sf(q1)
cafe1  <- res$osm_points[!is.na(res$osm_points$amenity),]
cafe2 <- res$osm_polygons
st_geometry(cafe2) <- st_centroid(st_geometry(cafe2))


osm_31 <- rbind(restau1[, c("osm_id", "name","amenity")], restau2[, c("osm_id", "name","amenity")], 
             restau3[, c("osm_id", "name","amenity")], fast1[, c("osm_id", "name","amenity")], 
             fast2[, c("osm_id", "name","amenity")], 
             cafe1[, c("osm_id", "name","amenity")], cafe2[, c("osm_id", "name","amenity")])
osm_31 <- st_transform(osm_31, 2154)
osm_31 <- st_intersection(osm_31, iris_31)
osm_31 <- st_jitter(osm_31, 1)




#osm_p14
# q <- opq(bbox=st_bbox(st_transform(iris_p14, 4326)))
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "restaurant" )
# res <- osmdata_sf(q1)
# restau1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# restau2 <- res$osm_polygons
# st_geometry(restau2) <- st_centroid(st_geometry(restau2))
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "fast_food" )
# res <- osmdata_sf(q1)
# fast1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# fast2 <- res$osm_polygons
# st_geometry(fast2) <- st_centroid(st_geometry(fast2))
# q1 <- add_osm_feature(opq = q, key = 'amenity', 
#                       value = "cafe" )
# res <- osmdata_sf(q1)
# cafe1  <- res$osm_points[!is.na(res$osm_points$amenity),]
# cafe2 <- res$osm_polygons
# st_geometry(cafe2) <- st_centroid(st_geometry(cafe2))
# osm_p14 <- rbind(restau1[, c("osm_id", "name")], restau2[, c("osm_id", "name")], 
#                  fast1[, c("osm_id", "name")], fast2[, c("osm_id", "name")], 
#                  cafe1[, c("osm_id", "name")], cafe2[, c("osm_id", "name")])
# osm_p14 <- st_transform(osm_p14, 2154)
# osm_p14 <- st_intersection(osm_p14, iris_p14)
# oms_p14 <- st_jitter(osm_p14, 1)


#osm_tlse
q <- opq(bbox=st_bbox(st_transform(iris_tlse, 4326)))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "restaurant" )
res <- osmdata_sf(q1)
restau1  <- res$osm_points[!is.na(res$osm_points$amenity),]
restau2 <- res$osm_polygons
st_geometry(restau2) <- st_centroid(st_geometry(restau2))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "fast_food" )
res <- osmdata_sf(q1)
fast1  <- res$osm_points[!is.na(res$osm_points$amenity),]
fast2 <- res$osm_polygons
st_geometry(fast2) <- st_centroid(st_geometry(fast2))
q1 <- add_osm_feature(opq = q, key = 'amenity', 
                      value = "cafe" )
res <- osmdata_sf(q1)
cafe1  <- res$osm_points[!is.na(res$osm_points$amenity),]
cafe2 <- res$osm_polygons
st_geometry(cafe2) <- st_centroid(st_geometry(cafe2))
osm_tlse <- rbind(restau1[, c("osm_id", "name")], restau2[, c("osm_id", "name")], 
                  fast1[, c("osm_id", "name")], fast2[, c("osm_id", "name")], 
                  cafe1[, c("osm_id", "name")], cafe2[, c("osm_id", "name")])
osm_tlse <- st_transform(osm_tlse, 2154)
osm_tlse <- st_intersection(osm_tlse, iris_tlse)
osm_tlse <- st_jitter(osm_tlse, 1)

####### tables de passage supra-communales (effet MAUP)
#devtools::install_github("antuki/COGugaison)
library(COGugaison)
library(dplyr)
table_MAUP <- table_supracom_2017 %>%
  filter(DEP=="31") %>% 
  select(-DEP,-REG,-NATURE_EPCI,-TUU2014,-TDUU2014,-TAU2014,-CATAEU2010)
  


#######  Export
saveRDS(object = iris_p13, file = "iris_p13.rds")
# saveRDS(object = iris_p14, file = "iris_p14.rds")
saveRDS(object = iris_31, file = "iris_31.rds")
# saveRDS(object = iris_tlse, file = "iris_tlse.rds")
# saveRDS(object = iris_75, file = "iris_75.rds")



saveRDS(object = sir_p13, file = "sir_p13.rds")
# saveRDS(object = sir_p14, file = "sir_p14.rds")
saveRDS(object = sir_31, file = "sir_31.rds")
# saveRDS(object = sir_tlse, file = "sir_tlse.rds")
# saveRDS(object = sir, file = "sir.rds")


saveRDS(object = osm_p13, file = "osm_p13.rds")
# saveRDS(object = osm_p14, file = "osm_p14.rds")
saveRDS(object = osm_31, file = "osm_31.rds")
# saveRDS(object = osm_tlse, file = "osm_tlse.rds")
# saveRDS(object = osm, file = "osm.rds")

saveRDS(object = table_MAUP, file = "table_MAUP.rds")

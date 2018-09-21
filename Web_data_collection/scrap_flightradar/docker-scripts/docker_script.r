library(jsonlite)
library(dplyr)
library(curl)
library(readr)
library(lubridate)

Sys.setenv(TZ="Europe/Berlin")

timestamp <- as.numeric(as.POSIXct(now()))
url <- paste("https://api.flightradar24.com/common/v1/airport.json?code=bod&plugin[]=&plugin-setting[schedule][mode]=&plugin-setting[schedule][timestamp]=",timestamp,"&page=1&limit=100&token=",sep="")

# https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
json <- jsonlite::fromJSON(url,flatten = T) 

pageOfData <- json$result$response$airport$pluginData$schedule$arrivals$data 
filteredData <- pageOfData %>% select(ICAO = flight.airline.code.icao, Name = flight.airline.name, AirportOrigin = flight.airport.origin.name, ICAOAirport = flight.airport.origin.code.icao, Latitude = flight.airport.origin.position.latitude, Longitude= flight.airport.origin.position.longitude) 

scrapedate <- paste(today(),hour(now()),"-",minute(now()),"-",second(now()),sep = "")
scrapepath <- paste("/usr/local/src/flight-scrap/docker-scripts/data/flightradar",scrapedate,".csv",sep="")
write.csv(filteredData, scrapepath)

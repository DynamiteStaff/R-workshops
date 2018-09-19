library(jsonlite)
library(dplyr)
library(curl)
library(readr)
library(lubridate)

url <- "https://api.flightradar24.com/common/v1/airport.json?code=bod&plugin[]=&plugin-setting[schedule][mode]=&plugin-setting[schedule][timestamp]=1537297562&page=1&limit=100&token="

# https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
json <- jsonlite::fromJSON(url,flatten = T) 

pageOfData <- json$result$response$airport$pluginData$schedule$arrivals$data 
filteredData <- pageOfData %>% select(ICAO = flight.airline.code.icao, Name = flight.airline.name, AirportOrigin = flight.airport.origin.name, ICAOAirport = flight.airport.origin.code.icao, Latitude = flight.airport.origin.position.latitude, Longitude= flight.airport.origin.position.longitude) 

now <- now()
scrapedate <- paste(today(),hour(now),"-",minute(now),"-",second(now),sep = "")
scrapepath <- paste("/data/flightradar",scrapedate,".csv",sep="")
write.csv(filteredData, scrapepath)

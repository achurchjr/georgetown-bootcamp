# LeafletMapsExample.R
#Gates

##########################################################################
##This code works well and creates an interactive layered leaflet/R map
## The map is choropleth and markered
##
## Required datasets are here:
##    CancerCountyFIPS.csv
##    CancerCountyFIPS_Breast.csv
##    LandUseDatasetREALLatlong.csv
## AND ##
############
# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# I downloaded the zip and placed all files in the zip into my RStudio folder
##us.map <- readOGR(dsn = ".", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
##head(us.map)
###############
##Not all of these libraries are required for this code, but
## they are good for more generalized goals
############################################################################

library(leaflet)
library(sp)
library(mapproj)
library(maps)
library(mapdata)
library(maptools)
library(htmlwidgets)
library(magrittr)
library(XML)
library(plyr)
library(rgdal)
library(WDI)
library(raster)
library(noncensus)
library(stringr)
library(tidyr)
library(tigris)
library(rgeos)
library(ggplot2)
library(scales)

data(zip_codes)
data(counties)

##############################
## REVIEW ALL OF THIS CODE
##############################

##
##Then you will add two layers to it
##
## See the Week 2 Assignment


################################################################
##https://www.statecancerprofiles.cancer.gov/incidencerates/index.php?stateFIPS=99&cancer=001&race=07&sex=
##0&age=001&type=incd&sortVariableName=rate&sortOrder=default#results
CancerRates <- read.csv('CancerCountyFIPS.csv')
#head(CancerRates)
CancerRatesB <- read.csv('CancerCountyFIPS_Breast.csv')
#head(CancerRatesB)
LandUse <- read.csv('LandUseDatasetREALLatlong.csv')
#head(LandUse)
## Not using this dataset yet...
#PowerPlants <- read.csv("PowerPlants.csv")
#head(PowerPlants)

## Make all the column names lowercase
names(CancerRates) <- tolower(names(CancerRates))
#head(CancerRates)

# Rename columns to make for a clean df merge later.
##GEOID is the same as FIPS
colnames(CancerRates) <- c("location", "GEOID", "rate")
#head(CancerRates)
colnames(CancerRatesB) <- c("location", "GEOID", "rate")
#head(CancerRatesB)
colnames(LandUse) <- c("offset", "lat", "lng", "url", "name")
#head(LandUse)

##Add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
##formatC is from C code formatting - creates a 5 digit int
CancerRates$GEOID <- formatC(CancerRates$GEOID, width = 5, format = "d", flag = "0")
#head(CancerRates)
CancerRatesB$GEOID <- formatC(CancerRatesB$GEOID, width = 5, format = "d", flag = "0")
head(CancerRatesB)

## Convert column called location to two columns: State and County
CancerRates <- separate(CancerRates, location, into = c("county", "state"), sep = ", ")
#head(CancerRates)
CancerRatesB <- separate(CancerRatesB, location, into = c("county", "state"), sep = ", ")
#head(CancerRatesB)

##Remove the (...) from the state values
CancerRates[] <- lapply(CancerRates, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRates)
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\s*\\([^\\)]+\\)", "", x))
head(CancerRatesB)

##Remove the space# from the end of some of the values in the rate column
CancerRatesB[] <- lapply(CancerRatesB, function(x) gsub("\\#", "", x))
#CancerRatesB

# Convert full state names to abbreviations for a clean df merge later.
CancerRates$state <- state.abb[match(CancerRates$state,state.name)]
#head(CancerRates)
CancerRatesB$state <- state.abb[match(CancerRatesB$state,state.name)]
#head(CancerRatesB)

#Change CancerRates$rate to a number
CancerRates$rate <- as.numeric(as.character(CancerRates$rate))
#head(CancerRates)
CancerRatesB$rate <- as.numeric(as.character(CancerRatesB$rate))
#head(CancerRatesB)

#Allen Church - Import and clean data
#Source https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2017
#Data is taken from 2017
tri <- read.csv('Allen_Church_TRI_2017_FED_Dirty.csv', sep=',', header=TRUE, stringsAsFactors = FALSE)
tridf <- data.frame(tri)

#Aircraft landing facilities
#Source https://geo.dot.gov/server/rest/services/Hosted/Airports_DS/FeatureServer/0
air <- read.csv('Allen_Church_Aircraft_Landing_Facilities_Dirty.csv', sep=',', header=TRUE, stringsAsFactors = FALSE)
airdf <- data.frame(air)

#Select facility type, facility name, elevation from dataframe
factype <- airdf[5]
facname <- airdf[15]
elev <- airdf[19]
airdf <- airdf[,1:2]
airdf <- cbind(airdf, factype, facname, elev)
airdf <- airdf[c(2,1,4,3,5)]
colnames(airdf) <- c("lat", "lng", "name", "type","elev")

#Subset airdf by Heliport
heldf <- airdf[airdf$type == "Heliport",]

#Select name and chemical column from tri dataframe, bind to lat/long columns, and rename
name <- tridf[4]
chem <- tridf[30]
tridf <- tridf[,12:13]
tridf <- cbind(tridf, name, chem)
colnames(tridf) <- c("lat", "lng", "name", "chem")


#Print to user how many total rows vs. how many complete (non-NA) rows for both dataframes
cat("The Heliport dataset has a total of", nrow(heldf)," rows.")
cat("The Heliport dataset has", nrow(heldf[complete.cases(heldf),]), "complete (non-NA) rows.")

cat("The TRI dataset has a total of", nrow(tridf)," rows.")
cat("The TRI dataset has", nrow(tridf[complete.cases(tridf),]), "complete (non-NA) rows.")

#Remove empty rows and verify totals for both dataframes
tridf <- na.omit(tridf)
nrow(tridf)
heldf <- na.omit(heldf)
nrow(heldf)

#Writing clean datasets to CSV per instructions - "Include any and all datasets that you use (after they are clean)"
write.csv(tridf, file = 'Allen_Church_TRI_2017_FED_Clean.csv')
write.csv(heldf, file = 'Allen_Church_Aircraft_Landing_Facilities_Clean.csv')



#End Allen Church data import, export, and cleaning


# Download county shape file.
## !! This is important. Shape files can be found here
#https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- tigris::counties(cb = TRUE, year = 2015)
#OR
# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# I downloaded the zip and placed all files in the zip into my RStudio folder
us.map <- readOGR(dsn = ".", layer = "cb_2016_us_county_20m", stringsAsFactors = FALSE)
head(us.map)
# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
#head(us.map)

# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]

# Merge spatial df with downloaded data.
## This is important
## Now we have our data and the needed carto data
cancermap <- merge(us.map, CancerRates, by=c("GEOID"))
cancermapB <- merge(us.map, CancerRatesB, by=c("GEOID"))

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    cancermap$county, 
                    "<br><strong>Cancer Rate (Age Adjusted) Out of 100,000: </strong>", 
                    cancermap$rate)

#Allen Church adding icon and popup data for TRI and heliport datasets
#source of helicopter icon http://www.otsego2000.org/wp-content/uploads/leaflet-maps-marker-icons/helicopter.png
heliIcon <- iconList(brown = makeIcon("helicopter.png", iconWidth = 24, iconHeight =32))

popup_FN <- paste0("<strong>Facility Name: </strong>", 
                   tridf$name, 
                   "<br><strong>Chemical: </strong>", 
                   tridf$chem)

popup_H <- paste0("<strong>Heliport Name: </strong>", 
                  heldf$name, 
                  "<br><strong>Elevation in Feet: </strong>", 
                  heldf$elev)

#Grouping for map options and User Choices
#https://rstudio.github.io/leaflet/showhide.html

##Make pop up for the land use sites
# Format popup data for leaflet map.
popup_LU <- paste0("<strong>Use Name: </strong>", 
                   LandUse$name, 
                   "<br><strong>Link: </strong>", 
                   LandUse$url)

pal <- colorQuantile("YlOrRd", NULL, n = 9)
gmap <- leaflet(data = cancermap) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 4) %>% 
  addPolygons(fillColor = ~pal(rate), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat,
              group="Cancer Rate/100,000 by Counties") %>% 
  
  # Overlay groups
  addMarkers(data=LandUse,lat=~lat, lng=~lng, popup=popup_LU, group = "Land Use Sites") %>%
  
  #Allen Church adding helicopter icons for heliport locations and circles for toxic release inventory
  addMarkers(data=heldf,lat=~lat, lng=~lng, popup=popup_H, group = "Heliport Locations", icon = heliIcon) %>%
  addCircles(data=tridf,lat=~lat, lng=~lng, popup=popup_FN, group = "Toxics Release Inventory") %>%
  
  
  # Layers control
  #Allen Church adding layers control to Dr. Gates' code
  addLayersControl(
    baseGroups = c("Cancer Rate/100,000 by Counties"),
    overlayGroups = c("Toxics Release Inventory", "Heliport Locations", "Land Use Sites"),
    options = layersControlOptions(collapsed = FALSE)
  )
gmap
#Allen Church - Could not determine source of error, included below code to ignore Warning (this is not ideal)
oldw <- getOption("warn")
options(warn = -1)

#saveWidget(gmap, 'US_county_cancer_poll_map.html', selfcontained = TRUE)

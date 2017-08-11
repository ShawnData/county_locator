

library(ggmap)

#load data and get latitude and longitude
school_name <- read.csv(file.choose(), header = F, stringsAsFactors = F)
lat_long <- geocode(school_name[,1])
# create ID for both data base
lat_long$id <- seq.int(nrow(lat_long))
school_name$id <- seq.int(nrow(school_name))
#match both data
school_latlong <- merge(lat_long, school_name, by.x = "id")
school_latlong

# fill in NAs

#University of Cincinnati – Blue Ash

#school_latlong[93,]$lon <- geocode("9555 Plainfield Rd, Blue Ash, OH 45236")[[1]]
#school_latlong[93,]$lat <- geocode("9555 Plainfield Rd, Blue Ash, OH 45236")[[2]]

#University of Cincinnati – Clermont
#school_latlong[94,]$lon <- geocode("4200 Clermont College Dr, Batavia, OH 45103")[[1]]
#school_latlong[94,]$lat <- geocode("4200 Clermont College Dr, Batavia, OH 45103")[[2]]

#school_latlong$lon <- unlist(school_latlong$lon)
#school_latlong$lat <- unlist(school_latlong$lat)
str(school_latlong)

# Function to Convert lat/long to county



library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}




#find counties

result <- list()
for(i in 1: nrow(school_latlong)){
  
  result[i] <- latlong2county(school_latlong[i,2:3])
  school_latlong$statecounty[i] <- unlist(result[i])
  
  
}



school_latlong$county <- sub(".*,", "",school_latlong$statecounty)
school_latlong$state <- sub(",.*","", school_latlong$statecounty)

#export data
write.csv(school_latlong, "schoolConties.csv", row.names = F)

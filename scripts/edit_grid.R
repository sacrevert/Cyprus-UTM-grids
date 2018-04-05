rm(list=ls())

require(rgdal)
require(sp)
require(raster)
require(sf)

# Read SHAPEFILE.shp from the current working directory
shape <- readOGR(dsn = "data/Grid10km_region.shp", layer = "Grid10km_region")
plot(shape)

shapeL <- as(shape, "SpatialLines")
bb <- bbox(shapeL) # get bounding box
plot(shapeL)

## North-South lines
# x-coords for 2 km grid lines, north-south
xcoordsS <- xcoordsN <- seq(from = bb[1,1], to = bb[1,2], by = 2000)
length(xcoordsN) # 120
# y-coords for 2 km grid lines, north-south
ycoordsS <- rep(bb[2,1], length(xcoordsS))
ycoordsN <- rep(bb[2,2], length(xcoordsN))

Scoords <- data.frame(xcoordsS, ycoordsS)
Ncoords <- data.frame(xcoordsN, ycoordsN)
coordinates(Scoords) = ~xcoordsS + ycoordsS
coordinates(Ncoords) = ~xcoordsN + ycoordsN

NSlines <- list()
for (i in 1:nrow(Scoords@coords)){ NSlines[[i]] <- Lines(list(Line(coordinates(rbind(Scoords[i,],Ncoords[i,])))), ID =  paste("X_",i, sep="") )
}

NSlinesSL <- SpatialLines(NSlines)
NSlinesSL@proj4string <- shape@proj4string
plot(NSlinesSL)

## East-West lines
# x-coords for 2 km grid lines, north-south
ycoordsE <- ycoordsW <- seq(from = bb[2,1], to = bb[2,2], by = 2000)
length(ycoordsE) # 71
# y-coords for 2 km grid lines, north-south
xcoordsW <- rep(bb[1,1], length(ycoordsE))
xcoordsE <- rep(bb[1,2], length(ycoordsE))

Wcoords <- data.frame(xcoordsW, ycoordsW)
Ecoords <- data.frame(xcoordsE, ycoordsE)
coordinates(Wcoords) = ~xcoordsW + ycoordsW
coordinates(Ecoords) = ~xcoordsE + ycoordsE

EWlines <- list()
for (i in 1:nrow(Wcoords@coords)){ EWlines[[i]] <- Lines(list(Line(coordinates(rbind(Wcoords[i,],Ecoords[i,])))), ID =  paste("X_",i, sep="") )
}

EWlinesSL <- SpatialLines(EWlines)
EWlinesSL@proj4string <- shape@proj4string
plot(EWlinesSL)

## Combine both NS and EW lines into one Spatial Lines object
twokm.lines <- list()
twokm.lines[[1]] <- NSlinesSL
twokm.lines[[2]] <- EWlinesSL
all.2km.lines <- do.call(rbind, twokm.lines)
plot(all.2km.lines)

## Convert to spatial lines data frame for writing
df<-SpatialLinesDataFrame(all.2km.lines, data.frame(id=1:length(all.2km.lines)), match.ID = F)
writeOGR(df, dsn="outputs/2km_Cyprus.shp" ,layer="2km", driver="ESRI Shapefile")

## Create KML as well
epsg4326String <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
dfGMap <- spTransform(df, epsg4326String)
writeOGR(dfGMap, dsn="outputs/2km_Cyprus_KML.kml" ,layer="2km", driver="KML")


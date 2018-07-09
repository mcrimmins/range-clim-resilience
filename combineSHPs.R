# combine region shapefiles
# MAC 6/26/18

library(sp)
library(rgdal)
#library(rgeos)
#library(maptools)
library(raster)


# shps <- dir("./regions/AllRegions", "*.shp", full.names = TRUE)
#  shps<-shps[-grep(".xml", shps)]
# regions<-do.call(rbind, lapply(shps[c(1,2,3,4,6,8)], rgdal::readOGR))
# # reproject data
# regionsLL<- spTransform(regions, CRS("+proj=longlat +datum=WGS84"))
# 
# #writeOGR(regionsLL, dsn = getwd(), layer = "regionsLL", driver="ESRI Shapefile")
# spplot(regionsLL, "FORM_LAB")

# test out zonal stats
# load("USDMRaster_2001_2018_16km.RData")
# prismGrid<-raster("prismGrid.grd")
# prismGrid<-aggregate(prismGrid, fact=4, fun=mean)
# rRegions <- rasterize(regionsLL, prismGrid, 'FORM_LAB')
# test<-zonal(tempGrid2, rRegions, 'mean')

# extract method
library(plyr)
load("USDMRaster_2001_2018_16km.RData")
shps <- dir("./regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
  regionNum<-1
  regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps[regionNum]))
regions<-do.call(rbind, lapply(shps[regionNum], rgdal::readOGR))
# reproject data
regionsLL<- spTransform(regions, CRS("+proj=longlat +datum=WGS84"))
# extract
extractUSDM <- (extract(tempGrid2, regionsLL, df=TRUE))
# build table of results
# empty df
tempDF<-data.frame(key=integer()) 
tempDF<-as.data.frame(c(NA,seq(0,4,1)))
  colnames(tempDF)[1]<-"key"
  #tempDF$key<-as.factor(tempDF$key)
# loop through cols
  for(i in 2:ncol(extractUSDM)){ 
      tempCol<-count(extractUSDM[,i])
      tempCol$freq<-tempCol$freq/sum(tempCol$freq)
      tempDF<-merge(tempDF,tempCol, by.x="key", by.y="x", all.x=TRUE)
    }
# clean up data frame
  tempDF<-as.data.frame(t(tempDF))
  tempDF<-tempDF[-1,] 
  colnames(tempDF)<-c("Dry","Moderate","Severe","Extreme","Exceptional","None")
  # add dates
  tempDF$week<-fileNames$date
  tempDF[is.na(tempDF)] <- 0
  # make stacked bar
  library(reshape2)
  library(ggplot2)
  tempDFmelt<-melt(tempDF,measure.vars = c("Dry","Moderate","Severe","Extreme","Exceptional","None"))
    tempDFmelt$variable <- factor(tempDFmelt$variable, levels = c("None","Dry","Moderate","Severe","Extreme","Exceptional"))
    tempDFmelt$value<-tempDFmelt$value*100
  ggplot(tempDFmelt, aes(x=week, y=value, fill=variable))+
    geom_area()+
    scale_fill_manual(values = c("white","yellow", "burlywood", "orange","red","firebrick"), name = "Category")+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    labs(title=paste0("USDM % area coverage for Region ",shps[regionNum]),
         x ="Date", y = "% of region covered")+
    theme_bw()

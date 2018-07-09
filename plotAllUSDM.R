# combine region shapefiles
# plot all USDM regions at one
# MAC 7/3/18

library(sp)
library(rgdal)
#library(rgeos)
#library(maptools)
library(raster)
library(plyr)

# load data
load("USDMRaster_2001_2018_16km.RData")

# get filenames 
shps <- dir("./regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]

# loop through all regions and create data frame
for(j in 1:length(shps)){
  # get shapes
  regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps[j]))
  regions<-do.call(rbind, lapply(shps[j], rgdal::readOGR))
  regionsLL<- spTransform(regions, CRS("+proj=longlat +datum=WGS84"))
  # extract
  print(paste0("extracting ",regionName))
  extractUSDM <- (extract(tempGrid2, regionsLL, df=TRUE))
  # build table of results
  tempDF<-data.frame(key=integer()) 
  tempDF<-as.data.frame(c(NA,seq(0,4,1)))
  colnames(tempDF)[1]<-"key"
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
  tempDF$region<-regionName
  
  # store tempGrids in stack  
  if (j==1){
    tempDFbind <- tempDF
  }else{
    tempDFbind <- rbind(tempDFbind, tempDF) # brick or stack?
  }
}

# save data file
#save(tempDFbind, file = "tempDFBind_allRegions.RData")
load("tempDFBind_allRegions.RData")

# make stacked bar
library(reshape2)
library(ggplot2)
# melt data frame
tempDFmelt<-melt(tempDFbind,measure.vars = c("Dry","Moderate","Severe","Extreme","Exceptional","None"))
tempDFmelt$variable <- factor(tempDFmelt$variable, levels = c("None","Dry","Moderate","Severe","Extreme","Exceptional"))
tempDFmelt$value<-tempDFmelt$value*100
# plot
ggplot(tempDFmelt, aes(x=week, y=value, fill=variable))+
  facet_wrap(~region, ncol = 1)+
  geom_area(alpha = 0.6)+
  scale_fill_manual(values = c("white","yellow", "burlywood", "orange","red","firebrick"), name = "Category")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  labs(title=("USDM % area coverage by Region"),
       x ="Date", y = "% of region covered")+
  theme_bw()
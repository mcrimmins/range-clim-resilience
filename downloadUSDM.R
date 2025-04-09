# download all years of USDM shapefiles
# MAC 6/19/18

#Set years
yr1<-2023
yr2<-2023

# set directories - only needed for CyVerse instances
dir.create("./tmpFiles")

for(i in yr1:yr2){
  paste0(i)
  
  # temporarily download year files and then delete
  # download test files; changed to "wget" from "curl"
  print("Downloading Yearly USDM Files")
  download.file(paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/",i,"_USDM_M.zip"), destfile = "./tmpFiles/USDM.zip", method="curl")
  print("Done downloading, extracting files")
  unzip("./tmpFiles/USDM.zip", exdir = "/scratch/crimmins/USDM/files",overwrite = TRUE)
  print("Done downloading, extracting files")
  do.call(file.remove, list(list.files("./tmpFiles", full.names = TRUE)))
} 


# unzip and process files
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
#library(cleangeo)

prismGrid<-raster("prismGrid.grd")
prismGrid<-aggregate(prismGrid, fact=4, fun=mean)

fileNames<-as.data.frame(list.files("/scratch/crimmins/USDM/files"))
colnames(fileNames)<-"files"
fileNames$date<-as.Date(substr(fileNames$files, 6,13), "%Y%m%d")
fileNames$code<-substr(fileNames$files, 6,13)

for(i in 1:nrow(fileNames)){
  unzip(paste0("/scratch/crimmins/USDM/files/",fileNames$files[i]),
        exdir = "./tmpFiles")
  tempUSDM <- readOGR(dsn = "./tmpFiles", layer = paste0("USDM_",fileNames$code[i]) )
  #tempUSDM <- readShapePoly(paste0("./tmpFiles/USDM_",fileNames$code[i],".shp")) # library maptools
  #tempUSDM <- clgeo_Clean(tempUSDM) # library cleangeo
  # rasterize
  tempGrid <- rasterize(tempUSDM, prismGrid, 'DM', fun='last')
  
  if (i==1){
    tempGrid2 <- tempGrid
  }else{
    tempGrid2 <- stack(tempGrid2, tempGrid) # brick or stack?
  }
  print(i)
  do.call(file.remove, list(list.files("./tmpFiles", full.names = TRUE)))
}

# set names and writeRaster
names(tempGrid2)<-fileNames$date
writeRaster(tempGrid2, filename = "USDMRaster_2000_2023_16km.grd", overwrite=TRUE)

# save datafiles
save(fileNames, tempGrid2, file = "USDMRaster_2000_2023_16km.RData")

names(tempGrid2)<-fileNames$date

# write out data file
#writeRaster(tempGrid2,filename="USDM2001_2018.grd")

# Analyze/Map Data (problem with rasterized maps 2000-2004)
load("USDMRaster_2000_2019_16km.RData")
library(rasterVis)
library(RColorBrewer)

# get state boundaries
states <- getData('GADM', country='United States', level=1)

names(tempGrid2)<-fileNames$date
cols <- brewer.pal(5, "YlOrRd")
week<-950
classUSDM<-as.factor(tempGrid2[[week]])
rat <- levels(classUSDM)[[1]]
# USDM categories
cats<-c("Dry","Moderate","Severe","Extreme","Exceptional")
rat[["cluster"]]<-cats[1:nrow(rat)]

levels(classUSDM) <- rat 

# plot classified map
levelplot(classUSDM, col.regions=cols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main=paste0("US Drought Monitor - ",fileNames$date[week]))+
  layer(sp.polygons(states))


# develop USDM like time series of D areas like http://droughtmonitor.unl.edu/Data/Timeseries.aspx
# Add seasons to the time series plots in quarters 

# load regions
regions <- readShapePoly("./western_states/western_states")


# rasterize USDM shapes to SW 
# adapted from downloadUSDM.R
# MAC 01/09/20

# unzip and process files
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
#library(cleangeo)

# get state boundaries for setting extents
states <- getData('GADM', country='United States', level=1)
  selectStates<-c('Arizona', 'New Mexico')
  subStates<-states[states$NAME_1 %in% selectStates,]
  
prismGrid<-raster("prismGrid.grd")
#prismGrid<-aggregate(prismGrid, fact=4, fun=mean)

fileNames<-as.data.frame(list.files("/scratch/crimmins/USDM/files"))
colnames(fileNames)<-"files"
fileNames$date<-as.Date(substr(fileNames$files, 6,13), "%Y%m%d")
fileNames$code<-substr(fileNames$files, 6,13)

# empty stack
tempGrid2 <- stack()

for(i in 1:nrow(fileNames)){
  unzip(paste0("/scratch/crimmins/USDM/files/",fileNames$files[i]),
        exdir = "./tmpFiles")
  tempUSDM <- readOGR(dsn = "./tmpFiles", layer = paste0("USDM_",fileNames$code[i]) )
  #tempUSDM <- readShapePoly(paste0("./tmpFiles/USDM_",fileNames$code[i],".shp")) # library maptools
  #tempUSDM <- clgeo_Clean(tempUSDM) # library cleangeo
  # rasterize
  tempGrid <- rasterize(tempUSDM, prismGrid, 'DM', fun='last')
  # reduce extent to SW
  tempGrid <- crop(tempGrid, extent(subStates))
  # build stack  
  tempGrid2 <- stack( tempGrid2 , tempGrid )
  
  print(i)
  do.call(file.remove, list(list.files("./tmpFiles", full.names = TRUE)))
}

# set names and writeRaster
names(tempGrid2)<-fileNames$date
writeRaster(tempGrid2, filename = "SW_USDMRaster_2000_2023_4km.grd", overwrite=TRUE)

# save datafiles
save(fileNames, tempGrid2, file = "SW_USDMRaster_2000_2023_4km.RData")
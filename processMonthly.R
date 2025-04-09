# combine monthly livneh data in raster stacks
# MAC 08/2/18


# precip in mm, temp in C

library(raster)

fileNames<-as.data.frame(list.files("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly"))
  colnames(fileNames)<-"files"
  fileNames$date<-as.Date(paste0(substr(fileNames$files, 26,29),substr(fileNames$files, 30,31),"01"), "%Y%m%d")

for(i in 1:nrow(fileNames)){
  monthFile<-paste0("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly/",fileNames$files[i])
  precTemp <- raster(monthFile, varname="Prec",  ncdf=TRUE)
  tminTemp <- raster(monthFile, varname="Tmin",  ncdf=TRUE)
  tmaxTemp <- raster(monthFile, varname="Tmax",  ncdf=TRUE)
  
  if (i==1){
    prec<- precTemp
    tmin<- tminTemp
    tmax<- tmaxTemp
  }else{
    prec <- stack(prec, precTemp)
    tmin <- stack(tmin, tminTemp) 
    tmax <- stack(tmax, tmaxTemp) 
  }
  #print(i)
}
  
# write to file
  # write out tempGrid2 stack -- large file and may not be necessary
  writeRaster(prec,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_prec_1915_2015.grd", overwrite=TRUE )
  writeRaster(tmin,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_tmin_1915_2015.grd", overwrite=TRUE )
  writeRaster(tmax,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_tmax_1915_2015.grd", overwrite=TRUE )
  
# western US only
  
  e <- extent(-125, -97, 25, 49)
  prec <- crop(prec, e)	
  tmin <- crop(tmin, e)	
  tmax <- crop(tmax, e)	
  
  writeRaster(prec,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd", overwrite=TRUE )
  writeRaster(tmin,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd", overwrite=TRUE )
  writeRaster(tmax,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd", overwrite=TRUE )
  
   
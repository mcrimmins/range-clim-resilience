# combine region shapefiles
# plot all USDM regions at one
# MAC 7/3/18

library(sp)
library(rgdal)
#library(rgeos)
#library(maptools)
library(raster)
library(plyr)
#library(maps)
library(ggplot2)

# load data
load("/home/crimmins/RProjects/USDM/SW_USDMRaster_2000_2019_4km.RData")

# FIPS codes
#data(county.fips)
#  countyDf <- map_data('county', 'arizona')
#  countyList<-unique(countyDf$subregion)
  
# get boundary
  us<-getData('GADM', country='USA', level=2)  
  counties<-subset(us, NAME_1=="Arizona")
  
# get filenames 
#shps <- dir("./regions/AllRegions", "*.shp", full.names = TRUE)
#shps<-shps[-grep(".xml", shps)]

# loop through all regions and create data frame
for(j in 1:nrow(counties)){
  # get shapes
  regionName<-counties$NAME_2[j]
    regions<-subset(counties, NAME_2==counties$NAME_2[j])
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
save(tempDFbind, file = "tempDFBind_AZ_USDM.RData")
load("tempDFBind_AZ_USDM.RData")

# make stacked bar
library(reshape2)
library(ggplot2)
# melt data frame
tempDFmelt<-melt(tempDFbind,measure.vars = c("Dry","Moderate","Severe","Extreme","Exceptional","None"))
tempDFmelt$variable <- factor(tempDFmelt$variable, levels = c("None","Dry","Moderate","Severe","Extreme","Exceptional"))
tempDFmelt$value<-tempDFmelt$value*100
# plot
p<-ggplot(tempDFmelt, aes(x=week, y=value, fill=variable))+
  facet_wrap(~region, ncol = 1)+
  geom_area(alpha = 0.6)+
  scale_fill_manual(values = c("white","yellow", "burlywood", "orange","red","firebrick"), name = "Category")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", limits=c(as.Date("2008-01-01"),as.Date("2018-12-31")))+
  labs(title=("USDM % area coverage by Region"),
       x ="Date", y = "% of region covered")+
  theme_bw()

# plot to png
 png("/home/crimmins/RProjects/DroughtEcon/AZ_USDM_2008_2018.png", width =8.5, height = 11, units = "in", res = 300L)
# #grid.newpage()
 print(p, newpage = FALSE)
 dev.off()
 
 # write to file
 write.csv(tempDFbind, file="AZ_USDM_2000_2019.csv", row.names = FALSE)
 
 
 # analyze USDM LFP triggers
 load("tempDFBind_AZ_USDM.RData")
 library(tidyr)
 library(dplyr)
 
 # find triggers
 tempDFbind$LFPsev<-ifelse(tempDFbind$Severe > 0 | tempDFbind$Extreme > 0 | tempDFbind$Exceptional > 0, 1,0)
 tempDFbind$LFPext<-ifelse(tempDFbind$Extreme > 0 | tempDFbind$Exceptional > 0, 1,0)
 tempDFbind$LFPexc<-ifelse(tempDFbind$Exceptional > 0, 1,0)
 # consecutive
  tempDFbind$LFPsev_consec<-ave(tempDFbind$LFPsev, cumsum((tempDFbind$LFPsev) == 0), FUN=seq_along) - 1
    tempDFbind$LFPsev_consec<-ifelse(tempDFbind$LFPsev_consec>=8,1,0)
  tempDFbind$LFPext_consec<-ave(tempDFbind$LFPext, cumsum((tempDFbind$LFPext) == 0), FUN=seq_along) - 1
    tempDFbind$LFPext_consec<-ifelse(tempDFbind$LFPext_consec>=4,1,0)
  
  # re-categorize into payment levels
  tempDFbind$pay1<-ifelse(tempDFbind$LFPsev_consec==1,1,0)  
  tempDFbind$pay3<-ifelse(tempDFbind$LFPext==1,3,0)  
  tempDFbind$pay4<-ifelse((tempDFbind$LFPext_consec+tempDFbind$LFPexc)>0,4,0)
  tempDFbind$maxPay<-apply(tempDFbind[, 14:16], 1, max)
  tempDFbind$year<-as.numeric(format(tempDFbind$week, "%Y"))
  
  # Count of exceptional weeks
  weeksPayments <- tempDFbind %>%
                    group_by(region,year) %>%
                    summarise(sumExc=sum(LFPexc))
  
  weeksPayments<-spread(weeksPayments, region, sumExc)
  
  sumPayments <- tempDFbind %>%
                    group_by(region,year) %>%
                    summarise(pay1sum = sum(pay1==1),
                              pay3sum = sum(pay3==3),
                              pay4sum = sum(pay4==4),
                              pay5sum = sum(LFPexc))
  sumPayments <- gather(sumPayments, key="var", value="value", pay1sum:pay5sum)
  
  ggplot(data=sumPayments) + 
    geom_col(mapping = aes(x=(year),value, fill = as.factor(var)),
             position="dodge")+
    facet_wrap(~region)+
    ggtitle("Counts of weeks in payment categories")+
    ylab("weeks")
  
  # total weeks
  totPayments <- tempDFbind %>%
    group_by(region) %>%
    summarise(pay1sum = round((sum(pay1==1)/1040)*100,0),
              pay3sum = round((sum(pay3==3)/1040)*100,0),
              pay4sum = round((sum(pay4==4)/1040)*100,0),
              pay5sum = round((sum(LFPexc)/1040)*100,0))
  totPayments<-gather(totPayments, key="var", value="value", pay1sum:pay5sum)
  
  ggplot(data=totPayments) + 
    geom_col(mapping = aes(x=as.factor(region),value, fill = as.factor(var)),
             position="dodge")+
    ggtitle("% of weeks in payment categories: 2000-2019")+
    ylab("% of weeks")

  # total weeks without double counting
  totPaymentsYear <- tempDFbind %>%
    group_by(region,year) %>%
    summarise(pay1sum = sum(pay1==1),
              pay3sum = sum(pay3==3),
              pay4sum = sum(pay4==4),
              pay5sum = sum(LFPexc))
  totPaymentsYear$pay1sum<-ifelse(totPaymentsYear$pay1sum==53, 52, totPaymentsYear$pay1sum) 
  totPaymentsYear[totPaymentsYear == 0] <- NA
  
  # gather into new groups
  temp <- gather(totPaymentsYear, key="var", value="value", pay1sum:pay5sum)
    regionYear<- temp %>% group_by(region,year) %>% slice_min(value)
    regionYear$paymentLev<-as.numeric(substr(regionYear$var, 4,4))
    regionYearMax<- regionYear %>% group_by(region, year) %>% slice_max(paymentLev)
  
    regionMax<-regionYearMax %>% group_by(region, var) %>% summarize(sumWeeks=round(sum(value)/1040,3)*100)

     ggplot(data=regionMax) + 
       geom_col(mapping = aes(x=as.factor(region),sumWeeks, fill = as.factor(var)))+
       scale_fill_manual(values = c("lightyellow", "yellow", "darkgoldenrod","firebrick"), name="Payment Level")+
       ylab("Percent weeks: 2000-2019")+
       xlab("AZ County")
        
    # ggplot(data=regionYearMax) + 
    #   geom_col(mapping = aes(x=as.factor(region),value, fill = as.factor(var)),
    #            position="dodge")+
    #   ggtitle("% of weeks in payment categories: 2000-2019")+
    #   ylab("% of weeks") 
    
    ggplot(data=regionYearMax) + 
      geom_col(mapping = aes(x=year,value, group=year, fill = as.factor(var)),
               position = "dodge")+
      facet_grid(~as.factor(region))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  
  # thinned out dataframe
  payments<-tempDFbind[,c(7,8,17)]
  payments$week<-as.POSIXct(payments$week)
  payments$year<-as.numeric(format(payments$week, "%Y"))
  payments$doy<-as.numeric(format(payments$week, "%j"))
  payments$weekNum<-as.numeric(format(payments$week, "%W"))
  

    # heatmap
  library(ggplot2)
  library(scales)
  #library(lubridate)
  # payments$week <- ymd(payments$week)
  ggplot(payments, aes(region, week, fill= as.factor(maxPay))) + 
    geom_tile()+
  scale_fill_manual(values = c("white", "yellow", "orange","red"))+
    scale_y_datetime(date_breaks = "12 month")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  ggplot(payments, aes(weekNum, year, fill= as.factor(maxPay))) + 
    geom_tile()+
    facet_wrap(~region)+
    scale_fill_manual(values = c("white", "yellow", "orange","red"))+
    #scale_y_datetime(date_breaks = "12 month")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
 #longUSDM<-gather(tempDFbind, usdmCat, percCov, Dry:None)

 # count of occurrences
  countPayments <- payments %>%
                    group_by(region) %>%
                    count(maxPay)
 
 ggplot(countPayments, aes(x=region, y=n), fill=factor(maxPay))+
   geom_bar(stat = "identity")
  
 ggplot(countPayments, aes(as.factor(region), n, fill=as.factor(maxPay)), )+
   geom_col(stat='identity')
 
 ggplot(data=countPayments) + 
   geom_col(mapping = aes(x=as.factor(region),n, fill = as.factor(maxPay)),
            position="dodge")+
   ggtitle("Counts of weeks in payment categories")
 
 
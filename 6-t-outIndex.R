# This script is for validation by SNOTEL SWE observations.
# SNOTEL stations are selected and the shapefile would be used to extract from the predicted rasters.

library(spdep)
library(foreign)
library(caret)
library(car)
library(tidyverse)
foldname <- 'mon'

#######################
# ESFs interpolation
#######################
for (mon in c(1,2,4)) {
  mname <- paste('esf.',as.character(mon),sep = '')
  model <- get(mname)
  tmp.var <- names(model$coefficients)[-1]
  evnames <- tmp.var[which(str_detect(tmp.var,'EV')==TRUE)]
  ESFs <- cbind(meigen[[mon]][,c("CODE","Geom_X","Geom_Y")],as.matrix(meigen[[mon]][,evnames])%*%as.vector(model$coefficients[evnames]))
  write.csv(ESFs,file = paste('D:/chenyuejun/SWE/RS/paper/ESFs.figure/ESFs',as.character(mon),'.csv',sep = ''))
}


####################
# VIF test
####################
for (mon in c(1,2,4)) {
  mname <- paste('esf.',as.character(mon),sep = '')
  model <- get(mname)
  tmp.var <- names(model$coefficients)[-1]
  tmp.lm<-lm(SNWD~.,data = modeldataEV[[mon]][,c('SNWD',tmp.var)])
  print(vif(tmp.lm))
  write.csv(vif(tmp.lm),file = paste('D:/chenyuejun/SWE/RS/tmp_file/',foldname,'/vif/m',as.character(mon),'.csv',sep = ''))
}



######################################################################################################
# Read raw SNOTEL dataset, select SNOTEL stations, export as shapefile to extract daiily predicted rasters.
# read the SNOTEL.csv joined from  SWE and SD.
# Both units: inches. inches (in) = 2.54 cm
# 1 feet = 0.305 meters
snotel<-read.csv('D:/newdata/rawdata/SNOTEL/csv/SNOTEL_SWE_SD_coordi_noclp.csv')
summary(snotel)
snotel$Elevation<-snotel$Elevation*0.305

# convert the YYYY/MM/DD to YYYYDDD
Year<-format(as.Date(snotel$DATE),"%Y")
JulianDay <- format(as.Date(snotel$DATE), "%j") 
snotel$JDate<-as.numeric(paste(Year,JulianDay,sep = ""))


# select the records only within the 11 States in West U.S.
snotel<-snotel[snotel$State %in% c("California",  "Utah",        "Montana",     "Washington",  "Oregon",      "Colorado",   
                                   "Idaho",       "Arizona",     "Wyoming",     "New Mexico",  "Nevada"),]
unique(snotel$State)

# calculate snow density by SNOTEL, remove records whose snow density equals 0, Inf, and greater than 1.
snotel$Denst<-snotel$SWE/snotel$SD
hist(snotel$Denst)
snotel$Denst[is.infinite(snotel$Denst)]<-NA
snotel$Denst[(snotel$Denst)==0]<-NA


# # remove records whose snow depth greater than 50 cm/19.685 in. 
# snotel$SD[snotel$SD>19.685]<-NA
snotel$SD[snotel$SD>39.37]<-NA #remove SD >100cm


snotel<-na.omit(snotel)
snotel$DATE <-as.integer(substr(snotel$JDate,3,7))
snotel$code <- as.integer(snotel$code)
rm(Year,JulianDay)
summary(snotel$SD)
summary(snotel$SWE)
summary(snotel$Denst)

# out SNOTEL points
# snotel.station <- unique(snotel[,c("station","Elevation","Latitude","Longitude","lonlat")])
# write.csv(snotel.station,'D:/chenyuejun/SWE/RS/tmp_file/snotel/snotel.station.csv')
# read data
esf <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/esf/snotel_addlandform.csv',sep = ''))
ols <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/ols/snotel_addlandform.csv',sep = ''))
AMSR2 <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/AMSR2/snotel_addlandform.csv',sep = ''))
ERA5 <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/ERA5/snotel_addlandform.csv',sep = ''))
Globsnow3 <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/Globsnow3/snotel_addlandform.csv',sep = ''))
NCAswe <- read.csv(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/NCAswe/snotel_addlandform.csv',sep = ''))
# vld.ml<-merge(lmer_swe[,c("station","code","State","Elevation","Latitude","Longitude","lonlat","MLC_2011","MLC_2012","MLC_2013","MLC_2014","MLC_2015","MLC_2016","lmer","DATE")],esf[,c("station","code","esf","DATE")],by=c("station","code","DATE"))
vld.ml<-merge(esf[,c("station","Elevation","Latitude","Longitude","lonlat","esf","DATE","Landform1k")],ols[,c("station","ols","DATE")],by=c("station","DATE"))
vld.ml<-merge(vld.ml,AMSR2[,c("station","AMSR2","DATE")],by=c("station","DATE"))
vld.ml<-merge(vld.ml,ERA5[,c("station","ERA5","DATE")],by=c("station","DATE"))
vld.ml<-merge(vld.ml,Globsnow3[,c("station","Globsnow3","DATE")],by=c("station","DATE"))
vld.ml<-merge(vld.ml,NCAswe[,c("station","NCAswe","DATE")],by=c("station","DATE"))
vld.ml <- merge(vld.ml,snotel[,c("station","SWE","DATE")],by=c("station","DATE"))

snotel.addlandform <- read.dbf('D:/chenyuejun/SWE/RS/tmp_file/mon/snotel/snotel_addlandform.dbf')
vld.ml.addlandform <- merge(snotel.addlandform[,c('station', 'MLC_2011', 'MLC_2012', 'MLC_2013', 'MLC_2014', 'MLC_2015', 'MLC_2016')],vld.ml,by=c("station"),all.y = TRUE) 
vld.ml.addlandform[,'MLC'] <- NA
vld.ml.addlandform[which(substr(vld.ml.addlandform$DATE,1,2)=='15'),'MLC'] <- vld.ml.addlandform[which(substr(vld.ml.addlandform$DATE,1,2)=='15'),'MLC_2015']
vld.ml.addlandform[which(substr(vld.ml.addlandform$DATE,1,2)=='16'),'MLC'] <- vld.ml.addlandform[which(substr(vld.ml.addlandform$DATE,1,2)=='16'),'MLC_2016']
vld.ml.addlandform[,'MLC.re'] <- NA
vld.ml.addlandform[which(vld.ml.addlandform$MLC==0),'MLC.re'] <- 0
vld.ml.addlandform[which(vld.ml.addlandform$MLC==1),'MLC.re'] <- 1
vld.ml.addlandform[which(vld.ml.addlandform$MLC==8),'MLC.re'] <- 3
vld.ml.addlandform[which(vld.ml.addlandform$MLC==9),'MLC.re'] <- 3
vld.ml.addlandform[which(vld.ml.addlandform$MLC==10),'MLC.re'] <- 4

######################################################################################################
# Calculate criteria: R, RMSE, MAE, PositiveError and NegativeError.
delete_date <- c()
for (year in c(2015,2016)) {
  if(year==2015){
    for (day in c(60:90)) {
      delete_date <- c(delete_date,paste('15',sprintf("%03d",day),sep = ''))
    }
  }
  if(year==2016){
    for (day in c(61:91)) {
      delete_date <- c(delete_date,paste('16',sprintf("%03d",day),sep = ''))
    }
  }
}
vld.ml1 <- vld.ml.addlandform[-which(as.character(vld.ml.addlandform$DATE)%in%delete_date),]

vdata<-vld.ml1
vdata$SWE<-vdata$SWE*25.4
vdata$ERA5<-vdata$ERA5*1000
vdata[vdata==-9999] <- NA
colnames(vdata)
dim(vdata)
summary(vdata)

################################################# 
vdata_y <- vdata[which(vdata$Landform1k<12),]
vdata_y[vdata_y==-9999] <- NA
vdata_y[which(vdata_y$ols<12),] <- NA
vdata_y[which(vdata_y$esf<12),] <- NA
vdata_y[which(vdata_y$Globsnow3<0),] <- NA
vdata_y<-na.omit(vdata_y)
summary(vdata_y)


dif <- vdata_y
dif[,'dif_esf'] <- vdata_y$esf-vdata_y$SWE
esf_view <- aggregate(dif$dif_esf, by=list(type=dif$station),mean)
delvaliP<- esf_view[which(abs(esf_view$x)>80),]$type
# # out delvaliP
# snotel.stations<-unique(snotel[c(3,4,6,7,8,9,10,11,12)])
# outdelP <- snotel.stations[which(snotel.stations$station%in%delvaliP),]
# write.csv(outdelP,'E:/lmer_oridata_test2/delvaliP50.csv')

summary(vdata_y)
vdata_y<-na.omit(vdata_y)
summary(vdata_y)
vdata_y <- vdata_y[-which(vdata_y$station%in%delvaliP),]
length(unique(vdata_y$station))

##############################################
criteria<-data.frame()

criteria['ols','RMSE']<-RMSE(vdata_y$ols,vdata_y$SWE)
criteria['esf','RMSE']<-RMSE(vdata_y$esf,vdata_y$SWE)
criteria['AMSR2','RMSE']<-RMSE(vdata_y$AMSR2,vdata_y$SWE)
criteria['ERA5','RMSE']<-RMSE(vdata_y$ERA5,vdata_y$SWE)
criteria['Globsnow3','RMSE']<-RMSE(vdata_y$Globsnow3,vdata_y$SWE)
criteria['NCAswe','RMSE']<-RMSE(vdata_y$NCAswe,vdata_y$SWE)

criteria['ols','MAE']<-MAE(vdata_y$ols,vdata_y$SWE)
criteria['esf','MAE']<-MAE(vdata_y$esf,vdata_y$SWE)
criteria['AMSR2','MAE']<-MAE(vdata_y$AMSR2,vdata_y$SWE)
criteria['ERA5','MAE']<-MAE(vdata_y$ERA5,vdata_y$SWE)
criteria['Globsnow3','MAE']<-MAE(vdata_y$Globsnow3,vdata_y$SWE)
criteria['NCAswe','MAE']<-MAE(vdata_y$NCAswe,vdata_y$SWE)

criteria['ols','cor']<-cor(vdata_y$ols,vdata_y$SWE)
criteria['esf','cor']<-cor(vdata_y$esf,vdata_y$SWE)
criteria['AMSR2','cor']<-cor(vdata_y$AMSR2,vdata_y$SWE)
criteria['ERA5','cor']<-cor(vdata_y$ERA5,vdata_y$SWE)
criteria['Globsnow3','cor']<-cor(vdata_y$Globsnow3,vdata_y$SWE)
criteria['NCAswe','cor']<-cor(vdata_y$NCAswe,vdata_y$SWE)


criteria['ols','PE']<-mean(as.vector(vdata_y$ols-vdata_y$SWE)[vdata_y$ols-vdata_y$SWE>0])
criteria['esf','PE']<-mean(as.vector(vdata_y$esf-vdata_y$SWE)[vdata_y$esf-vdata_y$SWE>0])
criteria['ERA5','PE']<-mean(as.vector(vdata_y$ERA5-vdata_y$SWE)[vdata_y$ERA5-vdata_y$SWE>0])
criteria['AMSR2','PE']<-mean(as.vector(vdata_y$AMSR2-vdata_y$SWE)[vdata_y$AMSR2-vdata_y$SWE>0])
criteria['Globsnow3','PE']<-mean(as.vector(vdata_y$Globsnow3-vdata_y$SWE)[vdata_y$Globsnow3-vdata_y$SWE>0])
criteria['NCAswe','PE']<-mean(as.vector(vdata_y$NCAswe-vdata_y$SWE)[vdata_y$NCAswe-vdata_y$SWE>0])

criteria['ols','NE']<-mean(as.vector(vdata_y$ols-vdata_y$SWE)[vdata_y$ols-vdata_y$SWE<0])
criteria['esf','NE']<-mean(as.vector(vdata_y$esf-vdata_y$SWE)[vdata_y$esf-vdata_y$SWE<0])
criteria['ERA5','NE']<-mean(as.vector(vdata_y$ERA5-vdata_y$SWE)[vdata_y$ERA5-vdata_y$SWE<0])
criteria['AMSR2','NE']<-mean(as.vector(vdata_y$AMSR2-vdata_y$SWE)[vdata_y$AMSR2-vdata_y$SWE<0])
criteria['Globsnow3','NE']<-mean(as.vector(vdata_y$Globsnow3-vdata_y$SWE)[vdata_y$Globsnow3-vdata_y$SWE<0])
criteria['NCAswe','NE']<-mean(as.vector(vdata_y$NCAswe-vdata_y$SWE)[vdata_y$NCAswe-vdata_y$SWE<0])

criteria['ols','corp']<-(cor.test(vdata_y$ols,vdata_y$SWE))[["p.value"]]
criteria['esf','corp']<-(cor.test(vdata_y$esf,vdata_y$SWE))[["p.value"]]
criteria['ERA5','corp']<-(cor.test(vdata_y$ERA5,vdata_y$SWE))[["p.value"]]
criteria['AMSR2','corp']<-(cor.test(vdata_y$AMSR2,vdata_y$SWE))[["p.value"]]
criteria['Globsnow3','corp']<-(cor.test(vdata_y$Globsnow3,vdata_y$SWE))[["p.value"]]
criteria['NCAswe','corp']<-(cor.test(vdata_y$NCAswe,vdata_y$SWE))[["p.value"]]

criteria
plot(vdata_y$ols,vdata_y$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y$esf,vdata_y$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y$ERA5,vdata_y$SWE,ylim=c(0,500),xlim=c(0,500))

criteria


###############################################
# mon 1
###############################################
##############################################
vdata_y1 <- vdata_y[which(as.integer(substr(vdata_y$DATE,3,5))>0&as.integer(substr(vdata_y$DATE,3,5))<=31),]
criteria1<-data.frame()
criteria1['ols','RMSE']<-RMSE(vdata_y1$ols,vdata_y1$SWE)
criteria1['esf','RMSE']<-RMSE(vdata_y1$esf,vdata_y1$SWE)
criteria1['ERA5','RMSE']<-RMSE(vdata_y1$ERA5,vdata_y1$SWE)
criteria1['AMSR2','RMSE']<-RMSE(vdata_y1$AMSR2,vdata_y1$SWE)
criteria1['Globsnow3','RMSE']<-RMSE(vdata_y1$Globsnow3,vdata_y1$SWE)
criteria1['NCAswe','RMSE']<-RMSE(vdata_y1$NCAswe,vdata_y1$SWE)

criteria1['ols','MAE']<-MAE(vdata_y1$ols,vdata_y1$SWE)
criteria1['esf','MAE']<-MAE(vdata_y1$esf,vdata_y1$SWE)
criteria1['ERA5','MAE']<-MAE(vdata_y1$ERA5,vdata_y1$SWE)
criteria1['AMSR2','MAE']<-MAE(vdata_y1$AMSR2,vdata_y1$SWE)
criteria1['Globsnow3','MAE']<-MAE(vdata_y1$Globsnow3,vdata_y1$SWE)
criteria1['NCAswe','MAE']<-MAE(vdata_y1$NCAswe,vdata_y1$SWE)

criteria1['ols','cor']<-cor(vdata_y1$ols,vdata_y1$SWE)
criteria1['esf','cor']<-cor(vdata_y1$esf,vdata_y1$SWE)
criteria1['ERA5','cor']<-cor(vdata_y1$ERA5,vdata_y1$SWE)
criteria1['AMSR2','cor']<-cor(vdata_y1$AMSR2,vdata_y1$SWE)
criteria1['Globsnow3','cor']<-cor(vdata_y1$Globsnow3,vdata_y1$SWE)
criteria1['NCAswe','cor']<-cor(vdata_y1$NCAswe,vdata_y1$SWE)



criteria1['ols','PE']<-mean(as.vector(vdata_y1$ols-vdata_y1$SWE)[vdata_y1$ols-vdata_y1$SWE>0])
criteria1['esf','PE']<-mean(as.vector(vdata_y1$esf-vdata_y1$SWE)[vdata_y1$esf-vdata_y1$SWE>0])
criteria1['ERA5','PE']<-mean(as.vector(vdata_y1$ERA5-vdata_y1$SWE)[vdata_y1$ERA5-vdata_y1$SWE>0])
criteria1['AMSR2','PE']<-mean(as.vector(vdata_y1$AMSR2-vdata_y1$SWE)[vdata_y1$AMSR2-vdata_y1$SWE>0])
criteria1['Globsnow3','PE']<-mean(as.vector(vdata_y1$Globsnow3-vdata_y1$SWE)[vdata_y1$Globsnow3-vdata_y1$SWE>0])
criteria1['NCAswe','PE']<-mean(as.vector(vdata_y1$NCAswe-vdata_y1$SWE)[vdata_y1$NCAswe-vdata_y1$SWE>0])

criteria1['ols','NE']<-mean(as.vector(vdata_y1$ols-vdata_y1$SWE)[vdata_y1$ols-vdata_y1$SWE<0])
criteria1['esf','NE']<-mean(as.vector(vdata_y1$esf-vdata_y1$SWE)[vdata_y1$esf-vdata_y1$SWE<0])
criteria1['ERA5','NE']<-mean(as.vector(vdata_y1$ERA5-vdata_y1$SWE)[vdata_y1$ERA5-vdata_y1$SWE<0])
criteria1['AMSR2','NE']<-mean(as.vector(vdata_y1$AMSR2-vdata_y1$SWE)[vdata_y1$AMSR2-vdata_y1$SWE<0])
criteria1['Globsnow3','NE']<-mean(as.vector(vdata_y1$Globsnow3-vdata_y1$SWE)[vdata_y1$Globsnow3-vdata_y1$SWE<0])
criteria1['NCAswe','NE']<-mean(as.vector(vdata_y1$NCAswe-vdata_y1$SWE)[vdata_y1$NCAswe-vdata_y1$SWE<0])


criteria1['ols','corp']<-(cor.test(vdata_y1$ols,vdata_y1$SWE))[["p.value"]]
criteria1['esf','corp']<-(cor.test(vdata_y1$esf,vdata_y1$SWE))[["p.value"]]
criteria1['ERA5','corp']<-(cor.test(vdata_y1$ERA5,vdata_y1$SWE))[["p.value"]]
criteria1['AMSR2','corp']<-(cor.test(vdata_y1$AMSR2,vdata_y1$SWE))[["p.value"]]
criteria1['Globsnow3','corp']<-(cor.test(vdata_y1$Globsnow3,vdata_y1$SWE))[["p.value"]]
criteria1['NCAswe','corp']<-(cor.test(vdata_y1$NCAswe,vdata_y1$SWE))[["p.value"]]

criteria1

plot(vdata_y1$ERA5,vdata_y1$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y1$esf,vdata_y1$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y1$ols,vdata_y1$SWE,ylim=c(0,500),xlim=c(0,500))

vdata_y2 <- vdata_y[which(as.integer(substr(vdata_y$DATE,3,5))>31&as.integer(substr(vdata_y$DATE,3,5))<=60),]
criteria2<-data.frame()
criteria2['ols','RMSE']<-RMSE(vdata_y2$ols,vdata_y2$SWE)
criteria2['esf','RMSE']<-RMSE(vdata_y2$esf,vdata_y2$SWE)
criteria2['ERA5','RMSE']<-RMSE(vdata_y2$ERA5,vdata_y2$SWE)
criteria2['AMSR2','RMSE']<-RMSE(vdata_y2$AMSR2,vdata_y2$SWE)
criteria2['Globsnow3','RMSE']<-RMSE(vdata_y2$Globsnow3,vdata_y2$SWE)
criteria2['NCAswe','RMSE']<-RMSE(vdata_y2$NCAswe,vdata_y2$SWE)

criteria2['ols','MAE']<-MAE(vdata_y2$ols,vdata_y2$SWE)
criteria2['esf','MAE']<-MAE(vdata_y2$esf,vdata_y2$SWE)
criteria2['ERA5','MAE']<-MAE(vdata_y2$ERA5,vdata_y2$SWE)
criteria2['AMSR2','MAE']<-MAE(vdata_y2$AMSR2,vdata_y2$SWE)
criteria2['Globsnow3','MAE']<-MAE(vdata_y2$Globsnow3,vdata_y2$SWE)
criteria2['NCAswe','MAE']<-MAE(vdata_y2$NCAswe,vdata_y2$SWE)

criteria2['ols','cor']<-cor(vdata_y2$ols,vdata_y2$SWE)
criteria2['esf','cor']<-cor(vdata_y2$esf,vdata_y2$SWE)
criteria2['ERA5','cor']<-cor(vdata_y2$ERA5,vdata_y2$SWE)
criteria2['AMSR2','cor']<-cor(vdata_y2$AMSR2,vdata_y2$SWE)
criteria2['Globsnow3','cor']<-cor(vdata_y2$Globsnow3,vdata_y2$SWE)
criteria2['NCAswe','cor']<-cor(vdata_y2$NCAswe,vdata_y2$SWE)

criteria2['ols','PE']<-mean(as.vector(vdata_y2$ols-vdata_y2$SWE)[vdata_y2$ols-vdata_y2$SWE>0])
criteria2['esf','PE']<-mean(as.vector(vdata_y2$esf-vdata_y2$SWE)[vdata_y2$esf-vdata_y2$SWE>0])
criteria2['ERA5','PE']<-mean(as.vector(vdata_y2$ERA5-vdata_y2$SWE)[vdata_y2$ERA5-vdata_y2$SWE>0])
criteria2['AMSR2','PE']<-mean(as.vector(vdata_y2$AMSR2-vdata_y2$SWE)[vdata_y2$AMSR2-vdata_y2$SWE>0])
criteria2['Globsnow3','PE']<-mean(as.vector(vdata_y2$Globsnow3-vdata_y2$SWE)[vdata_y2$Globsnow3-vdata_y2$SWE>0])
criteria2['NCAswe','PE']<-mean(as.vector(vdata_y2$NCAswe-vdata_y2$SWE)[vdata_y2$NCAswe-vdata_y2$SWE>0])

criteria2['ols','NE']<-mean(as.vector(vdata_y2$ols-vdata_y2$SWE)[vdata_y2$ols-vdata_y2$SWE<0])
criteria2['esf','NE']<-mean(as.vector(vdata_y2$esf-vdata_y2$SWE)[vdata_y2$esf-vdata_y2$SWE<0])
criteria2['ERA5','NE']<-mean(as.vector(vdata_y2$ERA5-vdata_y2$SWE)[vdata_y2$ERA5-vdata_y2$SWE<0])
criteria2['AMSR2','NE']<-mean(as.vector(vdata_y2$AMSR2-vdata_y2$SWE)[vdata_y2$AMSR2-vdata_y2$SWE<0])
criteria2['Globsnow3','NE']<-mean(as.vector(vdata_y2$Globsnow3-vdata_y2$SWE)[vdata_y2$Globsnow3-vdata_y2$SWE<0])
criteria2['NCAswe','NE']<-mean(as.vector(vdata_y2$NCAswe-vdata_y2$SWE)[vdata_y2$NCAswe-vdata_y2$SWE<0])


criteria2['ols','corp']<-(cor.test(vdata_y2$ols,vdata_y2$SWE))[["p.value"]]
criteria2['esf','corp']<-(cor.test(vdata_y2$esf,vdata_y2$SWE))[["p.value"]]
criteria2['ERA5','corp']<-(cor.test(vdata_y2$ERA5,vdata_y2$SWE))[["p.value"]]
criteria2['AMSR2','corp']<-(cor.test(vdata_y2$AMSR2,vdata_y2$SWE))[["p.value"]]
criteria2['Globsnow3','corp']<-(cor.test(vdata_y2$Globsnow3,vdata_y2$SWE))[["p.value"]]
criteria2['NCAswe','corp']<-(cor.test(vdata_y2$NCAswe,vdata_y2$SWE))[["p.value"]]

criteria2
plot(vdata_y2$ERA5,vdata_y2$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y2$esf,vdata_y2$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y2$ols,vdata_y2$SWE,ylim=c(0,500),xlim=c(0,500))


# vdata_y3 <- vdata_y[which(as.integer(substr(vdata_y$DATE,3,5))>60&as.integer(substr(vdata_y$DATE,3,5))<=100),]
# criteria3<-data.frame()
# criteria3['ols','RMSE']<-RMSE(vdata_y3$ols,vdata_y3$SWE)
# criteria3['esf','RMSE']<-RMSE(vdata_y3$esf,vdata_y3$SWE)
# criteria3['ERA5','RMSE']<-RMSE(vdata_y3$ERA5,vdata_y3$SWE)
# criteria3['Globsnow3','RMSE']<-RMSE(vdata_y3$Globsnow3,vdata_y3$SWE)
# criteria3['NCAswe','RMSE']<-RMSE(vdata_y3$NCAswe,vdata_y3$SWE)
# 
# criteria3['ols','MAE']<-MAE(vdata_y3$ols,vdata_y3$SWE)
# criteria3['esf','MAE']<-MAE(vdata_y3$esf,vdata_y3$SWE)
# criteria3['ERA5','MAE']<-MAE(vdata_y3$ERA5,vdata_y3$SWE)
# criteria3['Globsnow3','MAE']<-MAE(vdata_y3$Globsnow3,vdata_y3$SWE)
# criteria3['NCAswe','MAE']<-MAE(vdata_y3$NCAswe,vdata_y3$SWE)
# 
# criteria3['ols','cor']<-cor(vdata_y3$ols,vdata_y3$SWE)
# criteria3['esf','cor']<-cor(vdata_y3$esf,vdata_y3$SWE)
# criteria3['ERA5','cor']<-cor(vdata_y3$ERA5,vdata_y3$SWE)
# criteria3['Globsnow3','cor']<-cor(vdata_y3$Globsnow3,vdata_y3$SWE)
# criteria3['NCAswe','cor']<-cor(vdata_y3$NCAswe,vdata_y3$SWE)
# criteria3
# plot(vdata_y3$ERA5,vdata_y3$SWE,ylim=c(0,500),xlim=c(0,500))
# plot(vdata_y3$esf,vdata_y3$SWE,ylim=c(0,500),xlim=c(0,500))
# plot(vdata_y3$ols,vdata_y3$SWE,ylim=c(0,500),xlim=c(0,500))

vdata_y4 <- vdata_y[which(as.integer(substr(vdata_y$DATE,3,5))>100&as.integer(substr(vdata_y$DATE,3,5))<=366),]
criteria4<-data.frame()
criteria4['ols','RMSE']<-RMSE(vdata_y4$ols,vdata_y4$SWE)
criteria4['esf','RMSE']<-RMSE(vdata_y4$esf,vdata_y4$SWE)
criteria4['ERA5','RMSE']<-RMSE(vdata_y4$ERA5,vdata_y4$SWE)
criteria4['AMSR2','RMSE']<-RMSE(vdata_y4$AMSR2,vdata_y4$SWE)
criteria4['Globsnow3','RMSE']<-RMSE(vdata_y4$Globsnow3,vdata_y4$SWE)
criteria4['NCAswe','RMSE']<-RMSE(vdata_y4$NCAswe,vdata_y4$SWE)

criteria4['ols','MAE']<-MAE(vdata_y4$ols,vdata_y4$SWE)
criteria4['esf','MAE']<-MAE(vdata_y4$esf,vdata_y4$SWE)
criteria4['ERA5','MAE']<-MAE(vdata_y4$ERA5,vdata_y4$SWE)
criteria4['AMSR2','MAE']<-MAE(vdata_y4$AMSR2,vdata_y4$SWE)
criteria4['Globsnow3','MAE']<-MAE(vdata_y4$Globsnow3,vdata_y4$SWE)
criteria4['NCAswe','MAE']<-MAE(vdata_y4$NCAswe,vdata_y4$SWE)

criteria4['ols','cor']<-cor(vdata_y4$ols,vdata_y4$SWE)
criteria4['esf','cor']<-cor(vdata_y4$esf,vdata_y4$SWE)
criteria4['ERA5','cor']<-cor(vdata_y4$ERA5,vdata_y4$SWE)
criteria4['AMSR2','cor']<-cor(vdata_y4$AMSR2,vdata_y4$SWE)
criteria4['Globsnow3','cor']<-cor(vdata_y4$Globsnow3,vdata_y4$SWE)
criteria4['NCAswe','cor']<-cor(vdata_y4$NCAswe,vdata_y4$SWE)


criteria4['ols','PE']<-mean(as.vector(vdata_y4$ols-vdata_y4$SWE)[vdata_y4$ols-vdata_y4$SWE>0])
criteria4['esf','PE']<-mean(as.vector(vdata_y4$esf-vdata_y4$SWE)[vdata_y4$esf-vdata_y4$SWE>0])
criteria4['ERA5','PE']<-mean(as.vector(vdata_y4$ERA5-vdata_y4$SWE)[vdata_y4$ERA5-vdata_y4$SWE>0])
criteria4['AMSR2','PE']<-mean(as.vector(vdata_y4$AMSR2-vdata_y4$SWE)[vdata_y4$AMSR2-vdata_y4$SWE>0])
criteria4['Globsnow3','PE']<-mean(as.vector(vdata_y4$Globsnow3-vdata_y4$SWE)[vdata_y4$Globsnow3-vdata_y4$SWE>0])
criteria4['NCAswe','PE']<-mean(as.vector(vdata_y4$NCAswe-vdata_y4$SWE)[vdata_y4$NCAswe-vdata_y4$SWE>0])

criteria4['ols','NE']<-mean(as.vector(vdata_y4$ols-vdata_y4$SWE)[vdata_y4$ols-vdata_y4$SWE<0])
criteria4['esf','NE']<-mean(as.vector(vdata_y4$esf-vdata_y4$SWE)[vdata_y4$esf-vdata_y4$SWE<0])
criteria4['ERA5','NE']<-mean(as.vector(vdata_y4$ERA5-vdata_y4$SWE)[vdata_y4$ERA5-vdata_y4$SWE<0])
criteria4['AMSR2','NE']<-mean(as.vector(vdata_y4$AMSR2-vdata_y4$SWE)[vdata_y4$AMSR2-vdata_y4$SWE<0])
criteria4['Globsnow3','NE']<-mean(as.vector(vdata_y4$Globsnow3-vdata_y4$SWE)[vdata_y4$Globsnow3-vdata_y4$SWE<0])
criteria4['NCAswe','NE']<-mean(as.vector(vdata_y4$NCAswe-vdata_y4$SWE)[vdata_y4$NCAswe-vdata_y4$SWE<0])


criteria4['ols','corp']<-(cor.test(vdata_y4$ols,vdata_y4$SWE))[["p.value"]]
criteria4['esf','corp']<-(cor.test(vdata_y4$esf,vdata_y4$SWE))[["p.value"]]
criteria4['ERA5','corp']<-(cor.test(vdata_y4$ERA5,vdata_y4$SWE))[["p.value"]]
criteria4['AMSR2','corp']<-(cor.test(vdata_y4$AMSR2,vdata_y4$SWE))[["p.value"]]
criteria4['Globsnow3','corp']<-(cor.test(vdata_y4$Globsnow3,vdata_y4$SWE))[["p.value"]]
criteria4['NCAswe','corp']<-(cor.test(vdata_y4$NCAswe,vdata_y4$SWE))[["p.value"]]

criteria4
plot(vdata_y4$ERA5,vdata_y4$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y4$esf,vdata_y4$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y4$ols,vdata_y4$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y1$AMSR2,vdata_y1$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y2$AMSR2,vdata_y2$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y3$AMSR2,vdata_y3$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y4$AMSR2,vdata_y4$SWE,ylim=c(0,500),xlim=c(0,500))
plot(vdata_y$AMSR2,vdata_y$SWE,ylim=c(0,500),xlim=c(0,500))
criteria1
criteria2
criteria4
criteria

dim(vdata_y1)
dim(vdata_y2)
dim(vdata_y4)
dim(vdata_y1)+dim(vdata_y2)+dim(vdata_y4)
table(vdata_y$MLC.re)

write.csv(unique(vdata_y[,c('station','code','State','Elevation','Latitude','Longitude')]),'D:/chenyuejun/SWE/Chap2/data/validation/snotel_points_filter/final_select/final_select.csv')
save.image(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/outIndex.Rdata',sep = ''))

write.csv(criteria1,'D:/chenyuejun/SWE/RS/paper/tables/mon1.csv')
write.csv(criteria2,'D:/chenyuejun/SWE/RS/paper/tables/mon2.csv')
write.csv(criteria4,'D:/chenyuejun/SWE/RS/paper/tables/mon4.csv')
write.csv(criteria,'D:/chenyuejun/SWE/RS/paper/tables/all.mon.landcover.csv')

criteria1[,'corp']<0.01
criteria2[,'corp']<0.01
criteria4[,'corp']<0.01

# 计算
# 1    3    4 
# 330 5053 6109 
table(vdata_y$MLC.re)
lc1<-vdata_y[vdata_y$MLC.re==1,]
lc1_output<-data.frame()
lc1_output['ols','RMSE']<-RMSE(lc1$ols,lc1$SWE)
lc1_output['esf','RMSE']<-RMSE(lc1$esf,lc1$SWE)
lc1_output['ERA5','RMSE']<-RMSE(lc1$ERA5,lc1$SWE)
lc1_output['AMSR2','RMSE']<-RMSE(lc1$AMSR2,lc1$SWE)
lc1_output['Globsnow3','RMSE']<-RMSE(lc1$Globsnow3,lc1$SWE)
lc1_output['NCAswe','RMSE']<-RMSE(lc1$NCAswe,lc1$SWE)

lc1_output['ols','MAE']<-MAE(lc1$ols,lc1$SWE)
lc1_output['esf','MAE']<-MAE(lc1$esf,lc1$SWE)
lc1_output['ERA5','MAE']<-MAE(lc1$ERA5,lc1$SWE)
lc1_output['AMSR2','MAE']<-MAE(lc1$AMSR2,lc1$SWE)
lc1_output['Globsnow3','MAE']<-MAE(lc1$Globsnow3,lc1$SWE)
lc1_output['NCAswe','MAE']<-MAE(lc1$NCAswe,lc1$SWE)

lc1_output['ols','cor']<-cor(lc1$ols,lc1$SWE)
lc1_output['esf','cor']<-cor(lc1$esf,lc1$SWE)
lc1_output['ERA5','cor']<-cor(lc1$ERA5,lc1$SWE)
lc1_output['AMSR2','cor']<-cor(lc1$AMSR2,lc1$SWE)
lc1_output['Globsnow3','cor']<-cor(lc1$Globsnow3,lc1$SWE)
lc1_output['NCAswe','cor']<-cor(lc1$NCAswe,lc1$SWE)

lc1_output['ols','PE']<-mean(as.vector(lc1$ols-lc1$SWE)[lc1$ols-lc1$SWE>0])
lc1_output['esf','PE']<-mean(as.vector(lc1$esf-lc1$SWE)[lc1$esf-lc1$SWE>0])
lc1_output['ERA5','PE']<-mean(as.vector(lc1$ERA5-lc1$SWE)[lc1$ERA5-lc1$SWE>0])
lc1_output['AMSR2','PE']<-mean(as.vector(lc1$AMSR2-lc1$SWE)[lc1$AMSR2-lc1$SWE>0])
lc1_output['Globsnow3','PE']<-mean(as.vector(lc1$Globsnow3-lc1$SWE)[lc1$Globsnow3-lc1$SWE>0])
lc1_output['NCAswe','PE']<-mean(as.vector(lc1$NCAswe-lc1$SWE)[lc1$NCAswe-lc1$SWE>0])

lc1_output['ols','NE']<-mean(as.vector(lc1$ols-lc1$SWE)[lc1$ols-lc1$SWE<0])
lc1_output['esf','NE']<-mean(as.vector(lc1$esf-lc1$SWE)[lc1$esf-lc1$SWE<0])
lc1_output['ERA5','NE']<-mean(as.vector(lc1$ERA5-lc1$SWE)[lc1$ERA5-lc1$SWE<0])
lc1_output['AMSR2','NE']<-mean(as.vector(lc1$AMSR2-lc1$SWE)[lc1$AMSR2-lc1$SWE<0])
lc1_output['Globsnow3','NE']<-mean(as.vector(lc1$Globsnow3-lc1$SWE)[lc1$Globsnow3-lc1$SWE<0])
lc1_output['NCAswe','NE']<-mean(as.vector(lc1$NCAswe-lc1$SWE)[lc1$NCAswe-lc1$SWE<0])


lc1_output['ols','corp']<-(cor.test(lc1$ols,lc1$SWE))[["p.value"]]
lc1_output['esf','corp']<-(cor.test(lc1$esf,lc1$SWE))[["p.value"]]
lc1_output['ERA5','corp']<-(cor.test(lc1$ERA5,lc1$SWE))[["p.value"]]
lc1_output['AMSR2','corp']<-(cor.test(lc1$AMSR2,lc1$SWE))[["p.value"]]
lc1_output['Globsnow3','corp']<-(cor.test(lc1$Globsnow3,lc1$SWE))[["p.value"]]
lc1_output['NCAswe','corp']<-(cor.test(lc1$NCAswe,lc1$SWE))[["p.value"]]

lc1_output[,'corp']<0.1

lc1_output

lc3<-vdata_y[vdata_y$MLC.re==3,]
lc3_output<-data.frame()
lc3_output['ols','RMSE']<-RMSE(lc3$ols,lc3$SWE)
lc3_output['esf','RMSE']<-RMSE(lc3$esf,lc3$SWE)
lc3_output['ERA5','RMSE']<-RMSE(lc3$ERA5,lc3$SWE)
lc3_output['AMSR2','RMSE']<-RMSE(lc3$AMSR2,lc3$SWE)
lc3_output['Globsnow3','RMSE']<-RMSE(lc3$Globsnow3,lc3$SWE)
lc3_output['NCAswe','RMSE']<-RMSE(lc3$NCAswe,lc3$SWE)

lc3_output['ols','MAE']<-MAE(lc3$ols,lc3$SWE)
lc3_output['esf','MAE']<-MAE(lc3$esf,lc3$SWE)
lc3_output['ERA5','MAE']<-MAE(lc3$ERA5,lc3$SWE)
lc3_output['AMSR2','MAE']<-MAE(lc3$AMSR2,lc3$SWE)
lc3_output['Globsnow3','MAE']<-MAE(lc3$Globsnow3,lc3$SWE)
lc3_output['NCAswe','MAE']<-MAE(lc3$NCAswe,lc3$SWE)

lc3_output['ols','cor']<-cor(lc3$ols,lc3$SWE)
lc3_output['esf','cor']<-cor(lc3$esf,lc3$SWE)
lc3_output['ERA5','cor']<-cor(lc3$ERA5,lc3$SWE)
lc3_output['AMSR2','cor']<-cor(lc3$AMSR2,lc3$SWE)
lc3_output['Globsnow3','cor']<-cor(lc3$Globsnow3,lc3$SWE)
lc3_output['NCAswe','cor']<-cor(lc3$NCAswe,lc3$SWE)

lc3_output['ols','PE']<-mean(as.vector(lc3$ols-lc3$SWE)[lc3$ols-lc3$SWE>0])
lc3_output['esf','PE']<-mean(as.vector(lc3$esf-lc3$SWE)[lc3$esf-lc3$SWE>0])
lc3_output['ERA5','PE']<-mean(as.vector(lc3$ERA5-lc3$SWE)[lc3$ERA5-lc3$SWE>0])
lc3_output['AMSR2','PE']<-mean(as.vector(lc3$AMSR2-lc3$SWE)[lc3$AMSR2-lc3$SWE>0])
lc3_output['Globsnow3','PE']<-mean(as.vector(lc3$Globsnow3-lc3$SWE)[lc3$Globsnow3-lc3$SWE>0])
lc3_output['NCAswe','PE']<-mean(as.vector(lc3$NCAswe-lc3$SWE)[lc3$NCAswe-lc3$SWE>0])

lc3_output['ols','NE']<-mean(as.vector(lc3$ols-lc3$SWE)[lc3$ols-lc3$SWE<0])
lc3_output['esf','NE']<-mean(as.vector(lc3$esf-lc3$SWE)[lc3$esf-lc3$SWE<0])
lc3_output['ERA5','NE']<-mean(as.vector(lc3$ERA5-lc3$SWE)[lc3$ERA5-lc3$SWE<0])
lc3_output['AMSR2','NE']<-mean(as.vector(lc3$AMSR2-lc3$SWE)[lc3$AMSR2-lc3$SWE<0])
lc3_output['Globsnow3','NE']<-mean(as.vector(lc3$Globsnow3-lc3$SWE)[lc3$Globsnow3-lc3$SWE<0])
lc3_output['NCAswe','NE']<-mean(as.vector(lc3$NCAswe-lc3$SWE)[lc3$NCAswe-lc3$SWE<0])


lc3_output['ols','corp']<-(cor.test(lc3$ols,lc3$SWE))[["p.value"]]
lc3_output['esf','corp']<-(cor.test(lc3$esf,lc3$SWE))[["p.value"]]
lc3_output['ERA5','corp']<-(cor.test(lc3$ERA5,lc3$SWE))[["p.value"]]
lc3_output['AMSR2','corp']<-(cor.test(lc3$AMSR2,lc3$SWE))[["p.value"]]
lc3_output['Globsnow3','corp']<-(cor.test(lc3$Globsnow3,lc3$SWE))[["p.value"]]
lc3_output['NCAswe','corp']<-(cor.test(lc3$NCAswe,lc3$SWE))[["p.value"]]

lc3_output[,'corp']<0.1

lc3_output

lc4<-vdata_y[vdata_y$MLC.re==4,]
lc4_output<-data.frame()
lc4_output['ols','RMSE']<-RMSE(lc4$ols,lc4$SWE)
lc4_output['esf','RMSE']<-RMSE(lc4$esf,lc4$SWE)
lc4_output['ERA5','RMSE']<-RMSE(lc4$ERA5,lc4$SWE)
lc4_output['AMSR2','RMSE']<-RMSE(lc4$AMSR2,lc4$SWE)
lc4_output['Globsnow3','RMSE']<-RMSE(lc4$Globsnow3,lc4$SWE)
lc4_output['NCAswe','RMSE']<-RMSE(lc4$NCAswe,lc4$SWE)

lc4_output['ols','MAE']<-MAE(lc4$ols,lc4$SWE)
lc4_output['esf','MAE']<-MAE(lc4$esf,lc4$SWE)
lc4_output['ERA5','MAE']<-MAE(lc4$ERA5,lc4$SWE)
lc4_output['AMSR2','MAE']<-MAE(lc4$AMSR2,lc4$SWE)
lc4_output['Globsnow3','MAE']<-MAE(lc4$Globsnow3,lc4$SWE)
lc4_output['NCAswe','MAE']<-MAE(lc4$NCAswe,lc4$SWE)

lc4_output['ols','cor']<-cor(lc4$ols,lc4$SWE)
lc4_output['esf','cor']<-cor(lc4$esf,lc4$SWE)
lc4_output['ERA5','cor']<-cor(lc4$ERA5,lc4$SWE)
lc4_output['AMSR2','cor']<-cor(lc4$AMSR2,lc4$SWE)
lc4_output['Globsnow3','cor']<-cor(lc4$Globsnow3,lc4$SWE)
lc4_output['NCAswe','cor']<-cor(lc4$NCAswe,lc4$SWE)

lc4_output['ols','PE']<-mean(as.vector(lc4$ols-lc4$SWE)[lc4$ols-lc4$SWE>0])
lc4_output['esf','PE']<-mean(as.vector(lc4$esf-lc4$SWE)[lc4$esf-lc4$SWE>0])
lc4_output['ERA5','PE']<-mean(as.vector(lc4$ERA5-lc4$SWE)[lc4$ERA5-lc4$SWE>0])
lc4_output['AMSR2','PE']<-mean(as.vector(lc4$AMSR2-lc4$SWE)[lc4$AMSR2-lc4$SWE>0])
lc4_output['Globsnow3','PE']<-mean(as.vector(lc4$Globsnow3-lc4$SWE)[lc4$Globsnow3-lc4$SWE>0])
lc4_output['NCAswe','PE']<-mean(as.vector(lc4$NCAswe-lc4$SWE)[lc4$NCAswe-lc4$SWE>0])

lc4_output['ols','NE']<-mean(as.vector(lc4$ols-lc4$SWE)[lc4$ols-lc4$SWE<0])
lc4_output['esf','NE']<-mean(as.vector(lc4$esf-lc4$SWE)[lc4$esf-lc4$SWE<0])
lc4_output['ERA5','NE']<-mean(as.vector(lc4$ERA5-lc4$SWE)[lc4$ERA5-lc4$SWE<0])
lc4_output['AMSR2','NE']<-mean(as.vector(lc4$AMSR2-lc4$SWE)[lc4$AMSR2-lc4$SWE<0])
lc4_output['Globsnow3','NE']<-mean(as.vector(lc4$Globsnow3-lc4$SWE)[lc4$Globsnow3-lc4$SWE<0])
lc4_output['NCAswe','NE']<-mean(as.vector(lc4$NCAswe-lc4$SWE)[lc4$NCAswe-lc4$SWE<0])


lc4_output['ols','corp']<-(cor.test(lc4$ols,lc4$SWE))[["p.value"]]
lc4_output['esf','corp']<-(cor.test(lc4$esf,lc4$SWE))[["p.value"]]
lc4_output['ERA5','corp']<-(cor.test(lc4$ERA5,lc4$SWE))[["p.value"]]
lc4_output['AMSR2','corp']<-(cor.test(lc4$AMSR2,lc4$SWE))[["p.value"]]
lc4_output['Globsnow3','corp']<-(cor.test(lc4$Globsnow3,lc4$SWE))[["p.value"]]
lc4_output['NCAswe','corp']<-(cor.test(lc4$NCAswe,lc4$SWE))[["p.value"]]

lc4_output[,'corp']<0.1

lc4_output


lc1_output
lc1_output[,'corp']<0.001
lc1_output[,'corp']<0.01
lc1_output[,'corp']<0.05
lc1_output[,'corp']<0.1

lc3_output
lc3_output[,'corp']<0.001


lc4_output
lc4_output[,'corp']<0.001

lc1_output
lc3_output
lc4_output
write.csv(lc1_output,'D:/chenyuejun/SWE/RS/paper/tables/landcover1.csv')
write.csv(lc3_output,'D:/chenyuejun/SWE/RS/paper/tables/landcover3.csv')
write.csv(lc4_output,'D:/chenyuejun/SWE/RS/paper/tables/landcover4.csv')


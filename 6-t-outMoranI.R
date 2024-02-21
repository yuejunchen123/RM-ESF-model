library(spdep)
library(dplyr)
library(ggplot2)

#######################
# save coefficients
i=4
write.csv(tmp.esf$coefficients,'E:/11-16data/output/model/esf4.coef.csv')

summary(tmp.esf)

# add ID
meigenID<-list()
for (i in c(1:4)) {
  meigen[[i]]["MstationID"]<-c(1:dim(meigen[[i]])[1])
  meigenID[[i]]<-meigen[[i]][,c("CODE","LATITUDE_x.x","LONGITUDE_x","Geom_X.x","Geom_Y.x","MstationID","LAT")]
}

moran.data<-list()
dateID<-read.csv('E:/11-16data/output/moran.test/dateID.csv')
for (i in c(1:4)) {
  moran.data[[i]]<-merge(modeldata[[i]],dateID,by = 'DATE',all.x=TRUE)
  moran.data[[i]]<-merge(moran.data[[i]],meigenID[[i]][,c("CODE","MstationID")],by = 'CODE',all.x=TRUE)
}
save(moran.data,meigenID,file = 'E:/11-16data/output/moran.test/esf.mc.Rdata')

#######################
# moran's I ESF
# 读一个改一个 i
i <- 4
mrt.output.esf4<-data.frame()
moran.data[[i]]['residual']<-tmp.esf$residuals
coords<-meigenID[[i]][,c('Geom_X.x','Geom_Y.x')]
knearest <- knearneigh(coordinates(coords), k = 4)
knearest<-knn2nb(knearest)
j <- 1
for(date in sort(unique(moran.data[[i]]$DATE))){
  data.ID<-moran.data[[i]][which(moran.data[[i]]$DATE==date),]
  data.ID<-data.ID[order(data.ID$MstationID),]
  if(dim(data.ID)[1]<5){next}
  knearest_dropped<-subset(knearest, 1:length(knearest) %in% data.ID$MstationID)
  lw <- nb2listw(knearest_dropped, style = "B", zero.policy=TRUE)
  moran<-moran.test(data.ID$residual,listw = lw, zero.policy=TRUE)
  name<-as.character(date)
  mrt.output.esf4[j,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  mrt.output.esf4[j,'p']<-moran[["p.value"]]
  mrt.output.esf4[j,'date']<-date
  mrt.output.esf4[j,'j']<-j
  j<-j+1
  print(date)
}
save(mrt.output.esf4,file = 'E:/11-16data/output/moran.test/mrt.output.esf4.Rdata')

# merge moran's I ESF out put
mrt.output.esf<-rbind(mrt.output.esf1,mrt.output.esf2,mrt.output.esf3,mrt.output.esf4)
mrt.output.esf<-merge(mrt.output.esf,dateID,by.x = 'date',by.y='DATE',all.x=TRUE)
save(mrt.output.esf,file = 'E:/11-16data/output/moran.test/mrt.output.esf.Rdata')

#######################

# save coefficients
i=3
write.csv(tmp.ols$coefficients,'E:/11-16data/output/model/ols3.coef.csv')

summary(tmp.ols)


# moran's I OLS
i <- 3
mrt.output.ols3<-data.frame()
moran.data[[i]]['olsresidual']<-tmp.ols$residuals
coords<-meigenID[[i]][,c('Geom_X','Geom_Y')]
knearest <- knearneigh(coordinates(coords), k = 4)
knearest<-knn2nb(knearest)
j <- 1
for(date in sort(unique(moran.data[[i]]$DATE))){
  data.ID<-moran.data[[i]][which(moran.data[[i]]$DATE==date),]
  data.ID<-data.ID[order(data.ID$MstationID),]
  if(dim(data.ID)[1]<5){next}
  knearest_dropped<-subset(knearest, 1:length(knearest) %in% data.ID$MstationID)
  lw <- nb2listw(knearest_dropped, style = "B", zero.policy=TRUE)
  moran<-moran.test(data.ID$olsresidual,listw = lw, zero.policy=TRUE)
  name<-as.character(date)
  mrt.output.ols3[j,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  mrt.output.ols3[j,'p']<-moran[["p.value"]]
  mrt.output.ols3[j,'date']<-date
  mrt.output.ols3[j,'j']<-j
  j<-j+1
  print(date)
}
# merge moran's I OLS out put
mrt.output.ols<-rbind(mrt.output.ols1,mrt.output.ols2,mrt.output.ols3,mrt.output.ols4)
mrt.output.ols<-merge(mrt.output.ols,dateID,by.x = 'date',by.y='DATE',all.x=TRUE)
save(mrt.output.ols,file = 'E:/11-16data/output/moran.test/mrt.output.ols.Rdata')

# merge ESF OLS output
names(mrt.output.esf)<-c('date','esf.mc','esf.p','j','dayID')
names(mrt.output.ols)<-c('date','ols.mc','ols.p','j','dayID')
mrt.output<-merge(mrt.output.esf,mrt.output.ols,by=c('date','j','dayID'))
write.csv(mrt.output,'E:/11-16data/output/moran.test/mrt.output.csv')





#WESD
modeldataunlist <- modeldata[[1]]
modeldataunlist <- rbind(modeldataunlist,modeldata[[2]])
modeldataunlist <- rbind(modeldataunlist,modeldata[[3]])
modeldataunlist <- rbind(modeldataunlist,modeldata[[4]])
datamean<-group_by(modeldataunlist,DATE ) %>% summarize_each(funs(mean))
datamean<-as.data.frame(datamean)
write.csv(datamean,'D:/chenyuejun/SWE/RS/paper/figure5/datamean.csv')
datameanmon <- read.csv('D:/chenyuejun/SWE/RS/paper/figure5/datamean.csv')
datameanmon1<-group_by(datameanmon,ym ) %>% summarize_each(funs(mean))
write.csv(as.data.frame(datameanmon1),'D:/chenyuejun/SWE/RS/paper/figure5/datameanmon1.csv')
ggplot(datamean,aes(x=DATE,y=WESD))+
  geom_point(size=1.5)+
  theme_bw()



# by station mean
stations.id<-unique(modeldataunlist[,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')])
coords<-stations.id[,c('Geom_X','Geom_Y')]
knearest <- knearneigh(coordinates(coords), k = 4)
knearest<-knn2nb(knearest)
lw <-  nb2listw(knearest, style = "B", zero.policy=TRUE)
datamean<-group_by(modeldataunlist,CODE ) %>% summarize_each(funs(mean))
moran<-moran.test(datamean$SNWD,listw = lw, zero.policy=TRUE)
moran[["estimate"]][["Moran I statistic"]]  #0.5927457
moran[["p.value"]]   #3.778092e-104


# by DATE
datelist <- unique(modeldataunlist$DATE)
datelist <- sort(datelist,decreasing = F)
i <- 1
SNWDmoran <- data.frame()
for (date in datelist) {
  print(date)
  tmp.data <- modeldataunlist[which(modeldataunlist$DATE==date),]
  if(dim(tmp.data)[1]<10){
    SNWDmoran[i,'mc']<-NA
    SNWDmoran[i,'p']<-NA
    SNWDmoran[i,'date']<-NA
    SNWDmoran[i,'i']<-NA
    i <- i+1
    next
  }else{
    stations.id<-unique(tmp.data[,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')])
    coords<-stations.id[,c('Geom_X','Geom_Y')]
    knearest <- knearneigh(coordinates(coords), k = 4)
    knearest<-knn2nb(knearest)
    lw <-  nb2listw(knearest, style = "B", zero.policy=TRUE)
    moran<-moran.test(tmp.data$SNWD,listw = lw, zero.policy=TRUE)
    SNWDmoran[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
    SNWDmoran[i,'p']<-moran[["p.value"]]
    SNWDmoran[i,'date']<-date
    SNWDmoran[i,'i']<-i
    i <- i+1
  }
}
SNWDmoran <- na.omit(SNWDmoran)
mean(SNWDmoran$mc)  #0.4998069
mean(SNWDmoran$p)   #2.274481e-07

# by mon station mean SNWD
monSNWDmoran <- data.frame()
i=1
for (j in c(1,2,4)) {
  tmp.data <- modeldata[[j]]
  stations.id<-unique(tmp.data[,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')])
  coords<-stations.id[,c('Geom_X','Geom_Y')]
  knearest <- knearneigh(coordinates(coords), k = 4)
  knearest<-knn2nb(knearest)
  lw <-  nb2listw(knearest, style = "B", zero.policy=TRUE)
  datamean<-group_by(tmp.data,CODE ) %>% summarize_each(funs(mean))
  moran<-moran.test(datamean$SNWD,listw = lw, zero.policy=TRUE)
  monSNWDmoran[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  monSNWDmoran[i,'p']<-moran[["p.value"]]
  i <- i+1
}
monSNWDmoran

# by mon station mean OLS residuals
monOLSmoran <- data.frame()
i=1
for (j in c(1,2,4)) {
  tmp.model.name <- paste('ols.',as.character(j),sep = '')
  print(tmp.model.name)
  tmp.model <- get(tmp.model.name)
  tmp.data <- cbind(modeldata[[j]][,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')],tmp.model$residuals)
  colnames(tmp.data) <- c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y','residuals')
  stations.id<-unique(tmp.data[,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')])
  coords<-stations.id[,c('Geom_X','Geom_Y')]
  knearest <- knearneigh(coordinates(coords), k = 4)
  knearest<-knn2nb(knearest)
  lw <-  nb2listw(knearest, style = "B", zero.policy=TRUE)
  datamean<-group_by(tmp.data,CODE ) %>% summarize_each(funs(mean))
  moran<-moran.test(datamean$residuals,listw = lw, zero.policy=TRUE)
  monOLSmoran[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  monOLSmoran[i,'p']<-moran[["p.value"]]
  i <- i+1
}
monOLSmoran

# by mon station mean ESF residuals
monESFmoran <- data.frame()
i=1
for (j in c(1,2,4)) {
  tmp.model.name <- paste('esf.',as.character(j),sep = '')
  print(tmp.model.name)
  tmp.model <- get(tmp.model.name)
  tmp.data <- cbind(modeldata[[j]][,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')],tmp.model$residuals)
  colnames(tmp.data) <- c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y','residuals')
  stations.id<-unique(tmp.data[,c('CODE','LATITUDE','LONGITUDE','Geom_X','Geom_Y')])
  coords<-stations.id[,c('Geom_X','Geom_Y')]
  knearest <- knearneigh(coordinates(coords), k = 4)
  knearest<-knn2nb(knearest)
  lw <-  nb2listw(knearest, style = "B", zero.policy=TRUE)
  datamean<-group_by(tmp.data,CODE ) %>% summarize_each(funs(mean))
  moran<-moran.test(datamean$residuals,listw = lw, zero.policy=TRUE)
  monESFmoran[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  monESFmoran[i,'p']<-moran[["p.value"]]
  i <- i+1
}
monESFmoran



monSNWDmoran[,'i'] <- c(1,2,3)
colnames(monSNWDmoran) <- c('SDmc','SDp','i')
monOLSmoran[,'i'] <- c(1,2,3)
colnames(monOLSmoran) <- c('OLSmc','OLSp','i')
monESFmoran[,'i'] <- c(1,2,3)
colnames(monESFmoran) <- c('ESFmc','ESFp','i')

monSNWDmoran
monOLSmoran
monESFmoran

moran.all <- merge(monSNWDmoran,monOLSmoran,by=c('i'))
moran.all <- merge(moran.all,monESFmoran,by=c('i'))
moran.all
write.csv(moran.all,'D:/chenyuejun/SWE/RS/paper/tables/moran.all.csv')


# MF_ESF
# esf
mrt_output.esf<-data.frame()
dmoranlist.esf<-list()
mdata.std.meigen.4moran1<-mdata.std.addid[mdata.std.addid$monthID==1,]
mdata.std.meigen.4moran1['residual']<-esf1$residuals

mdata.std.meigen.4moran2<-mdata.std.addid[mdata.std.addid$monthID==2,]
mdata.std.meigen.4moran2['residual']<-esf2$residuals

mdata.std.meigen.4moran3<-mdata.std.addid[mdata.std.addid$monthID==12,]
mdata.std.meigen.4moran3['residual']<-esf3$residuals

mdata.std.meigen.4moran<-rbind(mdata.std.meigen.4moran1,mdata.std.meigen.4moran2,mdata.std.meigen.4moran3)

i<-1
for (date in sort(unique(mdata.std.meigen.4moran$DATE))) {
  data.ID<-mdata.std.meigen.4moran[which(mdata.std.meigen.4moran$DATE==date),]
  data.ID<-data.ID[order(data.ID$stationID),]
  knearest_dropped<-subset(knearest, 1:length(knearest) %in% data.ID$stationID)
  lw <- nb2listw(knearest_dropped, style = "B", zero.policy=TRUE)
  moran<-moran.test(data.ID$residual,listw = lw, zero.policy=TRUE)
  name<-as.character(date)
  mrt_output.esf[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  mrt_output.esf[i,'p']<-moran[["p.value"]]
  mrt_output.esf[i,'date']<-date
  mrt_output.esf[i,'i']<-i
  dmoranlist.esf[[name]]<-moran
  i<-i+1
  print(date)
}
ggplot(mrt_output.esf,aes(x=i,y=mc))+
  geom_point(size=1.5)+
  theme_bw()

# MF_MLR
# ols
mrt_output.ols<-data.frame()
dmoranlist.ols<-list()
mdata.std.meigen.4moran1<-mdata.std.addid[mdata.std.addid$monthID==1,]
mdata.std.meigen.4moran1['residual']<-ols1$residuals

mdata.std.meigen.4moran2<-mdata.std.addid[mdata.std.addid$monthID==2,]
mdata.std.meigen.4moran2['residual']<-ols2$residuals

mdata.std.meigen.4moran3<-mdata.std.addid[mdata.std.addid$monthID==12,]
mdata.std.meigen.4moran3['residual']<-ols3$residuals

mdata.std.meigen.4moran<-rbind(mdata.std.meigen.4moran1,mdata.std.meigen.4moran2,mdata.std.meigen.4moran3)

i<-1
for (date in sort(unique(mdata.std.meigen.4moran$DATE))) {
  data.ID<-mdata.std.meigen.4moran[which(mdata.std.meigen.4moran$DATE==date),]
  data.ID<-data.ID[order(data.ID$stationID),]
  knearest_dropped<-subset(knearest, 1:length(knearest) %in% data.ID$stationID)
  lw <- nb2listw(knearest_dropped, style = "B", zero.policy=TRUE)
  moran<-moran.test(data.ID$residual,listw = lw, zero.policy=TRUE)
  name<-as.character(date)
  mrt_output.ols[i,'mc']<-moran[["estimate"]][["Moran I statistic"]]
  mrt_output.ols[i,'p']<-moran[["p.value"]]
  mrt_output.ols[i,'date2']<-date
  mrt_output.ols[i,'i']<-i
  dmoranlist.ols[[name]]<-moran
  i<-i+1
  print(date)
}
ggplot(mrt_output.ols,aes(x=i,y=mc))+
  geom_point(size=1.5)+
  theme_bw()

# bind ols esf
mrt_output<-list()
mrt_output[['ols']]<-mrt_output.ols
mrt_output[['esf']]<-mrt_output.esf
dmoranlist<-list()
dmoranlist[['ols']]<-dmoranlist.ols
dmoranlist[['esf']]<-dmoranlist.esf

write.csv(mrt_output,'E:/11-16data/output/moran.test/mrt_output.csv')

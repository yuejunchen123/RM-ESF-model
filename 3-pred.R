library(raster)
library(lme4)
library(tidyverse)

timenamelist<-c()
for (i in c(2015:2016)) {
  if(i == 2012|i == 2016){
    for (j in c(1:91)) {
      sprintf("%03d", i)
      timenamelist[length(timenamelist)+1]<-paste(sprintf("%03d", i),sprintf("%03d", j),sep = '')
    }
    for (j in c(336:366)) {
      sprintf("%03d", i)
      timenamelist[length(timenamelist)+1]<-paste(sprintf("%03d", i),sprintf("%03d", j),sep = '')
    }
  }
  if(i == 2011|i == 2013|i == 2014|i == 2015){
    for (j in c(1:90)) {
      sprintf("%03d", i)
      timenamelist[length(timenamelist)+1]<-paste(sprintf("%03d", i),sprintf("%03d", j),sep = '')
    }
    for (j in c(335:365)) {
      sprintf("%03d", i)
      timenamelist[length(timenamelist)+1]<-paste(sprintf("%03d", i),sprintf("%03d", j),sep = '')
    }
  }
}
rm(i,j)
timenamelist

# function to std pred data
std_pred_data <- function(data,mon,varname){
  tmp <- as.matrix(para.df[[mon]])
  as.numeric(tmp[,varname][1])
  max <- as.numeric(tmp[,varname][1])
  min <- as.numeric(tmp[,varname][2])
  std.data <- (data-min)/(max-min)
}

# function to read data to pred
read_data_to_pred <- function (mon,t,var) {
  year <- as.character(substr(t,1,4))
  returnlist <- list()
  path <- 'E:/11-16data/data/latest_mask'
  # var <- c(get(paste('y',as.character(year),'.',as.character(mon),'.varnames',sep = '')))
  print(var)
  flag <- 1
  for (varstr in var) {
    print(varstr)
    
    # NCA
    if(substr(varstr,1,2)=='NC'){
      if(varstr=='NCATair'){
        assign(varstr,std_pred_data(as.vector(as.matrix(raster(paste(path,'/nca/',varstr,'_f_',t,'.tif',sep = '')))[3:331,-1]),mon,varstr))
        
      }
      if(varstr=='NCASoilT'){
        assign(varstr,std_pred_data(as.vector(as.matrix(raster(paste(path,'/nca/',varstr,'emp0_',t,'.tif',sep = '')))[3:331,-1]),mon,varstr))
      }
      if(varstr!='NCASoilT'&varstr!='NCATair')
      {
        assign(varstr,std_pred_data(as.vector(as.matrix(raster(paste(path,'/nca/',varstr,'_',t,'.tif',sep = '')))[3:331,-1]),mon,varstr))
      }
    }
    
    # TB
    if(substr(varstr,1,2)=='TB'){
      
      if(tolower(substr(varstr,3,5))%in%c('37h','37v','91h','91v')){
        tb1 <- as.vector(as.matrix(raster(paste(path,'/tb/',tolower(substr(varstr,3,5)),'_',t,'.tif',sep = '')))[3:331,-1])
      }else{
        tb1 <- as.vector(as.matrix(raster(paste(path,'/tb/',tolower(substr(varstr,3,5)),'_',t,'.tif',sep = '')))[3:331,])
      }
      
      if(tolower(substr(varstr,6,8))%in%c('37h','37v','91h','91v')){
        tb2 <- as.vector(as.matrix(raster(paste(path,'/tb/',tolower(substr(varstr,6,8)),'_',t,'.tif',sep = '')))[3:331,-1])
      }else{
        tb2 <- as.vector(as.matrix(raster(paste(path,'/tb/',tolower(substr(varstr,6,8)),'_',t,'.tif',sep = '')))[3:331,])
      }
      
      tbd <- tb1 - tb2
      
      assign(varstr,std_pred_data(tbd,mon,varstr))
      
    }
    
    # TP
    if(substr(varstr,1,2)=='TP'){
      assign(varstr,std_pred_data(as.vector(as.matrix(raster(paste(path,'/',varstr,'/',varstr,'.tif',sep = "")))[2:330,]),mon,varstr))
    }
    
    # TC
    if(substr(varstr,1,2)=='TC'){
      assign(varstr,std_pred_data(as.vector(as.matrix(raster(paste(path,'/tc/TC_',year,'.tif',sep = "")))[3:331,-1]),mon,varstr))
    }
    
    # LAT
    if(varstr=='LAT'){
      assign(varstr,std_pred_data(as.vector(as.matrix(raster('E:/11-16data/data/latest_mask/lat/LAT.tif'))[3:331,-1]),mon,varstr))
    }
    
    # LON
    if(varstr=='LON'){
      assign(varstr,std_pred_data(as.vector(as.matrix(raster('E:/11-16data/data/latest_mask/lon/LON.tif'))[3:331,-c(1:2)]),mon,varstr))
    }
    
    # EV
    if(substr(varstr,1,2)=='EV'){
      assign(varstr, as.vector(as.matrix(raster(paste('D:/chenyuejun/SWE/RS/data/EV.interpolation/tif/',mon,'/',varstr,'.tif',sep = '')))[-1,]))
    }
    
    # save
    if(flag==1){
      ras.facEV <- get(varstr)
    }else{
      ras.facEV <- cbind.data.frame(ras.facEV,get(varstr))
    }
    flag <- flag+1
  }
  
  
  # DOHY
  if (year %in% c('2012','2016')){
    if (mon==1){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+92,nrow=329, ncol = 280))
    }
    if (mon==2){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+123,nrow=329, ncol = 280))
    }
    if (mon==3){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+152,nrow=329, ncol = 280))
    }
    if (mon==4){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+61,nrow=329, ncol = 280))
    }
    
  }else{
    if (mon==1){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+92,nrow=329, ncol = 280))
    }
    if (mon==2){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+123,nrow=329, ncol = 280))
    }
    if (mon==3){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+151,nrow=329, ncol = 280))
    }
    if (mon==4){
      DOHY<<-as.vector(matrix(as.numeric(substr(t, 5, 7))+61,nrow=329, ncol = 280))
    }
  }
  ras.facEV <- cbind.data.frame(ras.facEV,DOHY)
  # NCASFrac
  NCASFrac <- as.vector(as.matrix(raster(paste(path,'/nca/NCASFrac_',t,'.tif',sep = '')))[3:331,-1])
  ras.facEV <- cbind.data.frame(ras.facEV,NCASFrac)
  # SFSC
  SFSC <- as.vector(as.matrix(raster(paste(path,'/sfsc/SFSC_',t,'.tif',sep = '')))[2:330,-1])
  ras.facEV <- cbind.data.frame(ras.facEV,SFSC)
  # NCAswe
  NCAswe <- as.vector(as.matrix(raster(paste(path,'/nca/NCASWE_',t,'.tif',sep = '')))[3:331,-1])
  ras.facEV <- cbind.data.frame(ras.facEV,NCAswe)
  # NCAsd
  NCAsd <- as.vector(as.matrix(raster(paste(path,'/nca/NCASndep_',t,'.tif',sep = '')))[3:331,-1])
  ras.facEV <- cbind.data.frame(ras.facEV,NCAsd)
  # MLC 
  MLC <- as.vector(as.matrix(raster(paste('E:/11-16data/data/latest_mask/mlc/MLC_',year,'.tif',sep = '')))[2:330,-1])
  ras.facEV <- cbind.data.frame(ras.facEV,MLC)
  # denst
  denst <- as.vector(as.matrix(raster(paste('E:/11-16data/data/density_interpo/interpolation/d',t,'.tif',sep = '')))[2:330,])
  ras.facEV <- cbind.data.frame(ras.facEV,denst)
  
  colnames(ras.facEV) <- c(var,'DOHY','SFrac','SFSC','NCAswe','NCAsd','MLC','denst')
  returnlist$varlist <- c(var,'DOHY')
  returnlist$ras.facEV <- ras.facEV
  return(returnlist)
  print('-------------data loaded-------------')
  
  # variable list
  return(returnlist)
}


# # EV interpolation
# for (mon in c(1:4)) {
#   mname <- paste('esf.',as.character(mon),sep = '')
#   model <- get(mname)
#   tmp.var <- names(model$coefficients)[-1]
#   evnames <- tmp.var[which(str_detect(tmp.var,'EV')==TRUE)]
#   for (EV in evnames) {
#     print(EV)
#     tmp <- meigen[[mon]][,c("CODE","Geom_X","Geom_Y",EV)]
#     colnames(tmp) <- c("CODE","Geom_X","Geom_Y",EV)
#     write.csv(tmp,paste('D:/chenyuejun/SWE/RS/data/EV.interpolation/csv/',as.character(mon),'/',EV,'.csv',sep = ''))
#   }
# }


# ESFs interpolation
for (mon in c(1:4)) {
  mname <- paste('esf.',as.character(mon),sep = '')
  model <- get(mname)
  tmp.var <- names(model$coefficients)[-1]
  evnames <- tmp.var[which(str_detect(tmp.var,'EV')==TRUE)]
  model$coefficients[evnames]*meigen[[mon]][,c("CODE","Geom_X","Geom_Y",EV)]
  for (EV in evnames) {
    print(EV)
    tmp <- meigen[[mon]][,c("CODE","Geom_X","Geom_Y",EV)]
    colnames(tmp) <- c("CODE","Geom_X","Geom_Y",EV)
    write.csv(tmp,paste('D:/chenyuejun/SWE/RS/data/EV.interpolation/csv/',as.character(mon),'/',EV,'.csv',sep = ''))
  }
}

###############
#EV_process.py
###############



# # OLS ESF prediction
# t1 <- Sys.time()
# for (mon in c(1:4)) {
#   ols.name <- paste('ols.',as.character(mon),sep = '')
#   model <- get(ols.name)
#   Coef.ols <- model$coefficients
#   
#   esf.name <- paste('esf.',as.character(mon),sep = '')
#   model <- get(esf.name)
#   Coef.esf <- model$coefficients
#   
#   if(mon==1){
#     esf.name <- paste('esf.',as.character(mon),sep = '')
#     model <- get(esf.name)
#     var <- names(model$coefficients)[-1]
#     for (t in c(timenamelist[1:31],timenamelist[122:152])) {
#       print(t)
#       returnlist <- read_data_to_pred(1,t,var)
#       ras.facEV <- returnlist$ras.facEV
#       ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
#       ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
#       ols[ols<0]<-0
#       ols[is.na(ols)] <- -9999
#       ols.m<-matrix(ols, nrow=329, ncol = 280)
#       write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#       
#       esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
#       esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
#       esf[esf<0]<-0
#       esf[is.na(esf)] <- -9999
#       esf.m<-matrix(esf, nrow=329, ncol = 280)
#       write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#     }
#   }
#   
#   if(mon==2){
#     esf.name <- paste('esf.',as.character(mon),sep = '')
#     model <- get(esf.name)
#     var <- names(model$coefficients)[-1]
#     for (t in c(timenamelist[32:59],timenamelist[153:181])) {
#       print(t)
#       returnlist <- read_data_to_pred(2,t,var)
#       ras.facEV <- returnlist$ras.facEV
#       ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
#       ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
#       ols[ols<0]<-0
#       ols[is.na(ols)] <- -9999
#       ols.m<-matrix(ols, nrow=329, ncol = 280)
#       write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#       
#       esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
#       esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
#       esf[esf<0]<-0
#       esf[is.na(esf)] <- -9999
#       esf.m<-matrix(esf, nrow=329, ncol = 280)
#       write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#     }
#   }
#   
#   if(mon==3){
#     esf.name <- paste('esf.',as.character(mon),sep = '')
#     model <- get(esf.name)
#     var <- names(model$coefficients)[-1]
#     for (t in c(timenamelist[60:90],timenamelist[182:212])) {
#       print(t)
#       returnlist <- read_data_to_pred(3,t,var)
#       ras.facEV <- returnlist$ras.facEV
#       ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
#       ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
#       ols[ols<0]<-0
#       ols[is.na(ols)] <- -9999
#       ols.m<-matrix(ols, nrow=329, ncol = 280)
#       write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#       
#       esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
#       esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
#       esf[esf<0]<-0
#       esf[is.na(esf)] <- -9999
#       esf.m<-matrix(esf, nrow=329, ncol = 280)
#       write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#     }
#   }
#   
#   if(mon==4){
#     esf.name <- paste('esf.',as.character(mon),sep = '')
#     model <- get(esf.name)
#     var <- names(model$coefficients)[-1]
#     for (t in c(timenamelist[91:121],timenamelist[213:243])) {
#       print(t)
#       returnlist <- read_data_to_pred(4,t,var)
#       ras.facEV <- returnlist$ras.facEV
#       ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
#       ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
#       ols[ols<0]<-0
#       ols[is.na(ols)] <- -9999
#       ols.m<-matrix(ols, nrow=329, ncol = 280)
#       write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#       
#       esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
#       esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
#       esf[esf<0]<-0
#       esf[is.na(esf)] <- -9999
#       esf.m<-matrix(esf, nrow=329, ncol = 280)
#       write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
#     }
#   }
# }
# t2 <- Sys.time()
# print(t2-t1)

# OLS ESF prediction
t1 <- Sys.time()
for (mon in c(1:4)) {
  ols.name <- paste('ols.',as.character(mon),sep = '')
  model <- get(ols.name)
  Coef.ols <- model$coefficients
  
  esf.name <- paste('esf.',as.character(mon),sep = '')
  model <- get(esf.name)
  Coef.esf <- model$coefficients
  
  if(mon==1){
    esf.name <- paste('esf.',as.character(mon),sep = '')
    model <- get(esf.name)
    var <- names(model$coefficients)[-1]
    for (t in c(timenamelist[1:31])) {
      print(t)
      returnlist <- read_data_to_pred(1,t,var)
      ras.facEV <- returnlist$ras.facEV
      ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
      ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
      ols[ols<0]<-0
      ols[is.na(ols)] <- -9999
      ols.m<-matrix(ols, nrow=329, ncol = 280)
      write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
      
      esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
      esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
      esf[esf<0]<-0
      esf[is.na(esf)] <- -9999
      esf.m<-matrix(esf, nrow=329, ncol = 280)
      write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
    }
  }
  
  if(mon==2){
    esf.name <- paste('esf.',as.character(mon),sep = '')
    model <- get(esf.name)
    var <- names(model$coefficients)[-1]
    for (t in c(timenamelist[32:59])) {
      print(t)
      returnlist <- read_data_to_pred(2,t,var)
      ras.facEV <- returnlist$ras.facEV
      ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
      ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
      ols[ols<0]<-0
      ols[is.na(ols)] <- -9999
      ols.m<-matrix(ols, nrow=329, ncol = 280)
      write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
      
      esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
      esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
      esf[esf<0]<-0
      esf[is.na(esf)] <- -9999
      esf.m<-matrix(esf, nrow=329, ncol = 280)
      write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
    }
  }
  
  if(mon==3){
    esf.name <- paste('esf.',as.character(mon),sep = '')
    model <- get(esf.name)
    var <- names(model$coefficients)[-1]
    for (t in c(timenamelist[60:90])) {
      print(t)
      returnlist <- read_data_to_pred(3,t,var)
      ras.facEV <- returnlist$ras.facEV
      ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
      ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
      ols[ols<0]<-0
      ols[is.na(ols)] <- -9999
      ols.m<-matrix(ols, nrow=329, ncol = 280)
      write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
      
      esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
      esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
      esf[esf<0]<-0
      esf[is.na(esf)] <- -9999
      esf.m<-matrix(esf, nrow=329, ncol = 280)
      write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
    }
  }
  
  if(mon==4){
    esf.name <- paste('esf.',as.character(mon),sep = '')
    model <- get(esf.name)
    var <- names(model$coefficients)[-1]
    for (t in c(timenamelist[91:121])) {
      print(t)
      returnlist <- read_data_to_pred(4,t,var)
      ras.facEV <- returnlist$ras.facEV
      ols<-as.matrix(ras.facEV[,names(Coef.ols[-1])])%*%Coef.ols[-1]+Coef.ols[1]
      ols<-ols*ras.facEV$denst*ras.facEV$SFrac/10
      ols[ols<0]<-0
      ols[is.na(ols)] <- -9999
      ols.m<-matrix(ols, nrow=329, ncol = 280)
      write.table(ols.m, paste('D:/chenyuejun/SWE/RS/data/pred/ols/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
      
      esf<-as.matrix(ras.facEV[,names(Coef.esf[-1])])%*%Coef.esf[-1]+Coef.esf[1]
      esf<-esf*ras.facEV$denst*ras.facEV$SFrac/10
      esf[esf<0]<-0
      esf[is.na(esf)] <- -9999
      esf.m<-matrix(esf, nrow=329, ncol = 280)
      write.table(esf.m, paste('D:/chenyuejun/SWE/RS/data/pred/esf/',t,'.txt',sep = ''), col.names = FALSE, row.names = FALSE)
    }
  }
}
t2 <- Sys.time()
print(t2-t1)

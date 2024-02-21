library('car')
library('foreign')
install.packages('writexl')
library('writexl')
install.packages("corrplot")
library(corrplot)
# library(regclass)
library(foreign)
library(corrplot)
library(caret)
library(regclass)

# function
std<-function(data.col){
  return(as.data.frame((data.col-min(data.col))/(max(data.col)-min(data.col))))
}

#######################################
# select records
#######################################
for(year in c(2011:2016)){
  for(mon in c(1,2,3,12)){
    print(year)
    print(mon)
    namestr <- paste('y',as.character(year),'.m',sprintf("%02d", mon),sep = '')
    assign(namestr,read.csv(paste('E:/11-16data/data/all_mask2/rbind/result/',as.character(year),sprintf("%02d", mon),'.csv',sep = '')))
    assign(namestr,get(namestr)[get(namestr)$DATE!=-999,])
    assign(namestr,get(namestr)[!as.logical(rowSums(get(namestr)==-9999)),])
  }
}

for(year in c(2011:2016)){
  for(mon in c(1,2,3,12)){
    print(year)
    print(mon)
    namestr <- paste('y',as.character(year),'.m',sprintf("%02d", mon),sep = '')
    if(year==2011&mon==1){
      assign('allData',get(namestr))
    }else{
      assign('allData',rbind(allData,get(namestr)))
    }
  }
}
# add LAT LON
# write.csv(unique(allData[,c("CODE","LATITUDE_x","LONGITUDE_x")]),'D:/chenyuejun/SWE/RS/tmp_file/allDataXY.csv')
allDataLATLONLand <- read.dbf('D:/chenyuejun/SWE/RS/tmp_file/allDataXY.dbf')
allData.tmp <- merge(allData,allDataLATLONLand,by=c('CODE'))
allData.tmp <- allData.tmp[,c("CODE","DATE","SNWD","WESD","DENST","LATITUDE_x.x","LONGITUDE_x","ELEVATION_x","STATE_x","Geom_X","Geom_Y","TB37H",
                              "TB37V","TB19H","TB19V","TB91H","TB91V","TB22V","NCAQg","NCAQsm","NCASndep","NCASnowf","NCASoilT","NCASWE",
                              "NCATair","NCATmax","NCATmin","NCASFrac","NCAWind","SFSC","TC","MLC","TPelev","TPslope","TPnorth","TPrough","LAT","LON","Landform1k")]
colnames(allData.tmp) <- c("CODE","DATE","SNWD","WESD","DENST","LATITUDE","LONGITUDE","ELEVATION","STATE","Geom_X","Geom_Y","TB37H",
                           "TB37V","TB19H","TB19V","TB91H","TB91V","TB22V","NCAQg","NCAQsm","NCASndep","NCASnowf","NCASoilT","NCASWE",
                           "NCATair","NCATmax","NCATmin","NCASFrac","NCAWind","SFSC","TC","MLC","TPelev","TPslope","TPnorth","TPrough","LAT","LON","Landform1k")
allData <- allData.tmp
rm(allData.tmp)
# filter points on mountain areas
allData.select <- allData[which(allData$Landform1k<12),]
allData.select[allData.select==-9999] <- NA
allData.select <- na.omit(allData.select)
#######################################
# generate model data
#######################################

# filter station records 30day*4month*6year*15%
code.sta<-table(allData.select$CODE)>108
code.sel<-names(code.sta[code.sta>0])
allData.select.final<-allData.select[allData.select$CODE %in% code.sel,]
modeldata <- list()
modeldata[[1]] <- allData.select.final[which(as.integer(substr(allData.select.final$DATE,1,4))<2015&as.integer(substr(allData.select.final$DATE,5,6))==01),]
modeldata[[2]] <- allData.select.final[which(as.integer(substr(allData.select.final$DATE,1,4))<2015&as.integer(substr(allData.select.final$DATE,5,6))==02),]
modeldata[[3]] <- allData.select.final[which(as.integer(substr(allData.select.final$DATE,1,4))<2015&as.integer(substr(allData.select.final$DATE,5,6))==03),]
modeldata[[4]] <- allData.select.final[which(as.integer(substr(allData.select.final$DATE,1,4))<2015&as.integer(substr(allData.select.final$DATE,5,6))==12),]

# generate TBD
for (i in c(1:4)) {
  modeldata[[i]] <- transform(modeldata[[i]],TB37H37V=TB37H-TB37V,TB37H91H=TB37H-TB91H,TB37H91V=TB37H-TB91V,TB37H19H=TB37H-TB19H,
                              TB37H19V=TB37H-TB19V,TB37H22V=TB37H-TB22V,TB37V91H=TB37V-TB91H,TB37V91V=TB37V-TB91V,TB37V19H=TB37V-TB19H,
                              TB37V19V=TB37V-TB19V,TB37V22V=TB37V-TB22V,TB91H91V=TB91H-TB91V,TB91H19H=TB91H-TB19H,TB91H19V=TB91H-TB19V,
                              TB91H22V=TB91H-TB22V,TB91V19H=TB91V-TB19H,TB91V19V=TB91V-TB19V,TB91V22V=TB91V-TB22V,TB19H19V=TB19H-TB19V,
                              TB19H22V=TB19H-TB22V,TB19V22V=TB19V-TB22V)
}

# record std para
para.df <- list()
modeldata.std <- list()
for (j in c(1:4)) {
  # std 0-1 every month
  flag <- 1
  for (i in c(1:length(modeldata[[j]]))) {
    if(colnames(modeldata[[j]])[i]%in%c("CODE","DATE","SNWD","WESD","DENST","LATITUDE","LONGITUDE","ELEVATION","STATE","Geom_X","Geom_Y","MLC","Landform1k")){
      next
    }
    else{
      tmp.max<-max(modeldata[[j]][,i])
      tmp.min<-min(modeldata[[j]][,i])
      tmp.name<-colnames(modeldata[[j]])[i]
      tmp.para.df<-data.frame(c(tmp.max,tmp.min))
      colnames(tmp.para.df)<-tmp.name
      rownames(tmp.para.df)<-c('max','min')
      tmp.mdata.df <- std(modeldata[[j]][,i])
      colnames(tmp.mdata.df)<-colnames(modeldata[[j]])[i]
      
      if(flag==1){
        para.df[[j]]<-tmp.para.df
        modeldata.std[[j]]<- tmp.mdata.df
        
      }
      else{
        # record std para
        para.df[[j]]<-cbind(para.df[[j]],tmp.para.df)
        modeldata.std[[j]]<-cbind(modeldata.std[[j]],tmp.mdata.df)
        print(para.df[[j]])
      }
      flag <- flag+1
    }
  }
  modeldata.std[[j]] <- cbind(modeldata[[j]][,c("CODE","DATE","SNWD","WESD","DENST","LATITUDE","LONGITUDE","ELEVATION","STATE","Geom_X","Geom_Y","MLC","Landform1k")],modeldata.std[[j]])
}

# meigen
stations <- list()
modeldataEV <- list()
meigen <- list()
EVnames <- list()
for (i in c(1:4)) {
  # points unique for every month
  stations[[i]]<-unique(modeldata.std[[i]][,c("CODE","LATITUDE","LONGITUDE","Geom_X","Geom_Y","ELEVATION")])
  
  # meigen
  tmp.knn<-extract_meigen(coords=stations[[i]][,c('Geom_X','Geom_Y')], model='k', threshold = 0.25, k=15)
  tmp.meigen<-as.data.frame(tmp.knn$ev)
  tmp.new.name<-paste("EV",c(1:length(tmp.meigen)),sep = '')
  colnames(tmp.meigen)<-tmp.new.name
  meigen[[i]]<-cbind(stations[[i]],tmp.meigen)
  EVnames[[i]]<-names(meigen[[i]])[-c(1:6)]
  
  for (EVname in EVnames[[i]]) {
    # write.csv(meigen[[i]][,c("CODE","LATITUDE","LONGITUDE","Geom_X","Geom_Y",EVname)],paste('D:/chenyuejun/SWE/RS/tmp_file/EVname/EVname',as.character(i),'.csv',sep = ''))
  }
  
  # merge
  meigen.merge<-subset(meigen[[i]], select = -c(LATITUDE,LONGITUDE,Geom_X,Geom_Y,ELEVATION))
  modeldataEV[[i]]<-merge(modeldata.std[[i]],meigen.merge,by='CODE')
  
  # test
  print('------------------')
  print(i)
  print(length(unique(modeldata.std[[i]]$CODE)))
  print(length(unique(meigen[[i]]$CODE)))
  print(length(meigen[[i]]))
  # print(length(meigen[[i]])/length(unique(meigen[[i]]$CODE))*100)
  # print(length(meigen[[i]])/37*100)
  print('------------------')
  
  # rm
}

rm(j,meigen.merge,flag,i,tmp.mdata.df,mon,namestr,tmp.max,tmp.min,tmp.name,tmp.para.df,year,tmp.new.name,tmp.meigen,tmp.knn)


#######################################
# select variables
#######################################

check_multicollinearity <- function(cor.select){
  initial.names <- colnames(cor.select)[-1]
  drop.names <- c()
  tmp.cor <- sort(cor.select[abs(cor.select)>0.7&cor.select!=1&is.na(cor.select)==FALSE],decreasing=TRUE)
  i=1
  for (cor in tmp.cor) {
    if(cor%in%cor.select){
      
      col.indx <- which(cor.select==cor,arr.ind = TRUE)[,'col']
      col.name <- rownames(cor.select)[col.indx]
      row.indx <- which(cor.select==cor,arr.ind = TRUE)[,'row']
      row.name <- rownames(cor.select)[row.indx]
      if(abs(cor.select['SNWD',row.indx])>abs(cor.select['SNWD',col.indx])){
        drop.names <- c(drop.names,col.name)
        cor.select <- cor.select[,-col.indx]
        cor.select <- cor.select[-col.indx,]
      }else{drop.names <- c(drop.names,row.name)
            cor.select <- cor.select[,-row.indx]
            cor.select <- cor.select[-row.indx,]}
      
    }
    i=i+1
    
  }
  return(setdiff(initial.names,drop.names))
}


# correlation for manual filter X(s)
select_variables <- function(data){
  tmp.TB.names<-c("SNWD","TB37H37V","TB37H91H","TB37H91V","TB37H19H","TB37H19V","TB37H22V","TB37V91H","TB37V91V","TB37V19H","TB37V19V","TB37V22V","TB91H91V","TB91H19H","TB91H19V","TB91H22V","TB91V19H","TB91V19V","TB91V22V","TB19H19V","TB19H22V","TB19V22V")
  tmp.factors.names<-c("SNWD","NCAQg","NCAQsm","NCASnowf","NCASoilT","NCATair","NCASFrac","NCAWind","TC","TPelev","TPslope")
  var.names <- list()
  for (mon in c(1:4)) {
    cor.fct <- cor(as.data.frame(data[[mon]])[,tmp.factors.names],method = 'pearson')
    p.fct <- cor.mtest(as.data.frame(data[[mon]])[,tmp.factors.names],method = 'pearson',conf.level = 0.99)
    fct.names<-names(cor.fct[,"SNWD"][abs(cor.fct[,"SNWD"])>0.1])
    fct.names <- names(p.fct$p[fct.names,"SNWD"][p.fct$p[fct.names,"SNWD"]<0.01])
    cor.fct.select <- cor.fct[fct.names,fct.names]
    cor.fct.select[!upper.tri(cor.fct.select, diag = TRUE)] <- NA
    fct.large.cor.idx <- as.matrix(which(abs(cor.fct.select)>0.7&cor.fct.select!=1,arr.ind = TRUE))
    if(length(fct.large.cor.idx)!=0){
      fct.names.select <- check_multicollinearity(cor.fct.select)
      print('in')
    }else{fct.names.select <- fct.names[-1]}
    
    cor.tb<-cor(as.data.frame(data[[mon]])[,tmp.TB.names])
    p.tb <- cor.mtest(as.data.frame(data[[mon]])[,tmp.TB.names],method = 'pearson',conf.level = 0.99)
    tb.names<-names(cor.tb[,"SNWD"][abs(cor.tb[,"SNWD"])>0.1])
    tb.names <- names(p.tb$p[tb.names,"SNWD"][p.tb$p[tb.names,"SNWD"]<0.01])
    cor.tb.select <- cor.tb[tb.names,tb.names]
    cor.tb.select[!upper.tri(cor.tb.select, diag = TRUE)] <- NA
    tb.large.cor.idx <- as.matrix(which(abs(cor.tb.select)>0.7&cor.tb.select!=1,arr.ind = TRUE))
    if(length(tb.large.cor.idx)!=0){
      tb.names.select <- check_multicollinearity(cor.tb.select)
      print('in')
    }else{tb.names.select <- tb.names[-1]}
    tmp.var.names <- c('SNWD',fct.names.select,tb.names.select,"LAT","LON")
    tmp.lm<-lm(SNWD~.,data = data[[mon]][,tmp.var.names])
    print(VIF(tmp.lm))
    # write.csv(VIF(tmp.lm),file = paste('D:/chenyuejun/SWE/RS/tmp_file/vif/m',as.character(mon),'.csv',sep = ''))
    var.names[[mon]] <- tmp.var.names
  }
  return(var.names)
}

variables <- select_variables(modeldataEV)

rm(var.names,a,tmp.TB.names,tmp.lm,tmp.TB.names,tmp.var.names,cor.fct,cor.tb,cor.tb.select,cor.fct.select,fct.large.cor.idx,fct.names,fct.names.select,fct.names.select,tb.large.cor.idx,tb.names,tb.names.select,p.fct,p.tb,tmp.factors.names)

save.image('D:/chenyuejun/SWE/RS/data/modeldata/model.Rdata')


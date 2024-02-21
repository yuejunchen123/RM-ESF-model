tmp <- allData.select.final

tmp[,'year'] <- substr(tmp$DATE,1,4)
tmp[,'mon'] <- substr(tmp$DATE,5,6)
tmp[,'yearmon'] <- substr(tmp$DATE,1,6)
table(tmp[,'yearmon'])
aggregate(tmp$WESD, by=list(type=tmp$yearmon),mean)
length(unique(tmp$CODE))


tmp <- allData.select.final[which(as.integer(substr(allData.select.final$DATE,1,4))<2015),]
tmp <- tmp[-which(as.integer(substr(tmp$DATE,5,6))==3),]

length(unique(tmp$CODE))
modelstation <- unique(tmp[,c('CODE','LAT','LON')])
write.csv(modelstation,'D:/chenyuejun/SWE/RS/paper/figure1/modelstation.csv') 


length(unique(vdata_y$station))
validatestation <- unique(vdata_y[,c('station','Latitude','Longitude')])
write.csv(validatestation,'D:/chenyuejun/SWE/RS/paper/figure1/validatestation.csv') 

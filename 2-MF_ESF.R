library(spdep)
library(car)
library(foreign)
library(caret)

for (mon in c(1:4)) {
  # ols
  print(paste('OLS in mon ',as.character(mon),sep = ''))
  t1<-Sys.time()
  print(t1)
  ols.name <- paste('ols.',as.character(mon),sep = '')
  assign(ols.name,lm(SNWD~.,data = modeldataEV[[mon]][,variables[[mon]]]))
  print(summary(get(ols.name)))
  ols.model <- get(ols.name)
  save(ols.model,file = paste('D:/chenyuejun/SWE/RS/data/',ols.name,'.Rdata',sep = ''))
  t2<-Sys.time()
  print(t2)
  print(t2-t1)
  print('-------------------------')
  # a <- ls()
  # rm(list=a[which(a == ols.name)])
}



for (mon in c(1:4)) {
  # esf
  print(paste('ESF in mon ',as.character(mon),sep = ''))
  t1<-Sys.time()
  print(t1)
  esf.name <- paste('esf.',as.character(mon),sep = '')
  assign(esf.name,forward_selection_lm(y = modeldataEV[[mon]]$SNWD, x = modeldataEV[[mon]][,variables[[mon]][-1]], ev = modeldataEV[[mon]][,EVnames[[mon]]], x_max = 50, force_x = TRUE ,criterion = "AIC"))
  print(summary(get(esf.name)))
  esf.model <- get(esf.name)
  save(esf.model,file = paste('D:/chenyuejun/SWE/RS/data/',esf.name,'.Rdata',sep = ''))
  t2<-Sys.time()
  print(t2)
  print(t2-t1)
  print('-------------------------')
  # a <- ls()
  # rm(list=a[which(a == esf.name)])
}


# rm(a)
rm(mon,t1,t2,esf.model,esf.name,ols.model,ols.name)



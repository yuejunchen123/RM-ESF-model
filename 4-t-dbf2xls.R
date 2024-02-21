install.packages("writexl")
library("writexl")
library(caret)
library(foreign)
foldname <- 'mon'
for(fold in c('esf','ols','AMSR2','ERA5','Globsnow3','NCAswe')){
  table <- read.dbf(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/',fold,'/snotel_addlandform.dbf',sep = ''))
  write_xlsx(table,path = paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/',fold,'/snotel_addlandform.xls',sep = ''),col_names = TRUE,format_headers = TRUE)
}

table <- read.dbf(paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/','NCAswe','/snotel_addlandform.dbf',sep = ''))
write_xlsx(table,path = paste('D:/chenyuejun/SWE/RS/data/',foldname,'/validation/','NCAswe','/snotel_addlandform.xls',sep = ''),col_names = TRUE,format_headers = TRUE)

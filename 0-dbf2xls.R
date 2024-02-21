install.packages("writexl")
library("writexl")
library(caret)
path <- 'D:/chenyuejun/SWE/Chap2/data/data4model&pred/pred/MLpred_swe/step2/'
folds <- c('Best', 'Best_ESF', 'XGB', 'XGB_ESF', 'RF', 'RF_ESF', 'ET', 'ET_ESF', 'NNF', 'NNF_ESF', 'LG', 'LG_ESF')
for (fold in folds) {
  table <- read.dbf(paste('D:/chenyuejun/SWE/Chap2/data/validation/maps/', fold ,'/snotel_stations.dbf',sep = ''))
  write_xlsx(table, path = paste('D:/chenyuejun/SWE/Chap2/data/validation/maps/', fold ,'/',fold,'.xls',sep = ''),col_names = TRUE,format_headers = TRUE,)
}




install.packages("writexl")
library("writexl")
library(caret)
library(foreign)
folds <- c('Best','CB','LG','LGL','LGXT',
           'XGB','RF','ET','KND','KNU',
           'Best_ESF','CB_ESF','LG_ESF','LGL_ESF','LGXT_ESF',
           'XGB_ESF','RF_ESF','ET_ESF','KND_ESF','KNU_ESF','OLS','ESF')

for (fold in folds) {
  table <- read.dbf(paste('D:/chenyuejun/SWE/Chap2/data_cv/validation/maps/', fold ,'/snotel_stations.dbf',sep = ''))
  write_xlsx(table, path = paste('D:/chenyuejun/SWE/Chap2/data_cv/validation/xls/',fold,'.xls',sep = ''),col_names = TRUE,format_headers = TRUE,)
}


install.packages("writexl")
library("writexl")
library(caret)
library(foreign)
table <- read.dbf('D:/chenyuejun/SWE/Chap2/data/validation/lmer/2016/snotel_stations.dbf')
write_xlsx(table,path = 'D:/chenyuejun/SWE/Chap2/data/validation/lmer/2016/snotel_stations.xls',col_names = TRUE,format_headers = TRUE,)

install.packages("writexl")
library("writexl")
library(caret)
library(foreign)
table <- read.dbf('D:/chenyuejun/SWE/Chap2/data/validation/OLSESF/2016/ols/snotel_stations.dbf')
write_xlsx(table,path = 'D:/chenyuejun/SWE/Chap2/data/validation/OLSESF/2016/ols/ols.xls',col_names = TRUE,format_headers = TRUE,)
table <- read.dbf('D:/chenyuejun/SWE/Chap2/data/validation/OLSESF/2016/esf/snotel_stations.dbf')
write_xlsx(table,path = 'D:/chenyuejun/SWE/Chap2/data/validation/OLSESF/2016/esf/esf.xls',col_names = TRUE,format_headers = TRUE,)



install.packages("writexl")
library("writexl")
library(caret)
library(foreign)
table <- read.dbf('E:/lmer_oridata_test/validation/esf/snotel_stations.dbf')
write_xlsx(table,path = 'E:/lmer_oridata_test/validation/esf/snotel_stations.xls',col_names = TRUE,format_headers = TRUE,)
table <- read.dbf('E:/lmer_oridata_test/validation/ols/snotel_stations.dbf')
write_xlsx(table,path = 'E:/lmer_oridata_test/validation/ols/snotel_stations.xls',col_names = TRUE,format_headers = TRUE,)
table <- read.dbf('E:/lmer_oridata_test/validation/lmer/snotel_stations.dbf')
write_xlsx(table,path = 'E:/lmer_oridata_test/validation/lmer/snotel_stations.xls',col_names = TRUE,format_headers = TRUE,)

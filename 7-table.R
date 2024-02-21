######################
# 1 模型系数表
######################
esf.1[["coefficients"]][variables[[1]][-1]]
esf.2[["coefficients"]][variables[[2]][-1]]
esf.4[["coefficients"]][variables[[4]][-1]]

coef <- data.frame()
for (i in c(1,2,3,4)) {
  for (var in c(variables[[i]][-1])) {
    tmp.model <- get(paste('esf.',as.character(i),sep = ''))
    coef[var,i] <- tmp.model[["coefficients"]][var]
  }
}
coef

coef[,1] <- esf.1[["coefficients"]][variables[[1]][-1]]


############################
# 2 分土地利用的指标表
############################
# 见6-t-outIndex

table(modeldataunlist$MLC)
table(modeldataEV[[1]]$MLC)
table(modeldataEV[[2]]$MLC)
table(modeldataEV[[4]]$MLC)

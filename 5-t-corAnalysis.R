
varcount <- sort(table(c(variables[[1]],variables[[2]],variables[[3]],variables[[4]])),decreasing = T)

varcor1 <- as.data.frame(cor(as.data.frame(modeldataEV[[1]])[,variables[[1]]],method = 'pearson')[,1])
colnames(varcor1) <- 'mon1'
varcor2 <- as.data.frame(cor(as.data.frame(modeldataEV[[2]])[,variables[[2]]],method = 'pearson')[,1])
colnames(varcor2) <- 'mon2'
varcor3 <- as.data.frame(cor(as.data.frame(modeldataEV[[3]])[,variables[[3]]],method = 'pearson')[,1])
colnames(varcor3) <- 'mon3'
varcor4 <- as.data.frame(cor(as.data.frame(modeldataEV[[4]])[,variables[[4]]],method = 'pearson')[,1])
colnames(varcor4) <- 'mon4'
varcor1
varcor2
varcor3
varcor4


varcor<-merge(varcor1, varcor2, by = "row.names", all = T)
row.names(varcor) <- varcor$Row.names
varcor <- varcor[,c('mon1','mon2')]
varcor<-merge(varcor, varcor3, by = "row.names", all = T)
row.names(varcor) <- varcor$Row.names
varcor <- varcor[,c('mon1','mon2','mon3')]
varcor<-merge(varcor, varcor4, by = "row.names", all = T)
row.names(varcor) <- varcor$Row.names
varcor <- varcor[,c('mon1','mon2','mon3','mon4')]
varcor
rm(varcor1,varcor2,varcor3,varcor4)

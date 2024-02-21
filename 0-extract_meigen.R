library(fields)
library(vegan)
library(MASS)
library(spdep)
extract_meigen <- function(coords, model, threshold = 0.25, k=NULL){
  D <- rdist(coords)
  h <- max(spantree(D)$dist)
  
  if(model == "exp"){
    C <- ifelse( D < h, exp( -D / h ), 0)
  }
  else if(model == "gau"){
    C <- ifelse( D < h, exp( -(D / h) ^ 2 ), 0)
  }
  else if(model == "sph"){
    C	<- ifelse( D < h , 1 - 1.5 * (D / h ) + 0.5 * ( D / h ) ^ 3, 0 )
  }
  else if(model == "gau2"){
	C <- exp( -(D / h) ^ 2 )
  }
  else if(model == "gau3"){
	C <- exp( -0.5 * (D / h) ^ 2 )
  }
  else if(model == 'k' && !is.null(k)){
    knearest <- knearneigh(coordinates(coords), k = k)
    C <-  nb2mat(knn2nb(knearest), style = "B")
    if(isSymmetric(C)==FALSE){
      C <- 0.5*(C+t(C))
    }
  }
  else{
    stop("弟弟，你玩我吧，没有这个模型呀！")
  }

  diag(C) <- 0
  n <- dim(C)[1]
  #中心化centering
  MCM <- (diag(n) - c(rep(1,n)) %*% t(c(rep(1,n)))/n) %*% C %*% (diag(n) - c(rep(1,n)) %*% t(c(rep(1,n)))/n)
  eigenC <- eigen( MCM )
  eigenC$values <- Re( eigenC$values )
  eigenC$vectors <- Re( eigenC$vectors )
  ev <- eigenC$vectors[, eigenC$values/max(eigenC$values) >= threshold]
  ev_v <- eigenC$values[ eigenC$values/max(eigenC$values) >= threshold]
  return( list(ev=ev, ev_v=ev_v) )
}

# how to use, 4表示认为最近邻4个是邻接的
data <- read.csv('C:/Users/tt/Desktop/out.csv')
meig <- extract_meigen(coords=data[,c('POINT_X','POINT_Y')], model='k', threshold = 0.25, k=4)
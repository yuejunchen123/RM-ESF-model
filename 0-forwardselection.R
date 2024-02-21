#y-因变量
#x-自变量
#ev-特征向量
#x_max-包含x和ev的最大个数
#force_x-是否强制所有x进入回归，TRUE，FALSE
#criterion-选择哪个最小的准则，AIC，BIC，RMSE

forward_selection_lm <- function(y, x, ev, x_max, force_x, criterion){
  rearrange_data <- function(ev, x=NULL){
    ev <- as.data.frame(ev)
    ev_name <- paste("EV", c(1:dim(ev)[2]), sep="")
    names(ev) <- ev_name
    if (is.null(x)){
      return(ev)
    } else{
      allx <- cbind(x, ev)
      return(allx)
    }
  }
  
  #获取标准---------
  get_obj	<- function(model, criterion){
    if (criterion == "r2"){
      obj	<- summary(model)$adj.r.squared
    } else if (criterion == "AIC"){
      obj	<- -AIC(model)
    } else if (criterion == "BIC"){
      obj	<- -BIC(model)
    } else if (criterion == "RMSE"){
      obj <- -sqrt(mean(model$residuals^2))
    }
    return( obj )
  }
  #-----------------
  lm_model <- NULL
  
  if (is.null(x_max)){
    x_max <- Inf
  }
  
  sel_factor <- NULL  #选择了的x，ev的列号（对于force_x==FALSE）
  sel_ev <- NULL  #选择了的ev的列号（对于force_x==TRUE）
  
  allx <- rearrange_data(ev=ev, x=x)  #把ev和x放在了一起
  ev <- rearrange_data(ev=ev)
  
  obj <- -Inf
  selectedx <- as.data.frame(x)
  selected_allx <- NULL
  
  if (force_x){
    lm_model <- lm(y~.,data = as.data.frame(cbind(y, selectedx)))  #如果强制所有x都要进入回归，那么先定下这个基准model
    tmp_obj <- get_obj(lm_model, criterion)
    length_ev <- dim(ev)[2]
    i_ev <- 1
    while((tmp_obj > obj) & (i_ev < length_ev) & (lm_model$rank < x_max) ){
      obj <- tmp_obj
      obj_list <- c()
      model_list <- list()
      for(i in c(1:dim(ev)[2])){
        
        selectedx_names <- names(selectedx)
        ev_names <- names(ev)[i]
        data <- as.data.frame(cbind(y, selectedx, ev[, i]))
        names(data)[1] <- 'y'
        names(data)[2:(length(selectedx_names)+1)] <- selectedx_names
        names(data)[length(names(data))] <- ev_names
        
        tmp_model <- lm(y~., data=data)
        tmp_obj <- get_obj(tmp_model, criterion)
        obj_list <- c(obj_list, tmp_obj)
        model_list <- c(model_list, list(tmp_model))
      }
      tmp_obj <- max(obj_list)
      if (tmp_obj > obj){
        ev_index <- ( 1:length( obj_list ) ) * ( obj_list == tmp_obj )  # 找到所选择的ev所在位置
        ev_index <- ev_index[ev_index!=0][1]  # 找到所选择的ev所在位置
        tmp_sel_ev_name <- names(ev)[ev_index]
        sel_ev <- ev[, ev_index]
        ev <- ev[, -ev_index]
        i_ev = i_ev+1
        selectedx <- cbind(selectedx, sel_ev)

        names(selectedx)[dim(selectedx)[2]] <- tmp_sel_ev_name
        lm_model <- model_list[[ev_index]]
      }
    }
    return(lm_model)
  } else{
    lm_model <- lm(y~1)
    tmp_obj <- get_obj(lm_model, criterion)
    length_allx <- dim(allx)[2]
    i_allx <- 1
    while((tmp_obj > obj) & (i_allx < length_allx) & (lm_model$rank < x_max) ){
      obj <- tmp_obj
      obj_list <- c()
      model_list <- list()
      for(i in c(1:dim(allx)[2])){
        if (is.null(selected_allx)){
          data <- as.data.frame( cbind(y, allx[, i]) )
        } else{
          data <- as.data.frame( cbind(y, selected_allx, allx[, i]) )
        }
        names(data)[1] <- 'y'
        names(data)[length(names(data))] <- names(allx)[i]  # 把新加入的因子附上名字
        tmp_model <- lm(y~., data=data)
        tmp_obj <- get_obj(tmp_model, criterion)
        obj_list <- c(obj_list, tmp_obj)
        model_list <- c(model_list, list(tmp_model))
      }
      tmp_obj <- max(obj_list)
      if (tmp_obj > obj){
        factor_index <- ( 1:length( obj_list ) ) * ( obj_list == tmp_obj )  # 找到所选择的ev所在位置
        factor_index <- factor_index[factor_index!=0][1]  # 找到所选择的ev所在位置
        tmp_sel_factor_name <- names(allx)[factor_index]
        sel_factor <- allx[, factor_index]
        allx <- allx[, -factor_index]
        i_allx <- i_allx+1
        if (is.null(selected_allx)){
          selected_allx <- as.data.frame( sel_factor )
        } else{
          selected_allx <- as.data.frame( cbind(selected_allx, sel_factor) )
        }
        names(selected_allx)[dim(selected_allx)[2]] <- tmp_sel_factor_name
        
        lm_model <- model_list[[factor_index]]
      }
    }
    return(lm_model)
  }
}
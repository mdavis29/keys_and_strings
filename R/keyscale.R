#' @title Key Scaler
#' @param data a data frame like object
#' @param params list(key_col = NULL, trans_cols = NULL, scale = TRUE, center = TRUE)
#' @author Matthew Davis
#' @description creates seperate scales by a key
#' @return a keyscale_obj
#' @export
#'

keyscale = function(data, val_cols = NULL, key_col = NULL){
  data = data.frame(data)
  if(is.null(val_col)){ # there is no value col, use all numeric and integer cols
    val_col = colnames(data)[lapply(data, class) %in% c('numeric', 'integer')]
    val_col = val_col[!val_col %in% key_col]
  }
  if(is.null(key_col)){ # if there is no key col, create one with one value
    data$temp_key = rep(1, nrow(data))
    key_col = temp_key
  }
  keys = unique(data[, key_col])
  nkeys = length(keys)
  key_output = as.list(keys)
  for(i in 1:nkeys){
    temp_key = keys[i]
    temp_data = data[data[, key_col] == temp_key, val_col]
    means = apply(temp_data,2,mean, na.rm = TRUE)
    means[is.na(means)] = 0
    sds = apply(temp_data,2,sd, na.rm = TRUE)
    sds[is.na(sds)] = 1
    mat = rbind(means, sds)
    key_output[[i]] = mat
  }
  names(key_output) = keys
  output = list(scales = key_output, key_col = key_col)
  class(output) = append('keyscale_obj', class(output))
  return(output)
}

#' @title Predict Key Scale
#' @param object a keyscale_obj
#' @param data new data frame like object
#' @param rev unscale a scaled object
#' @author Matthew Davis
#' @description creates seperate scales by a key
#' @return a keyscale_obj
#' @export
#'


predict.keyscale_obj <- function(object, data, unscale = FALSE, verbose = FALSE){
  data = data.frame(data)
  have_cols = intersect(colnames(object$scales[[1]]), colnames(data))
  keys = names(object$scales)
  nkeys = length(keys)
  nc = length(have_cols)
  if(nkeys == 1 & !object$key_col %in% colnames(data)){
    data$temp_key = rep(1, nrow(data))
  }
    for (i in 1:nkeys){
    temp_key = keys[i]
    temp_data = data[data[,key_col] == temp_key, have_cols ]
    temp_scale = object$scales[[temp_key]][, have_cols]
    if(!unscale){
      if(verbose){print('scaling')}
      for (j in 1:nc){
        temp_col = have_cols[j]
        temp_data[, temp_col]=(temp_data[, temp_col] + temp_scale[1,temp_col])/temp_scale[2,temp_col] 
        data[,temp_col] = temp_data
      }
    attr(data, 'scaled') = TRUE
    } # end scaling case
    if(unscale | attr(data, 'scaled')){
      if(verbose)print('unscaling')
      for (j in 1:nc){
        temp_col = have_cols[j]
        temp_data[, temp_col] =temp_data[, temp_col]*temp_scale[2,temp_col]+ temp_scale[1,temp_col] 
        data[,temp_col] = temp_data
      }
      attr(data, 'scaled') = NULL
    } # end unscaling case
  } # end loop through keys
  return(data)
}
  
  
  

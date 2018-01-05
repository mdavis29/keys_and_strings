#' @title Key Scaler
#' @param data a data frame like object
#' @param val_col char vector of value columns (optional)
#' @param key_col key column (a seperate key will be made for each key)
#' @param verbose print debugging
#' @author Matthew Davis
#' @description creates seperate scales by a key
#' @return a keyscale_obj
#' @export
#'

keyscale = function(data, val_col = NULL, key_col = NULL, verbose = FALSE){
  data = data.frame(data)
  if(is.null(val_col)){ # there is no value col, use all numeric and integer cols
    val_col = colnames(data)[lapply(data, class) %in% c('numeric', 'integer')]
    val_col = val_col[!val_col %in% key_col]
  }
  if(is.null(key_col)){ # if there is no key col, create one with one value
    data$temp_key = rep(1, nrow(data))
    key_col = 'temp_key'
    if(verbose)print('using only 1 key, ie scaling everything together')
  }
  keys = unique(data[, key_col])
  nkeys = length(keys)
  key_output = as.list(keys)
  if(verbose)print(paste('keys', paste(keys, collapse = ',')))
  if(verbose)print(paste('val_col', paste(val_col, collapse = ',')))
  for(i in 1:nkeys){
    temp_key = keys[i]
    temp_data = as.matrix(data[data[, key_col] == temp_key, val_col])
    means = apply(temp_data,2,mean, na.rm = TRUE)
    means[is.na(means)] = 0
    sds = apply(temp_data,2,sd, na.rm = TRUE)
    sds[is.na(sds)] = 1
    sds[sds == 0] = 1
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
#' @param unscale unscale a scaled object ( will auto read when scaled attr is set from previous predict)
#' @param ... addtional params
#' @param verbose print debugging
#' @author Matthew Davis
#' @description creates seperate scales by a key
#' @return a data frame with either scale or unscaled values
#' @export
#'


predict.keyscale_obj <- function(object, data, unscale = FALSE, verbose = FALSE, ...){
  dsattr = attr(data, 'scaled')
  have_cols = intersect(colnames(object$scales[[1]]), colnames(data))
  keys = names(object$scales)
  key_col = object$key_col
  new_keys = setdiff(unique(data[,object$key_col]),names(object$scales) )
  lnk = length(new_keys)
  if(lnk>0){print(paste(lnk,'new keys found, passing through unscaled', sep = ':'))}
  nkeys = length(keys)
  nc = length(have_cols)
  if(!is.null(dsattr)){
    if(dsattr){unscale = TRUE}
  }
  if(nkeys == 1 & key_col %in% c('temp_key')){
    data[,'temp_key'] = rep(1, nrow(data))
    if(verbose)print('using only 1 key, ie scaling everything together')
  }
    for (i in 1:nkeys){
    temp_key = keys[i]
    temp_data = matrix(data[data[,key_col] == temp_key, have_cols ], 
                          ncol = length(have_cols), dimnames = list(NULL, have_cols) )
    temp_scale = object$scales[[temp_key]][, have_cols]
    if(unscale){ 
      if(verbose)print('using unscaler')
      for (j in 1:nc){
        temp_col = have_cols[j]
        if(verbose){
          print(paste('unscaling key',paste(temp_key, temp_col, sep = ':'), sep = ':'))
          print(paste('with length', nrow(temp_data)))
        }
        temp_data[,temp_col] =temp_data[, temp_col]*temp_scale[2,temp_col] + temp_scale[1,temp_col] 
        data[data[,key_col] == temp_key, temp_col] = temp_data[, temp_col]
      }
      attr(data, 'scaled') = FALSE
    } # end unscaling case
    
    if(!unscale){
      if(verbose){print('using scaler')}
      for (j in 1:nc){
        temp_col = have_cols[j]
        if(verbose){
          print(paste('scaling key',paste(temp_key, temp_col, sep = ':'), sep = ':'))
          print(paste('with length', nrow(temp_data)))
        }
        temp_data[, temp_col]=(temp_data[, temp_col] - temp_scale[1,temp_col])/temp_scale[2,temp_col] 
        data[data[,key_col] == temp_key, temp_col] = temp_data[, temp_col]
      }
    attr(data, 'scaled') = TRUE
    } # end scaling case
  } # end loop through keys
  if('temp_key' %in% colnames(data)){
    data[,'temp_key'] = NULL 
  }
  return(data)
}
  
  
  

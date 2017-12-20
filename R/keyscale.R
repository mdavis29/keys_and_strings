#' @title Key Scaler
#' @param data a data frame like object
#' @param params list(key_col = NULL, trans_cols = NULL, scale = TRUE, center = TRUE)
#' @author Matthew Davis
#' @description creates seperate scales by a key
#' @return a keyscale_obj
#' @export
#'

keyscale = function(data, params = list()){
  param_names = names(params)
  p = list(key_col = NULL, trans_cols = NULL, scale = TRUE, center = TRUE)
  l = length(param_names)
  if(l > 0){ #ceates list of parameters
    for ( i in 1:l ){
      p[param_names[i]] = params[param_names[i]]
    }
  }
  if(is.null(p$trans_cols)){
    p$trans_cols = colnames(data)[lapply(data, class) %in% c('integer', 'numeric')]
  }
  have_keys = unique(data[,p$key_col])
  nkeys = length(have_keys)
  scales = list(have_keys)
  for(i in 1:nkeys){
    temp_key = have_keys[i]
    temp_data =data[data[,p$key_col] == temp_key,p$trans_cols]
    means = apply(temp_data, 2, mean, na.rm = TRUE)
    sds = apply(temp_data, 2, sd, na.rm = TRUE)
    scales[[temp_key]] = list(means = means, sds = sds)
    means[is.na(means)] = 0
    sds[is.na(sds)] = 1
  }
  names(scales) = have_keys
  output = list(scales = scales, params = p)
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


predict.keyscale_obj <- function(object, data, rev = FALSE){
  have_keys = unique(data[,object$params$key_col])  
  new_keys = setdiff(have_keys , names(object$scales))
  nkeys = length(have_keys)
  have_cols = intersect(colnames(data),object$params$trans_cols)
  ncols = object$params$trans_cols
  if(length(new_keys)>0)warning(paste('found new_keys:', new_keys))
  if(!rev){
    for (i in 1:nkeys){
      temp_key = have_keys[i]
        temp_col = have_cols[i]
        temp_mean = object$scales[[temp_key]]$means[have_cols]
        temp_sd = object$scales[[temp_key]]$sds[have_cols]
        temp_data = data[data[,object$params$key_col] == temp_key, have_cols]
        temp_data_scaled = scale(temp_data, center = temp_mean, scale = temp_sd)
        data[data[,object$params$key_col] == temp_key, have_cols] = temp_data_scaled 
      
    }
    attr(data, 'scaled') <- TRUE
  }
  if(rev ){
    if(!attr(data, 'scaled'))warning('attr "scaled" not attached to data')
    for (i in 1:nkeys){
      temp_key = have_keys[i]
      temp_col = have_cols[i]
      temp_mean = object$scales[[temp_key]]$means[have_cols]
      temp_sd = object$scales[[temp_key]]$sds[have_cols]
      temp_data = data[data[,object$params$key_col] == temp_key, have_cols]
      temp_data_scaled = scale(temp_data, center = -temp_mean, scale = 1/temp_sd)
      data[data[,object$params$key_col] == temp_key, have_cols] = temp_data_scaled 
    }
  }
  return(data)
}
  
  
  

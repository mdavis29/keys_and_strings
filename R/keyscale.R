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
  }
  names(scales) = have_keys
  output = list(scales = scales, keys = have_keys, params = p)
  return(output)
}
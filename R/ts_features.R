#' @title Time Series Features from Data Frame
#' @param data a data frame or time series
#' @param params a list of parameter list'time_col = NULL,key_col = NULL,val_col = NULL,n_future = 6,n_past = 6
#' @param verbose print debugging output
#'
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export

ts_features<-function(data,params = list(), verbose  = TRUE){
  p = list( time_col = NULL, key_col = NULL, val_col = NULL, n_future = 6, n_past = 6, na_fill = NULL)
  param_names = names(params)
  l = length(param_names)
  if(l > 0){ #ceates list of parameters
    for ( i in 1:l ){
      p[param_names[i]] = params[param_names[i]]
    }
  }
  if(!all(c(p$time_col, p$key_col, p$val_col) %in% colnames(data))){warning('one of :time_col, key_col, val_col, missing from data')}  
  if(is.null(dim(data))){ # makes a data frame if the data is a factor
    data = data.frame(time = 1:(length(data)), 
                      val = data)
    p$val_col = 'val'
    p$time_col = 'time'
  }
  if(is.null(p$key_col)){ # creates a key column if there is none
    data[, 'key'] = rep(1, nrow(data))
    p$key_col = 'key'
  }
  if(is.null(p$val_col)){ # guessing val col if it's missing
    p$val_col = colnames(data)[!colnames(data) %in% c(p$time_col, p$key_col) ][1]
    print(paste('guessing val col', p$val_col))
  }
  keys_and_time = unique(data[, c(p$key_col, p$time_col)]) 
  if(nrow(keys_and_time) < nrow(data)){warning('unique keys and dates are less than data, ie data contains repeates')}
  have_keys = unique(data[,p$key_col])
  have_time = unique(data[, p$time_col])
  nkeys = length(have_keys)
  if(!is.null(p$na_fill) & !is.null(dim(data))){ # fills NA for missing date key combinations
    for(i in 1:nkeys){ # test each key for missing dates
      temp_key = have_keys[i]
      temp_key_data = data[data[,p$key_col] == temp_key  ,]
      temp_key_time = unique(temp_key_data[,p$time_col])
      missing_dates = setdiff( have_time, temp_key_time)
      n.missing = length(missing_dates)
      if(n.missing> 0 ){ # create back fill if missing
        back_fill = data.frame(matrix(p$na_fill, ncol = ncol(data), nrow = n.missing, 
                           dimnames = list(NULL, colnames(data))))
        back_fill[, p$time_col] = missing_dates
        back_fill[,p$key_col] = rep(temp_key, n.missing)
        data = rbind(data, back_fill)
        if(verbose){print(paste('NA back filling dates', paste(have_keys[i], missing_dates)))}
      }
    }
  } # end fill NA operations
  nr = nrow(data)
  output = matrix(NA, ncol = p$n_future+p$n_past, nrow  = nrow(data))
  past_names = c()
  future_names = c()
  if(p$n_past > 0 ){past_names = paste('p', p$n_past:1, sep = '')}
  if(p$n_future > 0){future_names = paste('f', 1:p$n_future, sep = '')}
  colnames(output) = c(past_names, future_names)
  for (i in 1:nr){
    temp_key = data[i,p$key_col]
    temp_time = data[i,p$time_col]
    temp_data = data[data[,p$key_col] %in% temp_key,]
    past = c()
    future = c()
    if(p$n_past>0){
      past = temp_data[ temp_data[, p$time_col] < temp_time,]
      past = tail(past[order(past[,p$time_col]),p$val_col], p$n_past)
      temp_n_past = length(past)
      if(temp_n_past <p$n_past){
        past =c( rep(NA, p$n_past-temp_n_past),past)
      }
    }
    if(p$n_future){
      future = temp_data[ temp_data[, p$time_col] > temp_time,]
      future = head(future[order(future[,p$time_col]),p$val_col], p$n_future)
      temp_n_future = length(future)
      if(temp_n_future < p$n_future){
        future = c(future, rep(NA, p$n_future - temp_n_future))
        }
      }
    output[i,] = c(past, future)
  }
  if(nkeys>1){
  output = cbind(output, data[, c(p$time_col, p$key_col)])
  }  
  return(output)
}
 



 

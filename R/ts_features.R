#' @title Time Series Features from Data Frame
#' @param data a data frame or time series
#' @param time_col char time col in data
#' @param key_col char primary key for the data frame 
#' @param val_col char value column in data frame 
#' @param n_future int number of future steps
#' @param n_past int number of past steps (lags)
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export

ts_features<-function(data, time_col = NULL, key_col = NULL, val_col = NULL, n_future = 6, n_past = 6){
  if(!all(c(time_col, key_col, val_col) %in% colnames(data))){warning('one of :time_col, key_col, val_col, missing from data')}  
  if(is.null(dim(data))){
    data = data.frame(time = 1:(length(data)), 
                      val = data)
    val_col = 'val'
    time_col = 'time'
  }
  if(is.null(key_col)){
    data[, 'key'] = rep(1, nrow(data))
    key_col = 'key'
  }
  if(is.null(val_col)){
    val_col = colnames(data)[!colnames(data) %in% c(time_col, key_col) ][1]
    print(paste('guessing val col', val_col))
  }
  nr = nrow(data)
  output = matrix(NA, ncol = n_future+n_past, nrow  = nrow(data))
  past_names = c()
  future_names = c()
  if(n_past > 0 ){past_names = paste('p', n_past:1, sep = '')}
  if(n_future > 0){future_names = paste('f', 1:n_future, sep = '')}
  colnames(output) = c(past_names, future_names)
  for (i in 1:nr){
    temp_key = data[i,key_col]
    temp_time = data[i,time_col]
    temp_data = data[data[,key_col] %in% temp_key,]
    past = c()
    future = c()
    if(n_past>0){
      past = temp_data[ temp_data[, time_col] < temp_time,]
      past = tail(past[order(past[,time_col]),val_col], n_past)
      temp_n_past = length(past)
      if(temp_n_past < n_past){
        past =c( rep(NA, n_past-temp_n_past),past)
      }
    }
    if(n_future){
      future = temp_data[ temp_data[, time_col] > temp_time,]
      future = head(future[order(future[,time_col]),val_col], n_future)
      temp_n_future = length(future)
      if(temp_n_future < n_future){
        future = c(future, rep(NA, n_future - temp_n_future))
        }
      
    }
    output[i,] = c(past, future)
    }
  return(output)
}
  

#' @title Split Key
#' @param x a character vector of text
#' @param chunk_len integer nchar length of each part of the key
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export
#' 

split_key <-function(x, chunk_len=3){
  key_len = unlist(lapply(x, function(x)nchar(x)))
  l = unique(key_len)
  if(length(l)>1)warning('key lengths are not consistant, no way to split')
  if(l %% chunk_len != 0 )warning('n in not a multiple of key length  (nchar/n)')
  newkey_list = lapply(x, chunk_string,  chunk_len = chunk_len )
  output = matrix(unlist(newkey_list), nrow = length(x), ncol = length(newkey_list[[1]]), byrow = TRUE)
  return(output)
}

#' @title chunk_string
#' @param x a text string (not a vector)
#' @param chunk_len integer nchar length of each part of the key
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a vector
#' @return a vector of the string chuncks
#' @export

chunk_string <- function(x, chunk_len=3 ){
  l = nchar(as.character(x))
  n_chunks = ceiling(l/chunk_len)
  if(l < chunk_len)stop(paste('string length smaller than chunck len',paste(l, n_chunks)))
  if(l %% chunk_len  != 0 ){warning('n_chunks in not a multiple of character length  (nchar/chunk_len )')}
  output = c() 
  for(i in 0:(n_chunks-1)){
    output = append(output, substr(x, (i*chunk_len)+1, i*chunk_len+chunk_len ))
  }
  return(output)
}






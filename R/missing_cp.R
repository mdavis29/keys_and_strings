#' @title missing calendar periods
#' @param data a vector of calendar perionds, c(201201, 201202)
#' @param return_completed logical return only missing periods, or a completed vector of calendar periods 
#' @param verbose print debugging
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a vector of missing calendar periods, or a vector of complete calendar periods from min to max data
#' @export

missing_cp <-function(data,return_completed = FALSE, verbose = FALSE){
  output = NULL
  max_cp = max(data, na.rm = TRUE)
  min_cp = min(data, na.rm = TRUE)
  new_cp = cp_seq(min_cp, max_cp - min_cp, use_first = TRUE )
  new_cp = new_cp[new_cp <=max_cp & new_cp >=min_cp]
  diffs = setdiff(new_cp, data)
  if(length(diffs)>0){
    output = diffs
    if(verbose)print(paste('missing', length(output), sep = ':'))
    if(return_completed){
      output = unique(c(data, output))
    }
    output = output[order(output)]
  }
  return(output)  
}
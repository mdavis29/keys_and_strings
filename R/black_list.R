#' @title black_list
#' @param data a data frame including factor variables
#' @param cols cols to look at minimum frequey
#' @param freq int miniumum frequency
#' @param white_list logical use a white list (inclusion) or blacklist exclusion 
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export
#' 
black_list = function(data, cols, freq = 12, white_list = FALSE){
  output = list()
  n = length(cols)
  for(i in 1:n){
    temp_table =  table(data[, cols[i]]) 
    if(!white_list){ temp_black_list = names(temp_table[temp_table < freq])}
    if(white_list){temp_black_list = names(temp_table[temp_table >= freq])}
    output[[i]] = temp_black_list
  }
  names(output) = cols
  class(output) = append('bl', class(output))
  attr(output, 'white_list') = white_list
  return(output)
}

#' @title predict method for blacklist object 
#' @param data a data frame including factor variables
#' @param cols cols to look at minimum frequey
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export
#' 
predict.bl = function(object, data, keep_rows = FALSE, val = 'black_listed'){
  have_names = intersect(names(object), colnames(data))
  n_names = length(have_names)
  temp_data = data
  white_list = attr(bl, 'white_list')
  for(i in 1:n_names){
    temp_name = have_names[i]
    temp_blist = object[[temp_name]]
    if(!white_list){
      if(!keep_rows){
      temp_data = temp_data[!temp_data[, have_names[i]] %in% temp_blist,]
      }
      if(keep_rows){
        temp_col = temp_data[,have_names[i]]
        levels(temp_col) = append(levels(temp_col), val)
        temp_col[temp_col %in% temp_blist] = val
        temp_data[,have_names[i]] = temp_col
        }
      }
    if(white_list){
      if(keep_rows){
        temp_data = temp_data[temp_data[, have_names[i]] %in% temp_blist,]
      }
      if(keep_rows){
        temp_col = temp_data[,have_names[i]]
        levels(temp_col) = append(levels(temp_col), val)
        temp_col[!temp_col %in% temp_blist] = val
        temp_data[,have_names[i]] = temp_col
      }
    }
  }
  temp_data = droplevels(temp_data)
  return(temp_data)  
}
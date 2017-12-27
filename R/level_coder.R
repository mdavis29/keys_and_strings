#' @title level_coder
#' @param data a data frame including factor variables
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export
#' 
# create the level coder object
level_coder <- function(data){
  if(is.null(dim(data))){
    data = data.frame(x = as.factor(data))
  }
    
  old_levs = lapply(data, function(x)levels(x))
  old_levs =  old_levs[unlist(lapply( old_levs, function(x)!is.null(x)))]
  new_levs = list()
  n = length(old_levs)
  codes = expand.grid(letters, letters, letters)
  codes = apply( codes, 1, function(x) paste(x, collapse = ''))
  for(i in 1:n){
    l = length(old_levs[[i]])
    x = codes[1:l]
    names(x) = old_levs[[i]]
    new_levs[names(old_levs)[i]] = list(x)
  }
  if(!all( names(old_levs) == names(new_levs))){stop('warning level mismatch in old and new keys')}
  class(new_levs) = c('level_coder_obj', class(new_levs))
  return(new_levs)
}

#' @title predict method for level_coder_obj
#' @param object a 'level_coder_obj' class object
#' @param data data frame of new data
#' @param rev reverse the transformation
#' @param return_primkey use a primary key
#' @param ... addtional params
#' @param verbose print debugging
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export

predict.level_coder_obj<-function(object, data, rev = FALSE, return_primkey = FALSE, verbose  = FALSE, ...){
  # case when data is a primary key
  primkey = NULL
  if(!is.null(attr(data,'primkey'))){
    if(attr(data,'primkey')){
      primkey = as.character(data)}
  }
  if(rev & is.null(dim(data))){
    primkey = as.character(data)
  }
 
  # case when data is a df and contains a primary key
  if(!is.null(primkey)){
    if(verbose)print('using primary key')
    data = split_key(primkey, 3)
    if(ncol(data)!=length(names(object))){stop('key split into different numbner of columns than object names')}
    colnames(data) = names(object)
    data[data == 'zzz'] = NA
    rev = TRUE
    if(verbose)print(head(data))
  }
  # case when data is a factor or a matrix
  if(is.null(dim(data))){
    data = data.frame(x = data)
  }
  if(class(data) %in% 'matrix'){
    data = data.frame(data)
  }
  # case where data is going to be encoded
  if(verbose)print('encoding')
  have_cols = intersect(names(object), colnames(data))
  need_cols = setdiff(names(object), colnames(data))
# if there are missing columns in the data required by the encoder
  if(verbose){
    print('final dims')
    print(dim(data))
    print(head(data),2)
    print(paste('have_cols', paste(have_cols, collapse = ',')))
    print(paste('need_cols', paste(need_cols, collapse = ',')))
  }
  if(length(need_cols)!=0){
    stop(paste('missing cols in data', paste(need_cols, collapse = ',')))
  }
  nc = length(have_cols)
  for(i in 1:nc){
    temp_col_name = have_cols[i]
    temp_col  = as.factor(data[, temp_col_name])
    if(!rev){
      temp_old_levs = levels(temp_col)
      temp_new_levs = object[[temp_col_name]]
      common_levs = intersect(names(temp_new_levs), temp_old_levs )
      if(length(common_levs)!=length(temp_old_levs)){
        stop(paste('level found in data, not found in encoder', paste(temp_col_name)))
      }
      levels(temp_col) = temp_new_levs[common_levs] 
      data[, temp_col_name] = temp_col
      
            }
    if(rev){
      if(verbose)print(paste('reversing', temp_col_name))
      temp_old_levs = levels(temp_col)
      temp_new_levs = object[[temp_col_name]]
      common_levs = intersect(temp_new_levs, temp_old_levs )
      if(length(common_levs)!=length(temp_old_levs)){
        stop(paste('level found in data, not found in encoder', paste(temp_col_name)))
      }
      levels(temp_col) = names(temp_new_levs[temp_new_levs == common_levs]) 
      data[, temp_col_name] = temp_col
    }
  } # end looping through columns
  if(return_primkey){
    # handels NAs in factors as zzz
    for (i in 1:nc){
      temp_col_name = have_cols[i]
      temp_col = data[, temp_col_name]
      levels(temp_col) = append(levels(temp_col), 'zzz')
      temp_col[is.na(temp_col)] = 'zzz'
      data[,temp_col_name] = droplevels(temp_col)
      }
  data = apply(data, 1, function(x)paste(x, collapse = ''))
  attr(data, 'primkey') = TRUE
  }
  return(data)
}  



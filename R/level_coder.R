#' @title level_coder
#' @param data a data frame including factor variables
#' @param rm_null_levs logical whether to remove null levels
#' @author Matthew Davis
#' @param verbose print debugging
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export
#' 
# create the level coder object
level_coder <- function(data, verbose = TRUE, rm_null_levs=TRUE){
  if(is.null(dim(data))){
    data = data.frame(x = as.factor(data))
  }
    
  old_levs = lapply(data, function(x)levels(x))
  old_levs =  old_levs[unlist(lapply( old_levs, function(x)!is.null(x)))]
  if(verbose){print(old_levs)}
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
  if(rm_null_levs){
    n = length(new_levs)
    for(i in 1:n){
      if(verbose)print(paste('removing NA levels from', names(new_levs[i])))
      new_levs[[i]] = new_levs[[i]][nchar(names(new_levs[[i]]))>0]
    }
  }
  return(new_levs)
}

#' @title predict method for level_coder_obj
#' @param object a 'level_coder_obj' class object
#' @param data data frame of new data
#' @param is_primkey logical if the data is a primary key
#' @param return_primkey use a primary key
#' @param ... addtional params
#' @param verbose print debugging
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export

predict.level_coder_obj<-function(object, data, is_primkey = FALSE, return_primkey = FALSE, verbose = FALSE, ...){
  # case when data is a primary key
  primkey = NULL
  if(!is.null(attr(data,'primkey'))){
    if(attr(data,'primkey')){
      primkey = as.character(data)}
  }
  if(is_primkey & is.null(dim(data))){
    primkey = as.character(data)
  }
  # case when data is a df and contains a primary key
  if(!is.null(primkey)){
    if(verbose)print('using primary key')
    data = split_key(primkey, 3)
    if(ncol(data)!=length(names(object))){stop('key split into different numbner of columns than object names')}
    colnames(data) = names(object)
    data[data == 'zzz'] = NA
    data = droplevels(data.frame(data))
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
    temp_levs = levels(temp_col)
    nlevs = length(temp_levs)
    temp_obj =  object[[temp_col_name]]
    for (j in 1:nlevs){
      temp_lev = temp_levs[j]
      if(!is.null(temp_lev)){
        if(temp_lev %in% names(temp_obj)){
        temp_levs[temp_levs == temp_lev] = temp_obj[[temp_lev]]
        if(verbose)print(paste(temp_col_name, paste(temp_lev,'encoding')))
      }
        if(temp_lev %in% temp_obj) {
        temp_levs[temp_levs == temp_lev] = names(temp_obj)[temp_obj == temp_lev] 
        if(verbose)print(paste(temp_col_name, paste(temp_lev,'reversing')))
      }
        if(!temp_lev %in% c(temp_obj, names(temp_obj))){
        temp_levs[temp_levs == temp_lev] = NA
        if(verbose)print(paste(temp_col_name, paste(temp_lev,'not founding object, mapping to NA Level')))
        }
      }
    }
    levels(data[, temp_col_name]) = temp_levs
  }
  if(return_primkey){
    primkey = data.frame(data[, names(object)])
    nc = ncol(primkey)
    for(i in 1:nc){
      levels(primkey[,i]) = append(levels(primkey[,i]), 'zzz')
      primkey[is.na(primkey[,i]),i] = 'zzz'
    }
  primkey = apply(primkey, 1,function(x)paste(x, collapse = ''))  
  attr(primkey, 'primkey') = TRUE
  data = primkey
  }
 return(data)
}  



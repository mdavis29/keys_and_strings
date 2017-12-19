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
    data = data.frame(x = data)
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
#' @param use_primkey use a primary key
#' @param primkey_col column to find the primary key
#' @param verbose print debugging
#' @author Matthew Davis
#' @description splits a string in to chunks of length chunk_len, returns a matrix
#' @return a matrix of split text
#' @export

predict.level_coder_obj<-function(object, data, rev = FALSE, use_primkey = FALSE, primkey_col = 'primkey', verbose  = FALSE){
  if(is.null(dim(data))){
    data = data.frame(x = data)
  }
  have_names = colnames(data)
  if(verbose)print(have_names)
  need_names = names(object)
  if(verbose)print(need_names)
  if(use_primkey & rev){
    if(ncol(data)==1){
      if(verbose)prinnt('assuming the factor is the primary key')
      colnames(data) = primkey_col
    }
    primkeys = unlist(data[, primkey_col])
    key_df = data.frame(split_key(primkeys))
    if(ncol(key_df)!=length(names(object))){stop('more columns on key split that in level_encoder_obj')}
    colnames(key_df) = names(object) 
    data = data.frame(key_df, data )
    if(ncol(key_df)==1){# addresss the case with only one name in the object
      colnames(data)[1] = names(object)[1]
    }
    have_names = colnames(data)
 }
  if(!all(need_names %in% have_names)){stop('missing names in encoder, not in cols of data')}
  n = length(need_names)
  for (i in 1:n){
    temp_name = need_names[i]
    if(verbose)print(temp_name)
    temp_levs = levels(data[,temp_name])
    if(verbose)print(temp_levs)
    for (j in 1:(length(temp_levs))){
      temp_obj = object[[temp_name]]
      if(!rev){
        temp_levs[[j]] = temp_obj[temp_levs[j]]
      }
      if(rev){
        temp_levs[[j]] = names(temp_obj[temp_obj == temp_levs[j]])
      }
      levels(data[, temp_name]) = temp_levs
    }
  }
  if(use_primkey & !rev){
    primkey  = c()
    for(i in 1:(nrow(data))){
      primkey = append(primkey, paste(as.character(unlist(data[i,need_names])), collapse = ''))
    }
    data = data[, !colnames(data) %in% need_names]
    data$primkey = primkey
  }
  if(use_primkey & rev){
    data = data[, !colnames(data)%in% primkey_col ]
  }
  return(data)
}  



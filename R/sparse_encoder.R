#' @title  sparseEncoder
#' @param data a dataframe with factor columns
#' @param minFreq minimum frequency of occucrence of factor levels
#' @param imputer_fun function to pass in for missing value imputation
#' @param f formula for encoding 
#' @param verbose print debugging
#' @import Matrix
#' @description sparse one hot catagorical encoder
#' @return a transformation object 
#' @export
sparseEncoder <- function(data, minFreq = 0.01, imputer_fun = median, f=formula(~.-1), verbose = FALSE){

  col_types = lapply(data, class)
  cat_cols = names(col_types[col_types == 'factor'])
  num_cols = names(col_types[col_types %in% c('integer', 'numeric')])
  options(na.action = 'na.pass')
  cat_mat = Matrix::sparse.model.matrix(f, data = data[,cat_cols], row.names=FALSE, drop.unused.levels=TRUE,na.action='na.pass')
  print(dim(data[,cat_cols]))
  print(dim(cat_mat))
  freqs = Matrix::colSums(cat_mat, na.rm=TRUE)/nrow(cat_mat)
  keep = names(freqs)[freqs >= minFreq]
  cat_mat = cat_mat[, keep]
  new_cat_cols = colnames(cat_mat)
  output = list(cat_cols = cat_cols, 
                new_cat_cols = new_cat_cols,
                f = f,
                imputer  = NULL)
  if(length(num_cols)> 0 ){
    print('using imputer')
    num_data = data.frame(data[, num_cols])
    colnames(num_data) = num_cols
    imputer = apply(num_data, 2, imputer_fun , na.rm = TRUE)
    if(verbose)print(imputer)
    output[['imputer']] = imputer
  }
  class(output)<-append( 'encoder', class(output))
  return(output)  
}
#' @title  predict method sparseEncoder
#' @param object a dataframe with factor columns
#' @param data a dataframe to encode using pre trained encoder
#' @param ... additional arguments
#' @import Matrix
#' @description transformation/predict method for catgories
#' @return a sparse matrix
#' @export

predict.encoder<-function(object, data, ...){
  options(na.action = 'na.pass')
  have_cols = colnames(data)
  num_cols = names(object$imputer)
  cat_mat = NULL
  if(!is.null(num_cols)){
    num_data = data[,num_cols]
    li = length(num_cols)  
    if(length(li) > 0 ){
      for (i in 1:(li)){
        temp = num_data[, num_cols[i]]  
      temp[is.na(temp)] = object$imputer[i]
      num_data[,num_cols[i]] = temp
      }
    }
    num_data = sparse.model.matrix(object$f, num_data)
  }
  if( length(object$cat_cols) > 0){
    cat_cols = intersect(object$cat_cols, have_cols)
    missing_cols = cat_cols[!cat_cols %in% object$cat_cols]
    if(length(missing_cols)>0)print(paste('warining missing', missing_cols))
    cat_mat =  Matrix::sparse.model.matrix(~.-1, data[,cat_cols], row.names=FALSE, drop.unused.levels=TRUE, na.action='na.pass')
    have_new_cats = colnames(cat_mat)
    keep_new_cats =  have_new_cats[have_new_cats %in% object$new_cat_cols]
    missing_new_cats = object$new_cat_cols[!object$new_cat_cols %in% keep_new_cats]
    if(length(missing_new_cats) > 0){
      missingMat =  Matrix::Matrix(0, 
                          ncol = length(missing_new_cats), 
                          nrow = nrow(cat_mat), 
                          sparse = TRUE)
      colnames(missingMat) = missing_new_cats
      cat_mat = cbind(cat_mat[,keep_new_cats], missingMat)
      cat_mat = cat_mat[, object$new_cat_cols]
    }
  }
  output = cbind(cat_mat, num_data)
  return(output)
}

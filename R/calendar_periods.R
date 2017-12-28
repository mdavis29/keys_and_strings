#' @title cp add 1 
#' @param x a signle calendar period ie: 201202
#' @param n int number of months to add
#' @author Matthew Davis
#' @description adds months to a calendar period, hidden helper function
#' @return a integet calendar period
#' @export
cp_add_1 = function(x, n){
  x = as.character(x)
  y = as.numeric(substr(x, 1,4))
  m =  as.numeric(substr(x, 5,6))
  ny = as.character(y + (n+m)%/% 12)
  nm = as.character((n+m)%%12)
  if(nm == '0'){
    nm = '12'
    ny = as.character(as.numeric(ny) - 1)
  }
  if(nchar(nm) == 1){nm = paste('0', nm, sep = '')}
  output = as.numeric(paste(ny, nm, sep = ''))
  return(output)
}

#' @title cp add  
#' @param x int vector of calendar periods ie: 201202
#' @param n int number of months to add
#' @author Matthew Davis
#' @description adds months to a calendar period, hidden helper function
#' @return a integer calendar period
#' @export

cp_add = function(x ,n = 6){
  unlist(lapply(x, function(x)cp_add_1(x, n)))
}

#' @title cp to fp   
#' @param x int vector of calendar periods ie: 201202
#' @param n int number of months to add
#' @param rev logical, reverse and go from fiscal period to calendar period 
#' @author Matthew Davis
#' @description adds months to a calendar period, hidden helper function
#' @return a integer calendar periods
#' @export

cp_to_fp = function(x, n = 6, rev = FALSE){
  if(!is.null(attr(x, 'fiscal'))){
    if(attr(x, 'fiscal'))rev = TRUE
    if(!attr(x, 'fiscal'))rev = FALSE
  }
  if(rev){n = -1*n}
  output = unlist(lapply(x, function(x)cp_add_1(x, n)))
  attr(output, 'fiscal') = !rev
  return(output)
}

#' @title cp_seq 
#' @param x int calendar period to start ie 201202
#' @param n int number of months to add in the sequence
#' @param use_first logical, in cluded x (as the start of the sequence) 
#' @author Matthew Davis
#' @description creates a sequence of calendar periods
#' @return a integer calendar period
#' @export

cp_seq = function(x, n = 6, use_first = TRUE){
  y = x
  n = seq(n) 
  if(use_first)n = n-1
  output = unlist(lapply(n, function(x)cp_add_1(y, x)))
  return(output)
}

#' @title CP to Calendar 
#' @param x int calendar period to start ie 201202
#' @param fp whether the calindar period is fiscal 
#' @author Matthew Davis
#' @import timeDate
#' @description finds holidays and buisness days for a calendar period
#' @return a vector of business days and holiday/weekends
#' @export

cp_to_calendar_1 = function(x, fp = FALSE){
  if(!is.null(attr(x, 'fiscal'))){
    if(attr(x, 'fiscal'))fp= TRUE
    if(!attr(x, 'fiscal'))fp = FALSE
  }
  if(fp){x =cp_to_fp(x)}
  x1 = cp_add_1(x,1)
  x = as.character(x)
  y = substr(x, 1,4)
  m = substr(x,5,6) 
  y1 = substr(x1, 1,4)
  m1 =  substr(x1, 5,6)
  start = as.Date(paste(paste(y,m, sep = '-'), 01, sep = '-'))
  end = as.Date(paste(paste(y1,m1, sep = '-'), 01, sep = '-'))-1
  time_seq = timeDate::timeSequence(start, end)
  output = c(holidays_weekends = sum(timeDate::isHoliday(time_seq)), 
            biz_days = sum(timeDate::isBizday(time_seq)) )
  return(output)
}

#' @title CP to Calendar non parallel
#' @param x int calendar period to start ie 201202
#' @param fp whether the calindar period is fiscal 
#' @author Matthew Davis
#' @description creates a sequence of calendar periods
#' @return a vector of business days and holiday/weekends
#' @export

cp_to_calendar_np = function(x, fp = FALSE){
  t(sapply(x, function(y)cp_to_calendar_1(y, fp = fp)))
  }
  
#' @title CP to Calendar 
#' @param x int calendar period to start ie 201202
#' @param fp whether the calindar period is fiscal 
#' @author Matthew Davis
#' @import parallel
#' @description creates a sequence of calendar periods
#' @return a vector of business days and holiday/weekends
#' @export 

cp_to_calendar = function(x, fp = FALSE){
  cores = parallel::detectCores()
  cl = parallel::makeCluster(cores)
  parallel::clusterEvalQ(cl, library(keysandstrings))
  output = parallel::clusterApply(cl, x=x, fun = cp_to_calendar_np )
  parallel::stopCluster(cl)
  output = matrix(unlist(output), ncol = 2, nrow = length(x), byrow = TRUE, dimnames = list(NULL, c("holidays_weekends", "biz_days")))
  return(output)
}


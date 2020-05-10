#' resampling 
#' 
#' resampling 
#'
#' @param xdata data set
#' @return None
#'
#'
#'
#'
#'
rsampl<- function(xdata=xdata){
  nr<- nrow(xdata)
  boot_id<- sample (1:nr,nr,replace=TRUE)
  boot_dat<- data.frame(xdata[boot_id,])
  return(boot_dat)
}

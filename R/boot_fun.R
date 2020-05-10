#' double bootstrap 
#' 
#' double bootstrap 
#'
#' @param nboot  number of boots
#' @param xdata sub data set
#' @return None
#'
#'
#'
#'
#'

boot_fun<- function(nboot,xdata=xdata){
  replicate(nboot,rsampl(xdata=xdata),simplify=FALSE)
}
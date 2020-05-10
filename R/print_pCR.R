#' print estimation results
#'
#' print estimation results
#'
#' @param x estimation output
#' @param ... other arguments
#' @return None
#'
#'
#' @export


print_pCR<- function(x,...){
  if(!is.null(cl<- x$call)){
    cat("Call:\n")
    dput(cl)
  }
  cat("pCR Threshold Estimate:\n")
  pCRL<- x$pCRL
  pCRU<- x$pCRU
  if(pCRL>=pCRU){
    temp<- pCRL
    pCRL<- pCRU
    pCRU<- temp
    }
  b<- c(x$pCR,pCRL,pCRU)
  names(b)<- c("pCR",paste0(1-x$alpha,"LB"),paste0(1-x$alpha,"UB"))

  print(b)
  invisible(x)
  cat("-----------------------------------------------------------\n")
}

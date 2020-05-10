#' double bootstrap confidence interval
#'
#' double bootstrap confidence interval
#'
#' @param N number of replicates
#' @param rho correlation estimator
#' @param alpha level of significant for rho confidence interval
#' @return None
#'
#'
#'
#'
boot_ci<- function(N,rho,alpha){
  F_corr<- 1/2*log((1+rho)/(1-rho))
  F_corr_se<- 1/sqrt(N-3)
  F_uci<- F_corr+qnorm(1-alpha/2)*F_corr_se
  F_lci<- F_corr+qnorm(alpha/2)*F_corr_se
  rho_u<- (exp(2*F_uci)-1)/(exp(2*F_uci)+1)
  rho_l<- (exp(2*F_lci)-1)/(exp(2*F_lci)+1)
  return(list(rho_u=rho_u,rho_l=rho_l))
}

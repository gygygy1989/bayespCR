#' conditional posterior
#'
#' conditional posterior
#'
#' @param x delta_pCR
#' @param mu_nz mean of z (log hr of EFS)
#' @param mu_nx mean of x (delta_pCR)
#' @param sd_nz  sd of z
#' @param sd_nx  sd of x
#' @param rho_n  correlation
#' @return None
#'
#'
#'
con_post_fun<- function(x,mu_nz,mu_nx,sd_nz,sd_nx,rho_n){
  mu_nzx<- mu_nz+rho_n*(sd_nz/sd_nx)*(x-mu_nx)
  if((1-rho_n^2)<0){
    sd_nzx<- 0
  }else{
    sd_nzx<- sd_nz*sqrt(1-rho_n^2)
  }
  return(list(mu_nzx=mu_nzx,sd_nzx=sd_nzx))
}

#' posterior estimator
#'
#' posterior estimator
#'
#' @param x  delta_pCR
#' @param mu0 prior of mean of z (log hr of EFS)
#' @param sigma0 priof covariance
#' @param smu estimated mu
#' @param ssigma  covariance matrix
#' @param cri_hr  target HR
#' @return None
#'
#'
#'
post_fun<- function(x,mu0,sigma0,smu,ssigma,cri_hr){
  # joint posterior dist
  mu_n<- ssigma%*%(solve(sigma0+ssigma))%*%mu0+sigma0%*%(solve(sigma0+ssigma))%*%smu
  sigma_n<- sigma0%*%(solve(sigma0+ssigma))%*%ssigma


  # conditional posterior dist for Z
  mu_nx<- mu_n[1]
  mu_nz<- mu_n[2]
  sd_nx<- sqrt(diag(sigma_n)[1])
  sd_nz<- sqrt(diag(sigma_n)[2])
  rho_n<- sigma_n[1,2]/(sd_nx*sd_nz)

  con.out<- con_post_fun(x=x,mu_nz=mu_nz,mu_nx=mu_nx,sd_nz=sd_nz,sd_nx=sd_nx,rho_n=rho_n)
  mu_nzx=con.out$mu_nzx
  sd_nzx=con.out$sd_nzx


  log_cri_hr<- log(cri_hr)
  post_z<- pnorm(log_cri_hr,mean=mu_nzx,sd=sd_nzx)
  out<- c(x,post_z)
  names(out)<- c("diff_pCR","post_prob")
  return(out)
}

#' estimation main function
#'
#' estimation main function
#'
#' @param M number of replicates
#' @param nboot number of boots
#' @param alpha level of significant for rho confidence interval
#' @param n sample size in Phase II
#' @param mm additional patients in Phase III
#' @param lambda0 baseline hazard rate with pCR=No
#' @param pp0 pCR rate in control arm
#' @param pp1 pCR rate in trt arm
#' @param hr_pcr exp(beta2)
#' @param mu1 prior for delta pCR mean in Phase II
#' @param mu2_hr prior for HR mean in Phase III
#' @param v20  loghr variance
#' @param rho0 suppose highly correlated
#' @param cri target HR
#' @param tau cutoff posterior probability
#' @return None
#'
#'
#' @export


main_fun<- function(M,nboot,alpha,n,mm,lambda0,pp0,pp1,hr_pcr,mu1,mu2_hr,v20,rho0,cri,tau){

  #cat("Running, please wait...\n")
  ##### get data and coxph fit #####
  #set.seed(10)
  tryCatch({
    xdata<- my_stats(M=M,n=n,mm=mm,lambda0=lambda0,pp0=pp0,pp1=pp1,hr_pcr=hr_pcr,mu2_hr=mu2_hr)
    rho<- boot_out(nboot=nboot,xdata=xdata)
    rho_ci<- boot_ci(N=M,rho=rho,alpha=alpha)
    rho_u=rho_ci$rho_u
    rho_l=rho_ci$rho_l
    sum_dat<- xdata$summary


    ##### data summary #####
    yx<- sum_dat[1]
    yz<- sum_dat[3]
    vx<- sum_dat[2]^2
    vz<- sum_dat[4]^2
    cov<- sqrt(vx)*sqrt(vz)*rho
    cov_u<- sqrt(vx)*sqrt(vz)*rho_u
    cov_l<- sqrt(vx)*sqrt(vz)*rho_l
    sigma<- matrix(c(vx,cov,cov,vz),nrow=2,ncol=2,byrow=T)
    sigma_u<- matrix(c(vx,cov_u,cov_u,vz),nrow=2,ncol=2,byrow=T)
    sigma_l<- matrix(c(vx,cov_l,cov_l,vz),nrow=2,ncol=2,byrow=T)

    ###########################################


    ###### prior #####
    mu2<- log(mu2_hr)
	  v10 <- pp1*(1-pp1)/n
    cov0<- sqrt(v10)*sqrt(v20)*rho0
    sigma0<- matrix(c(v10,cov0,cov0,v20),nrow=2,ncol=2,byrow=T)
    ############################################


    ##### postior #####
    x<- seq(0.001,1,by=0.001)
    post_out<- lapply(x,post_fun,mu0=c(mu1,mu2),sigma0=sigma0,smu=c(yx,yz),ssigma=sigma,cri_hr=cri)
    post_out<- data.frame(do.call(rbind,post_out))

    target_pos<- which.min(abs(post_out$post_prob-tau))
    target_pCR<- post_out$diff_pCR[target_pos]

    ### upper
    post_out_u<- lapply(x,post_fun,mu0=c(mu1,mu2),sigma0=sigma0,smu=c(yx,yz),ssigma=sigma_u,cri_hr=cri)
    post_out_u<- data.frame(do.call(rbind,post_out_u))

    target_pos_u<- which.min(abs(post_out_u$post_prob-tau))
    target_pCR_u<- post_out_u$diff_pCR[target_pos_u]



    ### lower
    post_out_l<- lapply(x,post_fun,mu0=c(mu1,mu2),sigma0=sigma0,smu=c(yx,yz),ssigma=sigma_l,cri_hr=cri)
    post_out_l<- data.frame(do.call(rbind,post_out_l))

    target_pos_l<- which.min(abs(post_out_l$post_prob-tau))
    target_pCR_l<- post_out_l$diff_pCR[target_pos_l]


  },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  return(list(target_pCR=target_pCR,target_pos=target_pos,target_pCR_u=target_pCR_u,target_pCR_l=target_pCR_l,post_out=post_out))
}


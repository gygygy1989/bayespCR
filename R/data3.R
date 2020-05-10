#' estimator generation.
#'
#' estimator generation.
#'
#' @param M number of replicates
#' @param n sample size in Phase II
#' @param mm additional patients in Phase III
#' @param lambda0 baseline hazard rate with pCR=No
#' @param pp0 pCR rate in control arm
#' @param pp1 pCR rate in trt arm
#' @param hr_pcr exp(beta2)
#' @param mu2_hr prior for HR mean in Phase III
#' @return None
#'
#'
#'



my_stats<- function(M=1000,n=300,mm=300,lambda0=1,pp0=0.5,pp1=0.7,hr_pcr=0.24, mu2_hr=0.7){

  diff<- c()
  lghr0<- c()
  cr<- c()
  for(i in 1:M){
    sj.data<- data_fun(n=n,mm=mm,lambda0=lambda0,pp0=pp0,pp1=pp1,hr_pcr=hr_pcr,mu2_hr=mu2_hr)
    diff[i]<- sj.data$diff
    lghr0[i]<- sj.data$lghr0
    cr[i]<- sj.data$cen3
  }

  mean_delta<- mean(diff)
  sd_delta<- sd(diff)
  delta<- rnorm(M,mean=mean_delta,sd=sd_delta)


  mean_cr<- mean(cr)
  mean_lghr0<- mean(lghr0)
  d0<- (n+mm)/2*(1-mean_cr)
  d1<- d0*mean(exp(lghr0))
  sd_lghr0<- sqrt(1/d1+1/d0)


  #mean_hr<- log(R1*(pp0+delta)+R0*(1-pp0-delta))/log(R1*pp0+R0*(1-pp0))
  #beta_pcr=log(hr_pcr)
  #beta_trt=log(mu2_hr)-beta_pcr*(pp1-pp0)
  #mean_hr<- exp(beta_trt+beta_pcr*(pp0+delta))/exp(beta_pcr*pp0)
  #mean_lghr<- log(mean_hr)
  mean_lghr<- lghr0
  sd_lghr<- sd_lghr0


  lghr<- c()
  for(i in 1:length(mean_lghr)){
  lghr[i]<- rnorm(1,mean=mean_lghr[i],sd=sd_lghr)
  }


  dat<- cbind(seq(1,M),delta,lghr)
  sum_data<- c(mean_delta,sd_delta,mean_lghr0,sd_lghr0)
  names(sum_data)<- c("yx","sdx","yz","sdz")
  return(list(dat=dat,summary=sum_data))
}

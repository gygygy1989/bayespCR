#' run the main function
#'
#' run the main function
#'
#' @param alpha significant level
#' @param mu2_hr prior of HR mean
#' @param cri target HR
#' @param tau  cutoff posterior probability
#' @param nboot  number of boots
#' @importFrom survival coxph Surv
#' @importFrom simsurv simsurv
#' @importFrom stats rbinom rnorm sd qnorm cor pnorm
#' @importFrom crayon silver
#'
#'
#' @return None
#'
#'
#' @export

pCR_est<- function(alpha,mu2_hr,cri,tau,nboot){

  call<- match.call()
  if(missing(alpha)){alpha=0.05;cat(silver("Note: alpha not specified, default=0.05 \n"))}
  if(missing(mu2_hr)){mu2_hr=0.71;cat(silver("Note: prior of HR not specified, default=0.7 \n"))}
  if(missing(cri)){cri=0.7;cat(silver("Note: HR target not specified, default=0.7 \n"))}
  if(missing(tau)){tau=0.8;cat(silver("Note: posterior probability not specified, default=0.8 \n"))}
  if(missing(nboot)){nboot=500;cat(silver("Note: boots # not specified, default=500 \n"))}
                              cat("-----------------------------------------------------------\n")

  cat("Running, please wait...\n")

  fun<-  main_fun(M=500,nboot=nboot,alpha=alpha,n=300,mm=300,lambda0=0.4,pp0=0.5,pp1=0.7,hr_pcr=0.24,
                      mu1=0.15,mu2_hr=mu2_hr,v20=0.006,rho0=-0.8,cri=cri,tau=tau)

  dif<- fun$post_out[fun$target_pos,2]-tau
      if(dif<= -0.05){
        cat("Cannot reach the target, the posterior prob=",fun$post_out[fun$target_pos,2],"\n")
      }else{
        pCR<- fun$target_pCR
        pCR.lower<- fun$target_pCR_l
        pCR.upper<- fun$target_pCR_u

      }


  fit<- list()
  class(fit)<- c("pCR Threshold")
  fit$pCR<- pCR
  fit$pCRL<- pCR.lower
  fit$pCRU<- pCR.upper
  fit$call<- call
  fit$alpha<- alpha
  cat("Done! \n")
  print_pCR(fit)
}

################################################


#' data generation.
#'
#' data generation.
#'
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

#phase II/III
data_fun<- function(n,mm,lambda0,pp0,pp1,hr_pcr,mu2_hr){

  ### phase II
  id2<- seq(1,n,1)
  id.name2<- id.fun(nn=n)
  treat2<- rbinom(n,1,0.5)
  pCR<- c()
  for(i in 1:n){
    if(treat2[i]==1){
      pCR[i]<- rbinom(1,1,pp1)
    }else if (treat2[i]==0){
      pCR[i]<- rbinom(1,1,pp0)
    }
  }


  ### phase III
  id.add<- seq(n+1,n+mm,1)
  id.name.add<- id.fun(nn=mm)
  treat.add<- rbinom(mm,1,0.5)
  pCR3<- c(pCR,rep(NA,mm))

  tot<- n+mm # total patients
  id3<- c(id2,id.add)
  id.name3<- c(id.name2,id.name.add)
  trt3=c(treat2,treat.add)

  for(i in 1:tot){
    if(trt3[i]==1){
      pCR3[i]<- rbinom(1,1,pp1)
    }else if (trt3[i]==0){
      pCR3[i]<- rbinom(1,1,pp0)
    }
  }

  trt_pcr <- 1*((pCR3==1) & (trt3==1))
  treat3<- data.frame(trt=trt3, pCR=pCR3, trt_pcr=trt_pcr)


  ## Stratified cox model
  # HR(EFS)=exp(beta_trt+beta_pcr*X_1pcr)/exp(beta_pcr*X_0pcr)
  beta_pcr=log(hr_pcr)
  beta_trt=log(mu2_hr)
  beta_sum=beta_pcr+beta_trt
  #lambda1=mu2_hr*lambda0

  # simulate stratified survival time
  # lambda0 is the hazard rate of control arm w/o pCR
  s1.c.p <- simsurv(lambdas = lambda0, gammas = 1, betas = c(pCR = beta_pcr), x = treat3, maxt = 5)
  s1.a.np <- simsurv(lambdas = lambda0, gammas = 1, betas = c(trt = beta_trt), x = treat3, maxt = 5)
  s1.a.p <- simsurv(lambdas = lambda0, gammas = 1, betas = c(trt_pcr = beta_sum), x = treat3, maxt = 5)

  s1_Time = (trt3==0)*s1.c.p$eventtime + ((trt3==1) & (pCR3==1))*s1.a.p$eventtime + ((trt3==1) & (pCR3==0))*s1.a.np$eventtime
  s1_Status = (trt3==0)*s1.c.p$status + ((trt3==1) & (pCR3==1))*s1.a.p$status + ((trt3==1) & (pCR3==0))*s1.a.np$status

  s1 = data.frame(id=id3, Time=s1_Time, Status=s1_Status)
  dat3 <- cbind(s1,treat3)

  #### censored time
  ##C<- runif(tot,0,censu)
  #C<- rexp(tot,censu*lambda0)
  #Status<- as.numeric(t<C)
  #Time<- Status*t+(1-Status)*C
  cen3<- 1-sum(s1$Status)/tot
  ##cat("censor rate=",1-sum(Status)/tot,"\n")


  dat2<- data.frame(id2,id.name2,treat2,pCR)
  colnames(dat2)<- c("id","id.name","Treat","pCR")
  #colnames(dat3)<- c("id","Time","Status","Treat","pCR")

  trt_dat2<- dat2[dat2$Treat==1,]
  con_dat2<- dat2[dat2$Treat==0,]
  p1<- sum(trt_dat2$pCR)/nrow(trt_dat2)
  p0<- sum(con_dat2$pCR)/nrow(con_dat2)
  diff<- p1-p0
  surv.fit<- coxph(Surv(Time,Status)~trt+strata(pCR),data=dat3)
  lghr0<- surv.fit$coefficients
  hr0 = exp(lghr0)
  return(list(dat2=dat2,dat3=dat3,cen3=cen3,lghr0=lghr0,p1=p1,p0=p0,diff=diff))
}





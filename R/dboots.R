#' double bootstrap output
#' 
#' double bootstrap output
#'
#' @param nboot  number of boots
#' @param xdata sub data set
#' @return None
#'
#'
#'
#'
#'
######################################################
boot_out<- function(nboot,xdata=xdata){
  
  sub_dat1<- boot_fun(nboot=nboot,xdata=xdata$dat)
  sub_dat2<- lapply(sub_dat1,boot_fun,nboot=nboot)
  sub_mean<- data.frame(t(sapply(do.call(c,sub_dat2),colMeans)[2:3,]))
  sub_mean<- split(sub_mean,rep(1:nboot,each=nboot))
  
  rho_dis<- sapply(sub_mean,function(x) cor(x$delta,x$lghr))
  rho<- mean(rho_dis)
}





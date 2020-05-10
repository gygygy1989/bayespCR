#' id generation.
#'
#' id generation.
#'
#' @param nn number of subjects
#' @return None
#'
#'
#'

#id function
id.fun<- function(nn){
  a<- do.call(paste0,replicate(5,sample(LETTERS,nn,TRUE),FALSE))
  paste0(a,sprintf("%04d",sample(9999,nn,TRUE)),sample(LETTERS,nn,TRUE))
}

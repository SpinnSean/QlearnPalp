
concat3DMat <- function(A1,A2){
  N <- dim(A1)[1] + dim(A2)[1]
  B <- dim(A1)[2]
  T <- dim(A1)[3]
  A <- array(c(A1,A2), dim= c(N,B,T))
  return(A)
}

create_result_df <- function(fitPath,group,subInd){
  fit <- readRDS(fitPath)
  N <- length(group)
  df <- data.frame(zmean=colMeans(as.data.frame(extract(fit,pars=sprintf("z[%s]",seq(1:N)), inc_warmup = FALSE))),
                   id=subInd,
                   group=group)
  return(df)
}

#' Summary
#'
#' @param object S3 class PLS
#' @param ... other parameters
#'
#' @return summary
#' @export
#'
#' @examples
#' summary(fit_pls(Species~.,iris,n_components=2))

summary.PLS<-function(object,...){
  objet_pls<-object

  Y_dummy<-objet_pls[[1]]$Ydum
  r<-nrow(objet_pls[[1]]$all_coefs_regression)

  coefsall<-data.frame(matrix(ncol=0,nrow=r))
  corall<-objet_pls[[1]]$Matrix_correlation
  r2all<-(objet_pls[[1]]$Matrix_correlation)^2
  Q2all<-objet_pls[[1]]$Q2

  colQ2<-c()
  for (i in 1: ncol(corall)){
    colQ2[i]<-paste("component th",i)
  }

  for (j in 1:ncol(Y_dummy)){
    coefsall<-cbind(coefsall,objet_pls[[j]]$all_coefs_regression)
  }

  for (k in 2:ncol(Y_dummy)){
    corall<-cbind(corall,objet_pls[[k]]$Matrix_correlation)
    r2all<-cbind(r2all,(objet_pls[[k]]$Matrix_correlation)^2)
    Q2all<-rbind(Q2all,objet_pls[[k]]$Q2)
  }

  names(coefsall)<-colnames(Y_dummy)
  Q2all<-as.data.frame(Q2all)
  names(Q2all)<-colQ2

  #cat("Coefficients de la regression pour chaque modalite \n")
  #print(coefsall)
  # cat("Correlation entre les composantes et les variables pour chaque modalite \n")
  # print(corall)
  #cat("R2 entre les composantes et les variables pour chaque modalite \n")
  #print(r2all)

  resultat<-list("Coefs"=coefsall,"Correlation"=corall,"R2"=r2all,"Q2"=Q2all)
  return(resultat)
}

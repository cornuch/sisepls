#' Print PLS
#'
#' @param x is a class S3
#' @param ... other parameters
#'
#' @return print all the matrix Ah, ...
#' @export
#'
#' @examples
#' print(fit_pls(Species~.,iris,n_components=4))

print.PLS<-function(x,...){
  objet_pls<-x

  Y_dummy<-objet_pls[[1]]$Ydum

  Whall<-objet_pls[[1]]$Features_Weights
  Chall<-objet_pls[[1]]$Coefs_yresiduals_comp
  Phall<-objet_pls[[1]]$Coefficients_xresiduals_components
  Whstar<-objet_pls[[1]]$W_star
  Ahall<-objet_pls[[1]]$ah
  RSSall<-objet_pls[[1]]$RSS
  PRESSall<-objet_pls[[1]]$PRESS

  colonne<-c()
  for (i in 1: ncol(Whall)){
    colonne[i]<-paste("component th",i)
  }

  for (j in 2:ncol(Y_dummy)){
    Whall<-cbind(Whall,objet_pls[[j]]$Features_Weights)
    Chall<-rbind(Chall,objet_pls[[j]]$Coefs_yresiduals_comp)
    Phall<-cbind(Phall,objet_pls[[j]]$Coefficients_xresiduals_components)
    Whstar<-cbind(Whstar,objet_pls[[j]]$W_star)
    Ahall<-cbind(Ahall,objet_pls[[j]]$ah)
    RSSall<-rbind(RSSall,objet_pls[[j]]$RSS)
    PRESSall<-rbind(PRESSall,objet_pls[[j]]$PRESS)
  }

  Chall<-as.data.frame(Chall)
  PRESSall<-as.data.frame(PRESSall)
  RSSall<-as.data.frame(RSSall)
  names(Chall)<-colonne
  names(RSSall)<-colonne
  names(PRESSall)<-colonne

  result<-list("RSS"=RSSall,"PRESS"=PRESSall,"Wh"=Whall,"Ch"=Chall,"Ph"=Phall,"Wh*"=Whstar,"Ah"=Ahall)
  return(result)

}


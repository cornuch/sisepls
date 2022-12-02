#' Printing PLS-DA Fits
#'
#' @param x : object of class S3 inheriting from "PLS".
#' @param ... : other parameters.
#'
#' @return print returns a list containing the following components for each level of the categorical variable:
#'
#' data frame of RSS values for models with 1, . . . , n_components.
#'
#' data frame of PRESS values for models with 1, . . . , n_components.
#'
#' data frame of Wh values for models with 1, . . . , n_components.
#'
#' data frame of Ch values for models with 1, . . . , n_components.
#'
#' data frame of Ph values for models with 1, . . . , n_components.
#'
#' data frame of Wh* values for models with 1, . . . , n_components.
#'
#' data frame of Ah values for models with 1, . . . , n_components.
#'
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet<-fit_pls(Species~.,train,n_components=4)
#' print(objet)
#' print(objet)$RSS
#'
#' m<-train_test_splits(iris,0.8)
#' train1<-m$data_train
#' obj<-fit_pls(Species~Petal.Length+Sepal.Width,train1,n_components=2)
#' print(obj)
#' print(objet)$Ah

print.PLS<-function(x,...){
  objet_pls<-x

  Y_dummy<-objet_pls[[1]]$Ydum

  #initialization of matrices
  Whall<-objet_pls[[1]]$Features_Weights
  Chall<-as.data.frame(objet_pls[[1]]$Coefs_yresiduals_comp)
  Phall<-objet_pls[[1]]$Coefficients_xresiduals_components
  Whstar<-objet_pls[[1]]$W_star
  Ahall<-objet_pls[[1]]$ah
  RSSall<-as.data.frame(objet_pls[[1]]$RSS)
  PRESSall<-as.data.frame(objet_pls[[1]]$PRESS)

  #Names of columns for Ch, RSS and PRESS data frame
  colonne<-c()
  for (i in 1: ncol(Whall)){
    colonne[i]<-paste("component th",i)
  }

  #Aggregating the results
  for (j in 2:ncol(Y_dummy)){
    Whall<-cbind(Whall,objet_pls[[j]]$Features_Weights)
    Chall<-rbind(Chall,as.data.frame(objet_pls[[j]]$Coefs_yresiduals_comp))
    Phall<-cbind(Phall,objet_pls[[j]]$Coefficients_xresiduals_components)
    Whstar<-cbind(Whstar,objet_pls[[j]]$W_star)
    Ahall<-cbind(Ahall,objet_pls[[j]]$ah)
    RSSall<-rbind(RSSall,as.data.frame(objet_pls[[j]]$RSS))
    PRESSall<-rbind(PRESSall,as.data.frame(objet_pls[[j]]$PRESS))
  }

  #Give a columns name
  names(Chall)<-colonne
  names(RSSall)<-colonne
  names(PRESSall)<-colonne

  #list of the results given by the print function
  result<-list("RSS"=RSSall,"PRESS"=PRESSall,"Wh"=Whall,"Ch"=Chall,"Ph"=Phall,"Wh*"=Whstar,"Ah"=Ahall)
  return(result)

}


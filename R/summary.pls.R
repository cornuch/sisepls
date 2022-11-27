#' Summarizing PLS-DA fits
#'
#' summary method for class "PLS".
#'
#' @param x : object of class S3 inheriting from "PLS".
#' @param ... : other parameters.
#'
#' @return summary returns a list containing the following components for each level of the categorical variable:
#'
#' all coefficients and constant of the PLS Regression for models with 1,...,n_components.
#'
#' data frame of correlation between each component and all the variables (explanatory and target)
#'
#' data frame of correlation squared between each component and all the variables (explanatory and target)
#'
#' data frame of Q2 values for models with 1, . . . , n_components.
#'
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet<-fit_pls(Species~.,train,n_components=4)
#' summary(objet)
#' summary(objet)$R2
#'
#' m<-train_test_splits(iris,0.8)
#' train1<-m$data_train
#' obj<-fit_pls(Species~Petal.Length+Sepal.Width,train1,n_components=2)
#' summary(obj)
#' summary(objet)$Coefs

summary.PLS<-function(x, ...){
  objet_pls<-x

  Y_dummy<-objet_pls[[1]]$Ydum
  r<-nrow(objet_pls[[1]]$all_coefs_regression)

  coefsall<-data.frame(matrix(ncol=0,nrow=r))
  corall<-objet_pls[[1]]$Matrix_correlation
  r2all<-(objet_pls[[1]]$Matrix_correlation)^2
  Q2all<-as.data.frame(objet_pls[[1]]$Q2)

  #Names of columns for Q2 data frame
  colQ2<-c()
  for (i in 1: ncol(corall)){
    colQ2[i]<-paste("component th",i)
  }

  #Aggregating all the coefficients of the linear regressions of y on x in one matrix
  for (j in 1:ncol(Y_dummy)){
    coefsall<-cbind(coefsall,objet_pls[[j]]$all_coefs_regression)
  }

  #Aggregating all the Correlation coefficients, the R^2 and the Q^2 coefficients
  for (k in 2:ncol(Y_dummy)){
    corall<-cbind(corall,objet_pls[[k]]$Matrix_correlation)
    r2all<-cbind(r2all,(objet_pls[[k]]$Matrix_correlation)^2)
    Q2all<-rbind(Q2all,as.data.frame(objet_pls[[k]]$Q2))
  }

  #name of the columns for the regression coefficients data frame
  names(coefsall)<-colnames(Y_dummy)

  #Tansform the matrix in a data frame
  #Q2all<-as.data.frame(Q2all)
  #Names of the columns of this data frame
  names(Q2all)<-colQ2

  #list of the results given by the summary function
  resultat<-list("Coefs"=coefsall,"Correlation"=corall,"R2"=r2all,"Q2"=Q2all)
  return(resultat)
}

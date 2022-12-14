#' Features weights
#'
#' Plots the weights of each features for the first and second component
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#' @param i : integer, default=1. It is the index of the outcome to be selected.
#'
#' @importFrom graphics abline arrows symbols text par legend segments
#'
#' @return a scatter plot of the weights of each features for the first and second component for a selected outcome.
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet<-fit_pls(Species~.,train,n_components=4)
#' plot_explanatories_weights(objet,i=2)

plot_explanatories_weights<-function(objet_pls,i=1){

  #Recovery of the Y dummy matrix and the explanatory variables
  X<-objet_pls[[1]]$X
  Y_dummy<-objet_pls[[1]]$Ydum

  #Check if i is appropriate (i<= number of outcomes)
  if (i>ncol(Y_dummy)){
   stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  #Map of the weights on the two first components
  ah1<-as.matrix(objet_pls[[i]]$ah[1])
  ah2<-as.matrix(objet_pls[[i]]$ah[2])
  plot(ah1,ah2,xlim=c(min(ah1),max(ah1)),ylim=c(min(ah2),max(ah2)),type="n",xlab="ah[1]",ylab="ah[2]",main=paste("Map of variables weights \n for the outcome",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i],"versus rest"))
  abline(h=0,v=0)
  text(ah1,ah2,colnames(X),cex=0.5)

}

#' Circle of correlation
#'
#' Plots the circle of correlation
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#' @param i : integer, default=1. It is the index of the outcome to be selected.
#'
#' @importFrom graphics abline arrows symbols text par legend segments
#'
#' @return the graph of the circle of correlation for all the features.
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet<-fit_pls(Species~.,train,n_components=4)
#' plot_circle_correlation(objet)

plot_circle_correlation<-function(objet_pls,i=1){

  #Recovery of the explanatory variables
  X<-objet_pls[[1]]$X
  Y_dummy<-objet_pls[[1]]$Ydum
  x1<-as.matrix(objet_pls[[i]]$Matrix_correlation[1][1:nrow(objet_pls[[1]]$Matrix_correlation)-1,])
  y1<-as.matrix(objet_pls[[i]]$Matrix_correlation[2][1:nrow(objet_pls[[1]]$Matrix_correlation)-1,])

  #Check if i is appropriate (i<= number of outcomes)
  if (i>ncol(Y_dummy)){
    stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

 #Map of variables correlation on the two first components
  plot(x1,y1,xlim=c(-3,+3),ylim=c(-3,3),xlab="th1 component",ylab="th2 component",main="Correlation map of explanatories \n for the two first components",type="n")

  #x=0 and y=0
  abline(h=0,v=0)

  #cex=0.8 in order to reduce the size of the text
  text(x1,y1,colnames(X),cex=0.8)

  #Circle centered in (0,0) and R=1
  symbols(0,0,circles=1,inches=F,add=T)

  #Putting the arrows
  arrows(0,0,x1-0.1,y1-0.1,angle=10,col="blue")

  }

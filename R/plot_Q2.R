#' Q2 plot
#'
#' Plots the Q2 values for all the outcomes and the number of components selected previously.
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#'
#' @importFrom graphics abline arrows symbols text par legend
#' @importFrom grDevices rainbow
#'
#' @return the graph of the Q2 values for all the strategies one versus all.
#'
#' A threshold of 0.0975 is drawn in red on the graph.
#'
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet<-fit_pls(Species~.,train,n_components=4)
#' plot_Q2(objet)

plot_Q2<-function(objet_pls){

  #Recovery of the Y dummy matrix
  Y_dummy<-objet_pls[[1]]$Ydum
  #colors for the graph
  couleur<-rainbow(ncol(Y_dummy))

  #number of components
  xmax<-objet_pls[[1]]$n_components

  #Create data frame with x=1...n_components
  #and y = Q2 values for each "component"
  dq2<-cbind(data.frame(c(1:xmax)),t(objet_pls[[1]]$Q2))
  names(dq2)<-c("n","Q2")

  #plot of Q2 on all components for the first outcome versus all
  plot(dq2$n,dq2$Q2,type="b",xlim=c(0,xmax),ylim=c(0,1),col="black",xlab="Number of components",ylab="",main="Q2")
  abline(h=0.0975,col="red")

  #Same method for all the other outcomes versus all
  if (ncol(Y_dummy)>=2){
    for (i in 2:ncol(Y_dummy)){
      dq2<-cbind(data.frame(c(1:xmax)),t(objet_pls[[i]]$Q2))
      names(dq2)<-c("n","Q2")
      par(new=TRUE)
      plot(dq2$n,dq2$Q2,type="b",xlim=c(0,xmax),ylim=c(0,1),xlab="",ylab="",col=couleur[i])
    }
  }
}

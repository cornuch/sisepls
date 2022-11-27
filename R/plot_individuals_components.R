#' Maps of individuals according to the first two components
#'
#' Plots the individuals in relation with the first two components.
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#' @param i : integer, default=1. It is the index of the outcome to be selected.
#'
#' @importFrom graphics abline symbols text par legend
#'
#' @return a scatter plot of the individuals according to the first two components.
#' @export
#'
#' @examples
#' plot_individuals_components(fit_pls(Species~.,iris,n_components=4),i=2)

plot_individuals_components<-function(objet_pls,i=1){

  par(xpd=TRUE, mar=c(4,4,4,4))

  #Recovery of the Y dummy matrix and the target variable
  Y_dummy<-objet_pls[[1]]$Ydum
  Y<-objet_pls[[1]]$Y_target

  #Check if i is appropriate (i<= number of outcomes)
  if (i>ncol(Y_dummy)){
    stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  #Create a data frame containing the components and the target variable
  dth<-cbind(objet_pls[[i]]$Components,Y)

  #Map of individuals on the first two components
  plot(dth$V1, dth$V2, col=dth$Y, xlim=c(min(dth$V1)-1,max(dth$V1)),xlab="th1 component",ylab="th2 component",main=paste("Maps of individuals on the first two components \n for the outcome",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i],"versus rest"),pch=21)
  #abline(h=0,v=0)
  segments(x0=min(dth$V1)-1.2,x1=max(dth$V1)+0.2,y0=0,y1=0)
  segments(x0=0,x1=0,y0=min(dth$V2)-0.2,y1=max(dth$V2)+0.2)
  #legend for the plot
  legend("topleft",legend=unique(dth$Y),col=1:length(dth$Y),pch=21,cex=0.8,bty="n")
  #text(dth$V1,dth$V2,rownames(dth),cex=0.5)
  }

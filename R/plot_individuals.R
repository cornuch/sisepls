#' Maps of individuals
#'
#' Plots the individuals in relation with one component and u1. The details of u1 are given under ‘Details’.
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#' @param i : integer, default=1. It is the index of the outcome to be selected.
#' @param j : integer, default=1. It is the index of the components to be selected.
#'
#' @importFrom graphics abline symbols text par legend
#'
#' @return plot
#' @export
#'
#' @examples
#' plot_individuals(fit_pls(Species~.,iris,n_components=4),i=2,j=1)
#'
#' @details
#' Regression of y on the first component th1.
#' y=c1*th1+y1
#'
#' u1 is the normalization of y : u1=y/c1
#'
#' It allows us to obtain the first bisector as the least squares line of u1 on th1.
#'

plot_individuals<-function(objet_pls,i=1,j=1){

  par(xpd=TRUE, mar=c(4,4,4,4))

  #Recovery of the Y dummy matrix and the target variable
  Y_dummy<-objet_pls[[1]]$Ydum
  Y<-objet_pls[[1]]$Y_target

  #Check if i is appropriate (i<= number of outcomes)
  if (i>ncol(Y_dummy)){
    stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  #Check if j is appropriate (j<= number of components)
  if (j>objet_pls[[1]]$n_components){
    stop("You have to choose an integer between 1 and the number of components")
  }

  #Map of individuals on the components chosen and u1
  dthu<-cbind(objet_pls[[i]]$Components[j],objet_pls[[i]]$u1)

  #Create a data frame containing the component, the values of u1 and the target variable
  df<-cbind(dthu,Y)
  names(df)<-c("th","u1","Y")

  #liaison entre première composante t1 et u1
  plot(df$th, df$u1, col=df$Y,xlab=paste("component th",j),ylab="u1",main=paste("Maps of individuals considering \n component u1 and th",j,"\n for the outcome",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i],"versus rest"))
  #horizontal and vertical segments (abline(v=0,h=0))
  segments(x0=min(df$th)-0.2,x1=max(df$th)+0.2,y0=0,y1=0)
  segments(x0=0,x1=0,y0=min(df$u1)-0.2,y1=max(df$u1)+0.2)
  #legend for the plot
  legend("topleft",legend=unique(df$Y),col=1:length(df$Y),pch=21,cex=0.8,bty="n")

 }

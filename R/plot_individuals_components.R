#' Maps individuals components
#'
#' @param objet_pls is a class S3
#' @param i is an integer
#'
#' @importFrom graphics abline symbols text par legend
#'
#' @return plot
#' @export
#'
#' @examples
#' plot_individuals_components(fit_pls(Species~.,iris,n_components=4),i=2)

plot_individuals_components<-function(objet_pls,i=1){

  par(xpd=TRUE, mar=c(4,4,4,4))

  Y_dummy<-objet_pls[[1]]$Ydum
  Y<-objet_pls[[1]]$Y_target

  if (i>ncol(Y_dummy)){
    stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  #Carte des individus sur les deux premières composantes PLS
  dth<-cbind(objet_pls[[i]]$Components,Y)
  plot(dth$V1, dth$V2, col=dth$Y, xlim=c(min(dth$V1)-1,max(dth$V1)),xlab="th1 component",ylab="th2 component",main=paste("Maps of individuals for the first two components \n for the outcome",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i],"versus rest"),pch=21)
  #text(dth$V1,dth$V2,rownames(dth),cex=0.5)
  legend("topleft",legend=unique(dth$Y),col=1:length(dth$Y),pch=21,cex=0.8,bty="n")
}

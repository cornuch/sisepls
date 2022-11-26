#' PLot correlation
#'
#' @param objet_pls if a class S3
#' @param i is an integer between 1 and the number of outcoms for the target variable
#'
#' @importFrom graphics abline arrows symbols text par legend segments
#'
#' @return plot
#' @export
#'
#' @examples
#' plot_explanatories_weights(fit_pls(Species~.,iris,n_components=4),i=2)

plot_explanatories_weights<-function(objet_pls,i=1){

  X<-objet_pls[[1]]$X
  Y_dummy<-objet_pls[[1]]$Ydum

  if (i>ncol(Y_dummy)){
   stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  #Carte des poids
  #Pour chaque modalité
  wsh1<-as.matrix(objet_pls[[i]]$W_star[1])
  wsh2<-as.matrix(objet_pls[[i]]$W_star[2])
  plot(wsh1,wsh2,xlim=c(min(wsh1),max(wsh1)),ylim=c(min(wsh2),max(wsh2)),type="n",xlab="Wh1",ylab="Wh2",main=paste("Carte des poids des variables \n pour la modalite",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i]))
  segments(x0 = min(wsh1)-0.05, x1 =max(wsh1)+0.05, y0 = 0, y1 = 0, lwd = 1, col = "black")
  segments(x0 =0, x1 =0 , y0 = min(wsh2)-0.05, y1 = max(wsh2)+0.05, lwd = 1, col = "black")
  #cex=0.5 pour réduire la taille du texte
  text(wsh1,wsh2,colnames(X),cex=0.5)

}

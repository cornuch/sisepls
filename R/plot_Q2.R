#' PLot Q2
#'
#' @param objet_pls is a class S3
#'
#' @importFrom graphics abline arrows symbols text par legend
#' @importFrom grDevices rainbow
#'
#' @return a plot
#' @export
#'
#' @examples
#' plot_Q2(fit_pls(Species~.,iris,n_components=4))

plot_Q2<-function(objet_pls){
  Y_dummy<-objet_pls[[1]]$Ydum
  couleur<-rainbow(ncol(Y_dummy))

  #Q2 selon les modalités
  xmax<-objet_pls[[1]]$n_components

  dq2<-cbind(data.frame(c(1:xmax)),t(objet_pls[[1]]$Q2))
  names(dq2)<-c("n","Q2")
  plot(dq2$n,dq2$Q2,type="b",xlim=c(0,xmax),ylim=c(0,1),col="black",xlab="Nombre de facteurs",ylab="",main="Q2")
  abline(h=0.0975,col="red")

  if (ncol(Y_dummy)>=2){
    for (i in 2:ncol(Y_dummy)){
      dq2<-cbind(data.frame(c(1:xmax)),t(objet_pls[[i]]$Q2))
      names(dq2)<-c("n","Q2")
      par(new=TRUE)
      plot(dq2$n,dq2$Q2,type="b",xlim=c(0,xmax),ylim=c(0,1),xlab="",ylab="",col=couleur[i])
    }
  }

}

#' Plotting the circle of correlation for explanatories variables
#'
#' @param objet_pls is a class S3
#'
#' @importFrom graphics abline arrows symbols text par legend segments
#'
#' @return the plot
#' @export
#'
#' @examples
#' plot_circle_correlation(fit_pls(Species~.,iris,n_components=4))

plot_circle_correlation<-function(objet_pls){

  X<-objet_pls[[1]]$X
  x1<-as.matrix(objet_pls[[1]]$Matrix_correlation[1][1:nrow(objet_pls[[1]]$Matrix_correlation)-1,])
  y1<-as.matrix(objet_pls[[1]]$Matrix_correlation[2][1:nrow(objet_pls[[1]]$Matrix_correlation)-1,])

 #Carte des corrélations variables et deux premières composantes
  plot(x1,y1,xlim=c(-3,+3),ylim=c(-3,3),xlab="th1 component",ylab="th2 component",main="Correlation map of explanatories \n for the two first components",type="n")
  #abline(h=0,v=0)
  segments(x0 = -3.2, x1 =3.2, y0 = 0, y1 = 0, lwd = 1, col = "black")
  segments(x0 =0, x1 =0 , y0 = -3.2, y1 = 3.2, lwd = 1, col = "black")
  #cex=0.5 pour réduire la taille du texte
  text(x1,y1,colnames(X),cex=0.8)
  symbols(0,0,circles=1,inches=F,add=T)
  #mettre les fleches
  arrows(0,0,x1-0.1,y1-0.1,angle=10,col="blue")

  }

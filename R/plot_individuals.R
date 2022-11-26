#' plot
#'
#' @param objet_pls is class S3
#' @param i integer
#' @param j integer
#'
#' @importFrom graphics abline symbols text par legend
#'
#' @return plot
#' @export
#'
#' @examples
#' plot_individuals(fit_pls(Species~.,iris,n_components=4),i=2,j=1)

plot_individuals<-function(objet_pls,i=1,j=1){

  par(xpd=TRUE, mar=c(4,4,4,4))

  Y_dummy<-objet_pls[[1]]$Ydum
  Y<-objet_pls[[1]]$Y_target

  if (i>ncol(Y_dummy)){
    stop("You have to choose an integer between 1 and the number of outcomes for the target variable")
  }

  if (j>objet_pls[[1]]$n_components){
    stop("You have to choose an integer between 1 and the number of components")
  }

  dthu<-cbind(objet_pls[[i]]$Components[j],objet_pls[[i]]$u1)
  df<-cbind(dthu,Y)
  names(df)<-c("th","u1","Y")
  #liaison entre première composante t1 et u1
  plot(df$th, df$u1, col=df$Y,xlab=paste("component th",j),ylab="component u1",main=paste("Maps of individuals considering \n component u1 and th",j,"\n for the outcome",colnames(as.data.frame(objet_pls[[1]]$Ydum))[i],"versus rest"))
  legend("topleft",legend=unique(df$Y),col=1:length(df$Y),pch=21,cex=0.8,bty="n")

 }

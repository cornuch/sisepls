#' Scale data
#'
#' Standardize each column of a data set.
#'
#' @param X : matrix or data frame.
#' @param reduce : bool, default = TRUE.
#' If True, scale the data to unit standard deviation.
#'
#' @importFrom stats sd
#'
#' @return a matrix-array of the transformed data.
#' @export
#'
#' @examples
#' xscale(iris[,1:4])
#' xscale(iris[,1:4],reduce=FALSE)

xscale<-function(X,reduce=TRUE){

  #Transform X in a matrix class
  X<-as.matrix(X)

  #Check if the type of all columns are "double"
  if(typeof(X)!="double"){
    stop("Not expected character's matrix")
  }

  #Compute the coefficients of the matrix in substracting the mean
  #corresponding to the column where the coefficient is situated
  X_scale<-apply(X,2,function(x){return(x-mean(x))})

  #Reduce the coefficients of the matrix if reduce is TRUE
  if(reduce==TRUE){
    #check if some columns have a standard deviation equal to 0
    stdev<-apply(X,2,sd)
    for (i in 1:ncol(X)){
      if (stdev[i]==0){
        print("Becarful : a minimum of one column as a standard deviation of 0")
      }else{
        #Compute the coefficients of the matrix in dividing the standard
        #corresponding to the column where the coefficient is situated
        X_scale <- apply(X_scale,2,function(x){return(x/sd(x))})
      }
    }
  }
  return(X_scale)
}

#' Dummy for a categorial variable
#'
#' Convert a categorical variable into dummy/indicator variables.
#'
#' @param Y : a factor. Data of which to get dummy indicators.
#'
#' @return Dummy-coded matrix-array.
#' @export
#'
#' @examples
#' dummy(iris[,5])

dummy<-function(Y){

  #Check if the target variable is a factor
  if (is.factor(Y)==FALSE){
    stop("The variable must be a factor")
  }else{
  #transform the column containing the target variable into a dummy matrix
  Y_dummy<-as.matrix(sapply(levels(Y),function(x){ifelse(Y==x,1,0)}))
  return(Y_dummy)
  }
}

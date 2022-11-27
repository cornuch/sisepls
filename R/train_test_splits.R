#' Train and test split data
#'
#' Split a data frame into random train and test subsets.
#'
#' @param data : data frame.
#' @param train_size : float, default=0.6.
#' Should be a float between 0.0 and 1.0 and represent the proportion of the data frame to include in the test split.
#'
#' @return List of data frame containing train-test split of inputs ($data_train and $data_test).
#' @export
#'
#' @examples
#' train_test_splits(mtcars)
#' d<-train_test_splits(iris,0.8)
#' train<-d$data_train
#' test<-d$data_test
#' train
#' test

train_test_splits<-function(data,train_size=0.6){

  #check if train_size in ]0;1[
  if(train_size>1 || train_size<0){
    stop("The train size must be a float between 0.0 and 1.0")
  }

  #check the consistency of the class
  if(!is.data.frame(data)){
    stop("The data source must be a data frame")
  }

  #number of rows in the data
  n <- nrow(data)

  #select the indices for individuals in the training sample
  i_sample<-sample(1:n,trunc(n*train_size))

  #Output list
  result_split<-list("data_train"=data[i_sample,],
                     "data_test"=data[setdiff(1:n,i_sample),])
  return(result_split)
}

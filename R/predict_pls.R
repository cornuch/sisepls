#' Predict
#'
#' Predicts the class of an individuals given the results of the PLS-DA model fitting function.
#'
#' @param objet_pls : object of class S3 inheriting from "PLS".
#' @param newdata : data frame. The data test source containing all the variables specified in the formula. 'Na's are not allowed.
#' @param type : integer equal to 1 or 2. 1 to predict the class and 2 in order to add the estimation of the class membership probabilities.
#'
#' @importFrom stats formula as.formula model.frame
#'
#' @return a list of predicted class and the estimation of the class membership probabilities if type=2.
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.8)
#' train<-d$data_train
#' test<-d$data_test
#' res<-fit_pls(Species~.,train,n_components=3)
#' predict_pls(res,test,2)
#'
#' @note The softmax function is used for the estimation of the class membership probabilities if type=2.
#'
#' The argmax is used to predict the class.

predict_pls<-function(objet_pls,newdata,type=1){
  #1 for predict the outcome
  #2 for scoring (probability) the outcome
  #one versus all

  # Check if required parameters are specified
  if(missing(objet_pls) || missing(newdata)){
    stop("predict_pls required objet=fit_pls(), newdata")
  }

  #check the consistency of the class
  if(!is.data.frame(newdata)){
    stop("The data source must be a data frame")
  }

  # Check if type corresponds to the format required
  if(type!=1 & type!=2){
    stop("type must be equal to 1 ou 2")
  }

  #Recovery of the formula
  formule<-objet_pls[[1]]$formula

  #Reconstitute a data frame from a formula
  df<-model.frame(formule, data = newdata)

  #Select the target variable
  Y<-df[,1]

  #Check if there is a minimum of explanatory variables
  if (ncol(df)==2){
    stop("You need minimum of two explanatory variables")
  }else{
    X<-df[,-1]
  }

  #Transform the categorical target variable in a dummy matrix
  Y_dummy<-ydummy(Y)
  X<-as.matrix(X)

  #Create the names of columns for the data frame predicts
  colonnes<-c(colnames(Y_dummy),"predict")
  #initialization of predict matrix
  Y_predict<-matrix(0,nrow(X),nlevels(Y)+1)

  for (i in (1:nlevels(Y))){
    #compute the predicted y with the coefficients of the fitted model : outcome levels "i" versus rest
    y_predict<-X%*%objet_pls[[i]]$Coefs_regression+objet_pls[[i]]$Constant_regression
    Y_predict[,i]<-y_predict
  }
  #Matrix of predictions
  Y_pred<-Y_predict[,1:nlevels(Y)]

  #data frame of predictions
  Y_predict<-as.data.frame(Y_predict,row.names = rownames(X))
  names(Y_predict)<-colonnes
  for (j in (1:nrow(X))){
    Y_predict[j,4]<-names(Y_predict)[which.max(Y_predict[j,])]
  }

  #softmax for probability score prediction
  Y_predict_proba<-t(apply(Y_pred,1,function(x){return(exp(x)/sum(exp(x)))}))
  #data frame with predict outcome + probability score predicted
  Y_predict_proba<-as.data.frame(Y_predict_proba,row.names = rownames(X))
  names(Y_predict_proba)<-colnames(Y_dummy)
  Y_predict_final<-cbind(Y_predict,Y_predict_proba)

  #using type to know what type of results are shown
  if (type==1){
    Y_result<-Y_predict
  }else{
    Y_result<-Y_predict_final
  }

  #list of the results given by the predict_pls function
  results_predict<-list("len"=nrow(X),"Y_test"=Y,"Y_predict"=Y_result)
  return(results_predict)
}

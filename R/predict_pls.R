#' predictions
#'
#' @param objet_pls is class S3
#' @param newdata is a data frame
#' @param type 1 pour ou 2
#' @importFrom stats formula as.formula model.frame
#'
#' @return predictions
#' @export
#'
#' @examples
#' d<-train_test_split(iris,0.8)
#' train<-d$data_train
#' test<-d$data_test
#' res<-fit_pls(Species~.,train,3)
#' predict_pls(res,test,2)

predict_pls<-function(objet_pls,newdata,type=1){
  #1 for predict the outcome
  #2 for scoring (probability) the outcome
  #one versus all

  # Check if required parameters are specified
  if(missing(objet_pls) || missing(newdata) || missing(type)){
    stop("plsda.predict required objet=fit(), newdata and type")
  }

  #Verifier si meme target et meme features ?

  formule<-objet_pls[[1]]$formula

  #Reconstitute a data frame from a formula
  df<-model.frame(formule, data = newdata)

  Y<-df[,1]
  if (ncol(df)==2){
    stop("You need minimum of two explanatory variables")
  }else{
    X<-df[,-1]
  }

  #Transform the categorial target variable in a dummy matrix
  Y_dummy<-dummy(Y)
  X<-as.matrix(X)

  colonnes<-c(colnames(Y_dummy),"predict")
  Y_predict<-matrix(0,nrow(X),nlevels(Y)+1)

  for (i in (1:nlevels(Y))){
    y_predict<-X%*%objet_pls[[i]]$Coefs_regression+objet_pls[[i]]$Constant_regression
    Y_predict[,i]<-y_predict
  }
  Y_pred<-Y_predict[,1:nlevels(Y)]
  Y_predict<-as.data.frame(Y_predict,row.names = rownames(X))
  names(Y_predict)<-colonnes
  for (j in (1:nrow(X))){
    Y_predict[j,4]<-names(Y_predict)[which.max(Y_predict[j,])]
  }
  #softmax for probability score prediction
  Y_predict_proba<-t(apply(Y_pred,1,function(x){return(exp(x)/sum(exp(x)))}))
  Y_predict_proba<-as.data.frame(Y_predict_proba,row.names = rownames(X))
  names(Y_predict_proba)<-colnames(Y_dummy)
  Y_predict_final<-cbind(Y_predict,Y_predict_proba)

  if (type==1){
    Y_result<-Y_predict
  }else{
    Y_result<-Y_predict_final
  }

  results_predict<-list("len"=nrow(X),"Y_test"=Y,"predicted_outcomes"=Y_result)
  return(results_predict)
}

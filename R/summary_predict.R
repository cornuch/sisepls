#' Predict quality
#'
#' Compute the confusion matrix and a performance indicator.
#'
#' @param pls_predict : list. Is the list returned with the predict_pls function.
#' @param beta : float : used only for the binary case (Y target as two outcomes).
#' Determines the weight of recall in the combined score.
#'
#' @return confusion matrix and the Mattheus Correlation Coefficient (MCC).
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.8)
#' train<-d$data_train
#' test<-d$data_test
#' res<-fit_pls(Species~.,train,n_components=3)
#' pred<-predict_pls(res,test,2)
#' summary_predict(pred)
#'
#' @note Becareful, for the binary case (the target variable has only two outcomes),
#' please check that your "positive" outcome is situated in the first level of your factor
#'
#' @references
#' GRANDINI M., BAGLI E., VISANI G., "METRICS FOR MULTI-CLASS CLASSIFICATION: AN OVERVIEW"

summary_predict<-function(pls_predict,beta=2){


  #Computation for a target variable with two outcomes
  if (nlevels(pls_predict$Y_test)==2){

    print("Be careful, you postive outcome must be placed first on the levels of your target variable")

    #Initialization
    M<-matrix(0,2,2)
    #M[1,1] : True Positive elements
    #M[1,2] : False Negative elements
    #M[2,1] : False Positive elements
    #M[2,2] : True Negative elements

    for (k in 1:pls_predict$len){
      #fullfilled the confusion matrix
      for (i in 1:nlevels(pls_predict$Y_test)){
        for (j in 1:nlevels(pls_predict$Y_test)){
          if((((pls_predict$Y_test[k]==levels(pls_predict$Y_test)[i])==TRUE) & ((pls_predict$Y_predict[[4]][k]==levels(pls_predict$Y_test)[j])==TRUE))==TRUE){
            M[i,j]<-M[i,j]+1
          }
        }
      }
    }

    #renaming
    TP<-M[1,1]
    FN<-M[1,2]
    FP<-M[2,1]
    TN<-M[2,2]

    #c : the total number of elements correctly predicted
    c<-sum(diag(M))
    #sum of columns
    p<-apply(M,2,sum)
    #sum of rows
    t<-apply(M,1,sum)

    #Construct the data frame corresponding to the confusion matrix
    col<-paste("predict",sep=" ",levels(pls_predict$Y_test))
    conf_mat <- as.data.frame(M,row.names=c(levels(pls_predict$Y_test)[1],levels(pls_predict$Y_test)[2]))
    names(conf_mat)<-col

    #Error rate
    err<-1-c/pls_predict$len
    #Precision
    precision_m<-TP/(TP+FP)
    #Recall
    recall_m<-TP/(TP+FN)
    #F1-score
    f1score<-beta/((1/precision_m)+(1/recall_m))

    #Mattheus Correlation Coefficient
    MCC<-(TP*TN-FP*FN)/sqrt((TP+FN)*(TP+FP)*(TN+FN)*(TN+FP))

    cat("Confusion Matrix")
    print(conf_mat)

    cat("Mattheus Correlation Coefficient for Multi-class Classification : ", MCC,"\n")

    cat("Precision : ", precision_m,"\n")

    cat("Recall : ", recall_m,"\n")

    cat("F1-score : ", f1score,"\n")

    cat("rate error : ", err,"\n")

  }else{
    #initialization of confusion matrix
    M<-matrix(0,nlevels(pls_predict$Y_test),nlevels(pls_predict$Y_test))
    #For all elements in the data test set
    for (k in 1:pls_predict$len){
      #fullfilled the confusion matrix
      for (i in 1:nlevels(pls_predict$Y_test)){
        for (j in 1:nlevels(pls_predict$Y_test)){
          if((((pls_predict$Y_test[k]==levels(pls_predict$Y_test)[i])==TRUE) & ((pls_predict$Y_predict[[nlevels(pls_predict$Y_test)+1]][k]==levels(pls_predict$Y_test)[j])==TRUE))==TRUE){
            M[i,j]<-M[i,j]+1
          }
        }
      }
    }

    col<-paste("predict",sep=" ",levels(pls_predict$Y_test))
    conf_mat <- as.data.frame(M,row.names=levels(pls_predict$Y_test))
    names(conf_mat)<-col

    #the total number of elements
    s<-sum(apply(M,2,sum))
    #c : the total number of elements correctly predicted
    c<-sum(diag(M))
    p<-apply(M,2,sum)
    t<-apply(M,1,sum)
    pkn<-0
    p2<-0
    t2<-0
    for (k in 1: nlevels(pls_predict$Y_test)){
      #the number of times the class k was predicted (column total)
      pk<-p[k]
      #the number of times the class k was truly predicted (row total)
      tk<-t[k]
      pkn<-pkn+pk*tk
      p2<-p2+pk*pk
      t2<-t2+tk*tk
    }
    MCC<-(c*s-pkn)/sqrt((s*s-p2)*(s*s-t2))

    cat("Confusion Matrix")
    print(conf_mat)

    cat("Mattheus Correlation Coefficient for Multi-class Classification : ",MCC,"\n")
  }
}

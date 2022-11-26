#' summary pls predict
#'
#' @param pls_predict is an object
#'
#' @return confusion matrix
#' @export
#'
#' @examples
#' d<-train_test_split(iris,0.8)
#' train<-d$data_train
#' test<-d$data_test
#' res<-fit_pls(Species~.,train,3)
#' pred<-predict_pls(res,test,2)
#' summary_predict(pred)

summary_predict<-function(pls_predict){
  if (nlevels(pls_predict$Y_test)==2){
    TP<-0
    FN<-0
    FP<-0
    TN<-0

    for (k in 1:pls_predict$len){
      if (pls_predict$Y_test[k]==levels(pls_predict$Y_test)[1]){
        if (pls_predict$Y_predict[k]==levels(pls_predict$Y_test)[1]){
          TP<-TP+1
        }else{
          FN<-FN+1
        }
      }
      if (pls_predict$Y_test[k]==levels(pls_predict$Y_test)[2]){
        if (pls_predict$Y_predict[k]==levels(pls_predict$Y_test)[2]){
          FP<-FP+1
        }else{
          TN<-TN+1
        }
      }
    }
    M<-matrix(0,2,2)
    M[1,1]<-TP
    M[1,2]<-FN
    M[2,1]<-FP
    M[2,2]<-TN
    col<-paste("predict",sep=" ",levels(pls_predict$Y_test))
    conf_mat <- as.data.frame(M,row.names=c(levels(pls_predict$Y_test)[1],levels(pls_predict$Y_test)[2]))
    names(conf_mat)<-col
    MCC<-(TP*TN-FP*FN)/sqrt((TP+FN)*(TP+FP)*(TN+FN)*(TN+FP))

    cat("Confusion Matrix")
    print(conf_mat)

    cat("Mattheus Correlation Coefficient for Multi-class Classification : ", MCC,"\n")

  }else{
    M<-matrix(0,nlevels(pls_predict$Y_test),nlevels(pls_predict$Y_test))
    for (k in 1:pls_predict$len){
      for (i in 1:nlevels(pls_predict$Y_test)){
        for (j in 1:nlevels(pls_predict$Y_test)){
          if((((pls_predict$Y_test[k]==levels(pls_predict$Y_test)[i])==TRUE) & ((pls_predict$predicted_outcomes[[4]][k]==levels(pls_predict$Y_test)[j])==TRUE))==TRUE){
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

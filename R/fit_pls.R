#' Partial Least Square Discriminant Analysis Regression
#'
#' Fit a PLS-DA Regression.
#' The PLS-DA is a statistical method used to find the relation between a set of explanatory quantitative variables and a categorical target variable.
#'
#' @param formule : object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data : data frame. The data train source containing all the variables specified in the formula. 'Na's are not allowed.
#' @param n_components : integer, default=2. Number of components to keep. Should be between 1 and the max(n_features).
#'
#' @importFrom stats formula as.formula model.frame cor
#'
#' @return object of class "PLS" is a list containing the following components for each level of the categorical variable:
#'
#' formula : object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#'
#' X : explanatory variables (a set of continuous variables).
#'
#' Y : target variable (a discrete variable).
#'
#' Ydum : dummy-coded matrix linked to the target variable.
#'
#' n_components : number of components included in the model.
#'
#' Components : data frame containing the components "th" values for models with 1,...,n_components. Each row corresponds to one individual.
#'
#' Features_Weights : data frame containing the weights "wh" values for models with 1,...,n_components. Each row corresponds to one feature.
#'
#' Coefs_yresiduals_comp : matrix of coefficients "ch" values for models with 1,...,n_components.
#'
#' Coefficients_xresiduals_components : data frame containing the "ph" values for models with 1,...,n_components. Each row corresponds to one feature.
#'
#' y_residuals : data frame containing the "yh" residuals values for models with 1,...,n_components. Each row corresponds to one individual.
#'
#' W_star : data frame containing the weights "w*h" values for models with 1,...,n_components. Each row corresponds to one feature.
#'
#' ah : data frame containing the "ah" coefficients for the regression for models with 1,...,n_components. Each row corresponds to one feature.
#'
#' u1 : standardization of y (u1=y/c1)
#'
#' Coefs_regression : coefficients of the PLS regression for models with 1,...,n_components. Each row corresponds to one feature.
#'
#' Constant_regression : constant of the PLS Regression for models with 1,...,n_components.
#'
#' all_coefs_regression : all coefficients and constant of the PLS Regression for models with 1,...,n_components.
#'
#' Matrix_correlation : data frame of correlation between each component and all the variables (explanatory and target)
#'
#' RSS : matrix of PRESS values for models with 1, . . . , n_components.
#'
#' PRESS : matrix of PRESS values for models with 1, . . . , n_components.
#'
#' Q2 : matrix of Q2 values for models with 1, . . . , n_components.
#'
#' @export
#'
#' @examples
#' d<-train_test_splits(iris,0.7)
#' train<-d$data_train
#' objet_pls<-fit_pls(Species~.,train,n_components=4)
#' objet_pls[[1]]$all_coefs_regression
#' objet_pls[[2]]$all_coefs_regression
#' objet_pls[[3]]$all_coefs_regression
#'
#' m<-train_test_splits(iris,0.8)
#' trains<-m$data_train
#' fit_pls(Species~Petal.Length+Sepal.Length+Sepal.Width,trains,n_components=2)
#'
#' @details
#' The PLS-DA used consist in a PLS1 Regression.
#'
#' If the discrete target variable has more than two outcomes, the PLS1 regression is applied on each levels of the categorial variable and using a one versus rest strategy.
#'
#' Formula:
#'
#' A typical predictor has the form response ~ terms where response is the factor response vector and terms is a series of terms which specifies a PLS-DA predictor for response.
#'
#' A terms specification of the form first + second indicates all the terms in first together with all the terms in second with any duplicates removed.
#'
#' @references
#' TENENHAUSS M., "La régression PLS - Théorie et pratique", Editions Technip
#'
#' WOLD H. (1985), "Partial Least Squares", in Encyclopedia of Statistical Sciences, vol. 6, Kotz, S & Jonhson, N.L (Eds), John Wiley & Sons, New York, pp.581-591
#'
#' CHAVENT M. and PATOUILLE B, "Calcul des coefficients de regression et du PRESS en régression PLS1"
#'
#' Abdi H (2010). Partial least squares regression and projection on latent structure regression (PLS Regression). Wiley Interdisciplinary Reviews: Computational Statistics, 2(1), 97-106.

fit_pls<-function(formule,data,n_components=2){

  # Check if required parameters are specified
  if(missing(formule) || missing(data) || n_components<0){
    stop("fit_pls requires a formula and a data frame. Moreover n_components should be an integer greater than 0")
  }

   # # Check of the inputs
   # if(!is.formula(formule)){
   #   stop("formula must be of type formula")
   # }

  #Check if the data source is a data frame
  if(!is.data.frame(data)){
    stop("The data source must be a data frame")
  }

  if(n_components>5){
    print("Warning : Perhaps you can choose a maximum of five components and watch the evolution of the Q2 indicator")
  }

  # column matching control between data and formula
  f = formula(formule)
  colonne_names <- colnames(data)

  #Check the concordance between the columns of the formula and those contained in the data source
  for (v in all.vars(f)){
    if(!is.element(v, colonne_names) && v != '.'){
      print(paste("Correspondence error between variables:: -->", v))
      stop("Check the concordance between the columns of the formula and those of the data source")
    }
  }

  #Construct a data frame from formula
  df<-model.frame(formula = as.formula(formule), data = data)

  #Check if the number of components is consistent with the number of explanatory variables selected
  if (ncol(df)<=n_components){
    stop("The number of explanatory variables must be greater or equal to the number of components")
  }

  #Select the values of the target
  Y<-df[,1]
  #Check if there is a minimum of two explanatory variables selected
  if (ncol(df)==2){
    stop("You need minimum of two explanatory variables")
  }else{
    #Select the explanatory variables
    X<-df[,-1]
  }

  #Transform the categorical target variable in a dummy matrix
  Y_dummy<-ydummy(Y)

  #scale matrix X and dummy matrix
  X0<-xscale(X,reduce=FALSE)
  Y0<-xscale(Y_dummy,reduce=FALSE)

  #function that compute the PLS1 Regression for one outcome versus the rest of the outcomes
  fit_one_simple<-function(f,X,Y,Y_dummy,X0,Y0,modalite,n_components){

    #function computing the PRESS
    res_press<-function(x,y){
      #Create a matrix where the y(h-1)i-y~h(-i) are stored
      RES<-matrix(0,nrow(x),1)
      for (i in 1:nrow(x)){
        #Matrix of X(h-1) residuals without the "i" individual values
        xi<-x[-i,]
        #Matrix of y_(h-1) deprived of the i-th value
        yi<-y[-i]
        #Calculation of wh(-i)
        covi<-t(xi)%*%yi
        wi<-covi/as.numeric(sqrt(t(covi)%*%covi))
        #Calculation of th(-i)
        tki<-xi%*%wi
        #Calculation of ch(-i)
        ci<-as.numeric((t(yi)%*%tki)/(t(tki)%*%tki))
        #Calculation of y~h(-i)
        yresi<-ci*x[i,]%*%wi
        #Calculation of y(h-1)i-y~h(-i)
        RES[i]<-y[i]-yresi
        #Sum of square for the RES matrix values
        PRES<-t(RES)%*%RES
      }
      return(PRES)
    }

    #Set of matrices for a single Y (here y0=Y[,1])
    #Matrix of individuals : components
    Th <- matrix(0, nrow(X), n_components)
    #Matrix of "weights"/"scores"
    Wh <- matrix(0, ncol(X), n_components)
    #Matrix of regression coefficients of t_h with respect to y_h
    Ch <- matrix(0, 1, n_components)
    #Matri of regression coefficients of x_j with respect to t_k
    Ph <- matrix(0, ncol(X), n_components)
    #Matrix of residuals from successive regressions of y against t
    Yh<-matrix(0,nrow(X),n_components)
    #W* matrix
    Wsh<-matrix(0,ncol(X),n_components)
    #Ah matrix
    Ah<-matrix(0,ncol(X),n_components)
    #RSS matrix
    RSS<-matrix(0,1,n_components)
    #PRESS matrix
    PRESS<-matrix(0,1,n_components)
    #Q2 matrix
    Q2<-matrix(0,1,n_components)

    x<-X0
    #Select one outcome versus the rest of the outcomes
    y<-Y0[,modalite]

    h<-1
    #RSS 0
    RSS0<-nrow(X)-1


    while (h<=n_components){
      Yh[,h]<-y

      #Calculation of w_h
      cov<-t(x)%*%y
      #w represents the link between each predictive variable and the specified outcome
      w<-cov/as.numeric(sqrt(t(cov)%*%cov))
      Wh[,h]<-w

      #Calculation of tk_h
      #h_ième component : th is a linear combination of the x set of vector
      #tk is a column vector where the lines are individuals
      tk<-x%*%w
      Th[,h]<-tk

      #Calculation of c_h
      #c is a float corresponding to the slope of the linear regression of y on tk_h
      ch<-as.numeric((t(y)%*%tk)/(t(tk)%*%tk))
      Ch[,h]<-ch

      #y_h : new y where y_(h-1)=c_h*tk_h+y_h thus y_h=y_(h-1)-c_h*tk_h
      #Matrix of residuals values of the linear regression of y_(h-1) on tk_h
      #y is a column vector where the lines are individuals
      y<-y-ch*tk
      Yh[,h]<-y

      #Calculation of RSS
      RSS[,h]<-t(y)%*%y

      #Calculation of PRESS
      PRESS[,h]<-res_press(x,y)

      #Calculation of Q^2
      if (h==1){
        Q2[,h]<-1-PRESS[,h]/RSS0
      }else{
        Q2[,h]<-1-PRESS[,h]/RSS[,h-1]
      }

      #p_h : slope of the linear regression of x_hj on tk_h
      #p is a column vector where the lines are variables
      p<-t(x)%*%tk/as.numeric(t(tk)%*%tk)
      Ph[,h]<-p

      #residuals x_h : new x where x_(h-1)=x_h-p_h*tk_h thus x_h=x_(h-1)-tk_h*p_h
      #Matrix of residuals values of the linear regression of x_hj on tk_h
      #Matrix (row for individuals, columns for variables)
      x<-x-tk%*%t(p)

      #Calculation of w*h
      ws<-w
      k<-1
      while (k<h){
        ws<-ws-Wh[,k]%*%t(Ph[,k])%*%w
        k<-k+1
      }
      Wsh[,h]<-ws

      #Calculation of Ah
      if (h==1){
        Ah[,h]<-Ch[,h]*Wsh[,h]
      }else{
        Ah[,h]<-Ch[,h]*Wsh[,h]+Ah[,h-1]
      }

      h<-h+1
    }

    #Transform matrix in data frame
    Th_df<-as.data.frame(Th[,1:n_components],row.names = rownames(x))
    Wh_df<-as.data.frame(Wh[,1:n_components],row.names = colnames(x))
    Ph_df<-as.data.frame(Ph[,1:n_components],row.names = colnames(x))
    Yh_df<-as.data.frame(Yh[,1:n_components],row.names = rownames(x))
    Wsh_df<-as.data.frame(Wsh[,1:n_components],row.names = colnames(x))
    Ah_df<-as.data.frame(Ah[,1:n_components],row.names = colnames(x))

    Coefs<-matrix(0,ncol(X),1)
    constant<-mean(Y_dummy[,modalite])

    #Computing the coefficients of the regression of y on x
    for (i in 1:ncol(X)){
      si<-apply(X,2,sd)[i]
      mi<-apply(X,2,mean)[i]
      Coefs[i,1]<-Ah_df[i,n_components]*sd(Y_dummy[,modalite])/si
      #constant
      constant<-constant-Coefs[i,1]*mi
    }

    #Transform into a data frame
    Coefs_df<-as.data.frame(Coefs)
    Coefs_df[ncol(X)+1,] <- constant
    rownames(Coefs_df)<-c(colnames(X),"constant")
    names(Coefs_df)<-c("Coefficients")

    #u1=y/c1 : normalization of y (where y=c1*t1+y1)
    u1<-Y0[,modalite]/Ch[,1]

    #Correlation between th and explanatory variables
    #Correlation matrix of individuals : components
    Cor_var<-matrix(0,ncol(X)+1,n_components)

    #R^2 between th and explanatory variables
    for (j in 1:n_components){
      for (i in 1:ncol(X)){
        Cor_var[i,j]<-cor(Th[,j],X0[,i])
      }
      Cor_var[ncol(X)+1,j]<-cor(Th[,j],Y0[,1])
    }
    Cor_var_df<-as.data.frame(Cor_var)
    rownames(Cor_var_df)<-c(colnames(X),"Y")

    #list of the results given by the fit_one_simple function
    results<-list("formula"=f,"X"=X,"Y_target"=Y,"Ydum"=Y_dummy,"n_components"=n_components,"Components"=Th_df,"Features_Weights"=Wh_df,"Coefs_yresiduals_comp"=Ch,"Coefficients_xresiduals_components"=Ph_df,"y_residuals"=Yh_df,"W_star"=Wsh_df,"ah"=Ah_df,"u1"=u1,"Coefs_regression"=Coefs,"Constant_regression"=constant,"all_coefs_regression"=Coefs_df,"Matrix_correlation"=Cor_var_df,"RSS"=RSS,"PRESS"=PRESS,"Q2"=Q2)
    return(results)
  }

  #Instance PLS
  objet_pls<-list()

  #Compute all the models of PLS1 for each outcome versus the others
  for (i in 1:nlevels(Y)){
    objet_pls[[i]]<-fit_one_simple(f,X,Y,Y_dummy,X0,Y0,modalite=i,n_components)
  }

  class(objet_pls)<-"PLS"
  return(objet_pls)
}

#' Regression PLS for a data frame
#'
#' @param formule is a formula
#' @param data is a data frame
#' @param n_components is an integer
#' @param n_cores is and integer
#'
#' @import parallel
#' @import doParallel
#' @import foreach
#' @importFrom stats formula as.formula model.frame cor
#'
#' @return the matrix Wh, Ah, etc
#' @export
#'
#' @examples
#' fit_pls_parall(Species~.,iris,n_components=4,n_cores=1)

fit_pls_parall<-function(formule,data,n_components=2,n_cores=1){

  # Check if required parameters are specified
  if(missing(formule) || missing(data) || n_components<0){
    stop("fit_pls requires a formula and a data frame. Moreover n_components should be an integer greater than 0")
  }

  #detect the number of cores and max = number of max cores detected -1
  max_cores<-parallel::detectCores()-1

  #Check the number of cores
  if (n_cores<1 || n_cores>max_cores){
    stop("The number of cores is not appropriate")
  }

  # # Check of the inputs
  # if(!is.formula(formule)){
  #   stop("formula must be of type formula")
  # }

  if(!is.data.frame(data)){
    stop("The data source must be a data frame")
  }

  if(n_components>5){
    print("Warning : Perhaps you can choose a maximum of five components and watch the evolution of the Q2 indicator")
  }

  # column matching control between data and formula
  f = formula(formule)
  colonne_names <- colnames(data)

  for (v in all.vars(f)){
    if(!is.element(v, colonne_names) && v != '.'){
      print(paste("Correspondence error between variables:: -->", v))
      stop("Check the concordance between the columns of the formula and those of the data source")
    }
  }

  # construct a data frame from formula
  df<-model.frame(formula = as.formula(formule), data = data)

  if (ncol(df)<=n_components){
    stop("The number of explanatories variables mus be greater or equal to the number of components")
  }

  Y<-df[,1]
  if (ncol(df)==2){
    stop("You need minimum of two explanatory variables")
  }else{
    X<-df[,-1]
  }

  #Transform the categorial target variable in a dummy matrix
  Y_dummy<-dummy(Y)

  #scale matrix X and dummy matrix
  X0<-scale(X)
  Y0<-scale(Y_dummy)

  fit_one_simple<-function(f,X,Y,Y_dummy,X0,Y0,modalite,n_components){

    res_press<-function(x,y){
      #création d'une matrice où on stocke les y(h-1)i-y~h(-i)
      RES<-matrix(0,nrow(x),1)
      for (i in 1:nrow(x)){
        #matrice des résidus X(h-1) privé de la ligne i
        xi<-x[-i,]
        #matrice des y_(h-1) privé de la i-ème valeur
        yi<-y[-i]
        #calcul de wh(-i)
        covi<-t(xi)%*%yi
        wi<-covi/as.numeric(sqrt(t(covi)%*%covi))
        #calcul de th(-i)
        tki<-xi%*%wi
        #calcul de ch(-i)
        ci<-as.numeric((t(yi)%*%tki)/(t(tki)%*%tki))
        #calcul de y~h(-i)
        yresi<-ci*x[i,]%*%wi
        #calcul de y(h-1)i-y~h(-i)
        RES[i]<-y[i]-yresi
        #somme des carrés des valeurs de la matrice RES
        PRES<-t(RES)%*%RES
      }
      return(PRES)
    }

    #Ensemble des matrices pour un seul Y (ici y0=Y[,1])
    #Matrice des individus : composantes
    Th <- matrix(0, nrow(X), n_components)
    #Matrice des "poids"/"scores" des variables
    Wh <- matrix(0, ncol(X), n_components)
    #Matrice des coefficients de regression de t par rapport à y
    Ch <- matrix(0, 1, n_components)
    #Matrice des coefficients de la droite de regression de x_j par rapport à tk
    Ph <- matrix(0, ncol(X), n_components)
    #Matrice des résidus des regressions successives de y par rapport à t
    Yh<-matrix(0,nrow(X),n_components)
    #Matrice W*
    Wsh<-matrix(0,ncol(X),n_components)
    #Matrice Ah
    Ah<-matrix(0,ncol(X),n_components)
    #Matrice RSS
    RSS<-matrix(0,1,n_components)
    #Matrice RSS
    PRESS<-matrix(0,1,n_components)
    #Matrice RSS
    Q2<-matrix(0,1,n_components)

    x<-X0
    y<-Y0[,modalite]

    h<-1
    #RSS 0
    RSS0<-nrow(X)-1

    while (h<=n_components){
      Yh[,h]<-y

      #calcul de w_h
      cov<-t(x)%*%y
      #w est une matrice colonne où les lignes sont les variables
      w<-cov/as.numeric(sqrt(t(cov)%*%cov))
      Wh[,h]<-w

      #calcul de tk_h
      #h_ième composante : combinaison linéaire des w par x
      #tk est une matrice colonne où les lignes sont les individus
      tk<-x%*%w
      Th[,h]<-tk

      #calcul de c_h
      #c est un réel il correspond au coefficient de la droite de regression de y en tk_h
      ch<-as.numeric((t(y)%*%tk)/(t(tk)%*%tk))
      Ch[,h]<-ch

      #y_h : new y où y_(h-1)=c_h*tk_h+y_h donc y_h=y_(h-1)-c_h*tk_h
      #matrice des résidus de la regression de y_(h-1) sur tk_h
      #y est une matrice colonne où les lignes sont les individus
      y<-y-ch*tk
      Yh[,h]<-y
      RSS[,h]<-t(y)%*%y
      PRESS[,h]<-res_press(x,y)
      if (h==1){
        Q2[,h]<-1-PRESS[,h]/RSS0
      }else{
        Q2[,h]<-1-PRESS[,h]/RSS[,h-1]
      }

      #p_h sont les coefficients de la droite de regression de x_hj par rapport à tk_h
      #p est une matrice colonne où les lignes sont les variables
      p<-t(x)%*%tk/as.numeric(t(tk)%*%tk)
      Ph[,h]<-p

      #résidus x_h : new x où x_(h-1)=x_h-p_h*tk_h donc x_h=x_(h-1)-tk_h*p_h
      #matrices des résidus des regressions de x_hj sur tk_h
      #C'est une matrice où le nombre de lignes sont les individus
      #et le nombre de colonnes sont les variables
      x<-x-tk%*%t(p)

      #w*h
      ws<-w
      k<-1
      while (k<h){
        ws<-ws-Wh[,k]%*%t(Ph[,k])%*%w
        k<-k+1
      }
      Wsh[,h]<-ws

      #print(Ch[,1]*Wsh[,1])
      if (h==1){
        Ah[,h]<-Ch[,h]*Wsh[,h]
      }else{
        Ah[,h]<-Ch[,h]*Wsh[,h]+Ah[,h-1]
      }

      h<-h+1
    }

    n_comp<-n_components

    Th_df<-as.data.frame(Th[,1:n_comp],row.names = rownames(x))
    Wh_df<-as.data.frame(Wh[,1:n_comp],row.names = colnames(x))
    Ph_df<-as.data.frame(Ph[,1:n_comp],row.names = colnames(x))
    Yh_df<-as.data.frame(Yh[,1:n_comp],row.names = rownames(x))
    Wsh_df<-as.data.frame(Wsh[,1:n_comp],row.names = colnames(x))
    Ah_df<-as.data.frame(Ah[,1:n_comp],row.names = colnames(x))

    Coefs<-matrix(0,ncol(X),1)
    #coefficients de la droite de régression de y en x
    #sans centrée réduire
    constant<-mean(Y_dummy[,modalite])
    for (i in 1:ncol(X)){
      si<-apply(X,2,sd)[i]
      mi<-apply(X,2,mean)[i]
      Coefs[i,1]<-Ah_df[i,n_components]*sd(Y_dummy[,modalite])/si
      #constant
      constant<-constant-Coefs[i,1]*mi
    }
    Coefs_df<-as.data.frame(Coefs)
    Coefs_df[ncol(X)+1,] <- constant
    rownames(Coefs_df)<-c(colnames(X),"constant")
    names(Coefs_df)<-c("Coefficients")

    #
    u1<-Y0[,modalite]/Ch[,1]

    #corrélation  entre th et variables explicatives
    #Matrice correlation des individus : composantes
    Cor_var<-matrix(0,ncol(X)+1,n_comp)
    #corrélation ^2 entre th et variables explicatives
    #à faire pour i allant de 1 à n_comp et j allant de 1 à p
    for (j in 1:n_comp){
      for (i in 1:ncol(X)){
        Cor_var[i,j]<-cor(Th[,j],X0[,i])
      }
      Cor_var[ncol(X)+1,j]<-cor(Th[,j],Y0[,1])
    }
    Cor_var_df<-as.data.frame(Cor_var)
    rownames(Cor_var_df)<-c(colnames(X),"Y")

    results<-list("formula"=f,"X"=X,"Y_target"=Y,"Ydum"=Y_dummy,"n_components"=n_comp,"Components"=Th_df,"Features_Weights"=Wh_df,"Coefs_yresiduals_comp"=Ch,"Coefficients_xresiduals_components"=Ph_df,"y_residuals"=Yh_df,"W_star"=Wsh_df,"ah"=Ah_df,"u1"=u1,"Coefs_regression"=Coefs,"Constant_regression"=constant,"all_coefs_regression"=Coefs_df,"Matrix_correlation"=Cor_var_df,"RSS"=RSS,"PRESS"=PRESS,"Q2"=Q2)
    return(results)
  }

  #ce sera l'objet PLSDA
  objet_pls<-list()

   cl <- parallel::makeCluster(n_cores)
   doParallel::registerDoParallel(cl)

   objet_pls<-foreach(i = 1:nlevels(Y),.inorder=TRUE) %dopar% {
     objet_pls[[i]]<-fit_one_simple(f,X,Y,Y_dummy,X0,Y0,modalite=i,n_components)
   }

   parallel::stopCluster(cl)

  class(objet_pls)<-"PLS"
  return(objet_pls)
}

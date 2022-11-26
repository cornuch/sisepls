# M2_SISE_PLSDA_R

PLS-DA: Partial Least Square Discriminant Analysis Regression

DESCRIPTION

This project is part of our training in Data Science at the University of Lyon 2. The main objective is to develop an R package under S3 that allows to do a Partial Least Square Discriminant Analysis. 

Partial Least Square Discriminant Analysis (expliquer ce que c'est que cette méthode) 

Here are the different functionalities of our package that we will present in the following lines: (explique ce que font ces fonctions)
  
    train_test_split : 
    fit_pls :
    summary.PLS :
    print.PLS :
    plot_circle_correlation
    plot
    predict_pls
    summary_predict

In order to use our package, you should install it from Github.

library(devtools)
install_github("cornuch/M2_SISE_PLSDA_R")

Once the package is downloaded and successfully installed, please load it for use.

library(sisepls)

Now you can access all available functions of the package. To prove it, we will display the documentation of our fit function. you can write in your console: ?fit_pls to see the documentation or:

help(fit_pls)

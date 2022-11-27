# sisepls

PLS-DA: Partial Least Square Discriminant Analysis Regression

# DESCRIPTION

This project is part of our training in Data Science at the University of Lyon 2. The main objective is to develop an R package under S3 that allows to do a Partial Least Square Discriminant Analysis. 

# Partial Least Square Discriminant Analysis

Partial Least Squares Disriminant Analysis regression (PLS) is often used when there are a lot of explanatory continuous variables, possibly correlated and where the target variable Y is discrete (categorial).Partial Least Squares regression (PLS) is an efficient and optimal regression method based on covariance. 


# Installation and data loadings

In order to use our package, you should install it from Github.

>library(devtools)

>install_github("cornuch/sisepls")

Once the package is downloaded and successfully installed, please load it for use.

> library(sisepls)

Now you can access all available functions of the package. 
   
To prove it, we will display the documentation of our fit function. you can write in your console: ?fit_pls to see the documentation or:

   help(fit_pls)

Here are the different functionalities of our package that we will present in the following lines: 
  
    train_test_splits:  
    fit_pls:
    summary.PLS:
    print.PLS:
    plot_Q2:
    plot_circle_correlation: 
    plot_explanatories_weights:
    plot_individuals_components:
    plot_individuals:
    predict_pls :
    summary_predict :

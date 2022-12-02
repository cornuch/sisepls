# sisepls

# PLS-DA: Partial Least Square Discriminant Analysis Regression

DESCRIPTION

This project is part of our training in Data Science at the University of Lyon 2. The main objective is to develop an R package under S3 that allows to do a Partial Least Square Discriminant Analysis. Moreover we had to build an interactive web apps R-Shiny showing the results and graphs of our package.

The Partial Least Square Discriminant Analysis regression method iteratively constructs h components t = (t1, t2, . . . , th) (the number of components being between 2 and the number of explanatory variables q) of two to two orthogonal and maximizing their covariance with the target variable. Indeed, the components th will be an ancillary variable that measures the relationship between the target and explanatory variables.
and the explanatory variables.

Here are the different functionalities of our package that we will present in the following lines:
  
    * train_test_splits
    * fit_pls
      *summary.PLS
      *print.PLS
      *plot_circle_correlation
      *plot_explanatories
   * predict_pls
      *summary_predict

In order to use our package, you should install it from Github.

library(devtools)

install_github("cornuch/sisepls")

Once the package is downloaded and successfully installed, please load it for use.

library(sisepls)

Now you can access all available functions of the package. To prove it, we will display the documentation of our fit function. you can write in your console: ?fit_pls to see the documentation or:

help(fit_pls)

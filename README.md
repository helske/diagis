[![Build Status](https://travis-ci.org/helske/diagis.png?branch=master)](https://travis-ci.org/helske/diagis)
[![codecov.io](http://codecov.io/github/helske/diagis/coverage.svg?branch=master)](http://codecov.io/github/helske/diagis?branch=master)
[![downloads](http://cranlogs.r-pkg.org/badges/diagis)](http://cranlogs.r-pkg.org/badges/diagis)
[![cran version](http://www.r-pkg.org/badges/version/diagis)](http://cran.r-project.org/package=diagis)

diagis: Diagnostic Plot and Multivariate Summary Statistics of Weighted Samples from Importance Sampling
=======================================================================================

`diagis` is a small package containing functions relating weighted samples obtained for example from importance sampling. 
The main motivation for developing `diagis` was to enable easy computation of summary statistics and diagnostics of the
weighted MCMC runs provided by [`bssm`](https://github.com/helske/bssm) package for Bayesian state space modelling. For more broader use, the `diagis` package provides functions for computing weighted means and covariances of possibly multivariate samples, the running versions of these, as well as diagnostic plot function `weight_plot` for graphical diagnostic of weights. Please see vignette for details.


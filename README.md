[![R-CMD-check](https://github.com/helske/diagis/workflows/R-CMD-check/badge.svg)](https://github.com/helske/diagis/actions)
[![Codecov test coverage](https://codecov.io/gh/helske/diagis/branch/master/graph/badge.svg)](https://codecov.io/gh/helske/diagis?branch=master)
[![downloads](http://cranlogs.r-pkg.org/badges/diagis)](http://cranlogs.r-pkg.org/badges/diagis)
[![cran version](http://www.r-pkg.org/badges/version/diagis)](http://cran.r-project.org/package=diagis)

diagis: Diagnostic Plot and Multivariate Summary Statistics of Weighted Samples from Importance Sampling
=======================================================================================

`diagis` is a small package containing functions relating weighted samples obtained for example from importance sampling. 
The main motivation for developing `diagis` was to enable easy computation of summary statistics and diagnostics of the
weighted MCMC runs provided by [`bssm`](https://github.com/helske/bssm) package for Bayesian state space modelling. For more broader use, the `diagis` package provides functions for computing weighted means, covariances, and quantiles of possibly multivariate samples, the running versions of (some of) these, as well as diagnostic plot function `weight_plot` for graphical diagnostic of weights. Please see vignette for more details.


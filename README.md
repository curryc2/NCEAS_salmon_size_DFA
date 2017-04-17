# NCEAS_salmon_size_DFA:
### Example code for NCEAS meeting, showing methods for implementing Dynamic Factor Analysis using both EM and Bayesian methods.

***
## Description of Model Files

Name                           | Description
-------------------------------|---------------------------------
Sockeye Size DFA.R             | Example of how to run DFA using the [MARSS](https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf) package in R, conducting estimation with EM alorithm.
Bayes DFA.R                    | Bayesian example of DFA using the [statss](https://github.com/nwfsc-timeseries/statss) package.
MARSS MLE Selection Example.R  | Example of AICc-based model selection for parsimonious number of common trends using the [MARSS](https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf) package.

***
## Citation Information:
#### Examples provided are based on length-at-age data provided by the Alaska Department of Fish and Game (ADF&G). 

#### All R scripts and helper functions are based on the following R packages (please cite accordingly):

* [MARSS](https://cran.r-project.org/web/packages/MARSS/MARSS.pdf) package, created by: Eli Holmes, Eric Ward, and Kellie Wills, NOAA, Seattle, US. The MARSS package provides maximum-likelihood parameter estimation for con-
strained and unconstrained linear multivariate autoregressive state-space (MARSS) mod-
els fit to multivariate time-series data
* [statss](https://github.com/nwfsc-timeseries/statss) package, created by: Eric Ward, Mark Scheuerell, and Eli Holmes. This Package Fits Stan Routines To Univariate And Multivariate Time Series (STan Analysis of Time SerieS).
* Please consult the [MARSS User Guide](https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf) for examples and information on correct model parameterization.

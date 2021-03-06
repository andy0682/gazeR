
# gazeR

<!-- badges: start -->

[![Build
Status](https://app.travis-ci.com/andy0682/gazeR.svg?branch=master)](https://app.travis-ci.com/andy0682/gazeR)
<!-- badges: end -->

The goal of gazeR is to identify visual fixations from eye-tracking data
with Density Based Clustering. For now the only tool is based on the
dbscan algorithm (from the dbscan library).

Here is a brief description of the package implementation procedure:

-   The package and its documentations were initiated in Rstudio with
    the devtools and roxygen2 libraries.
-   An initial Unit Test Protocol was implemented with the testthat
    library.
-   Final full check (as with R cmd check) was performed with the
    devtools::check() function.
-   The packages was then pushed to this GitHub repository.
-   Continuous integration with Travis was implemented via
    github_actions (<https://app.travis-ci.com/github/andy0682/gazeR>)

## Installation

You can install this version under development of gazeR from:

``` r
# install.packages("devtools")
devtools::install_github("andy0682/gazeR")
```

For the moment the package has only one function: gazeClusters(dat, eps,
minPts = 5, recRate = NULL)

If you download the package, you can see the function documentation by
typing ?gazeClusters in the console.

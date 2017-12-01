
<!-- README.md is generated from README.Rmd. Please edit that file -->
isorunN2O
=========

[![Build Status](https://travis-ci.org/sebkopf/isorunN2O.svg)](https://travis-ci.org/sebkopf/isorunN2O) [![codecov.io](https://codecov.io/github/sebkopf/isorunN2O/coverage.svg?branch=master)](https://codecov.io/github/sebkopf/isorunN2O?branch=master)

This package facilitates the reading, processing and visualization of N2O isotope data including drift correction, 17O correction and standards calibration.

Installation
------------

The package is based on the functionality provided by the [`isoread`](https://github.com/sebkopf/isoread#isoread) and [`isotopia`](https://github.com/sebkopf/isotopia#isotopia) packages to load IRMS data directly from their raw data files. All three can be installed directly from GitHub using the `devtools` package (see [troubleshooting](#troubleshooting) section below in case of issues with `devtools` installs).

``` r
install.packages("devtools")
devtools::install_github("sebkopf/isotopia", build_vignettes = TRUE, dependencies = TRUE)
devtools::install_github("sebkopf/isoread", build_vignettes = TRUE, dependencies = TRUE)
devtools::install_github("sebkopf/isorunN2O", build_vignettes = TRUE, dependencies = TRUE)
```

How to use?
-----------

The package includes a tutorial that introduces most functionality and provides examples of how it works and can be used. After the `isorunN2O` package is installed, the newest version of this tutorial can always be loaded up as a vignette directly in R by calling `vignette("N2O_data_reduction_tutorial")` in the command line. The RMarkdown file underlying the vignette is also available [directly in this repository](vignettes/N2O_data_reduction_tutorial.Rmd) including the resulting [HTML output](https://rawgit.com/sebkopf/isorunN2O/master/inst/doc/N2O_data_reduction_tutorial.html).

Additional functionality: N2O Data Viewer
-----------------------------------------

This package also includes a browser based application for interactive N2O run monitoring. It can be launched with the function `run_data_viewer()`, additional information is [available here](https://github.com/sebkopf/isorunN2O/tree/master/inst/shiny-apps/data_viewer#n2o-data-viewer).

![Screenshot of the Data Viewer](https://github.com/sebkopf/isorunN2O/blob/master/inst/shiny-apps/data_viewer/doc/data_overview.png?raw=true)

Troubleshooting
---------------

On Windows, the above `install_github` calls can fail because of the requirement to create zip archieves during some vignette creation steps. If this is the case, [install Rtools](https://cran.r-project.org/bin/windows/Rtools/), restart R and RStudio and try to run the installation again.

If you run into any trouble installting the packages that is not resolved easily, try installing them again without the vignettes, which are sometimes the sole culprits, i.e. `devtools::install_github(...., build_vignettes = FALSE)`, which should provide the fully functional package, just without the `vignette` tutorial (which you can still check out online as mentioned in the **How to use?** section).

#### Issues with automatic dependency installation

The latest version of the `devtools` package on CRAN (version 1.12.0) has a bug that causes issues with the installation of package dependencies, especially on Windows, which usually manifest as errors during the installation of GitHub packages stating some dependency (e.g. `stringi` or `dplyr` or `htmltools`, etc. package missing). Please check this [troubleshooting Gist](https://gist.github.com/sebkopf/3cf82afb2e535e92f2cfcf3e66d48475) for details if you encounter this error.

isorunN2O
=======
[![Build Status](https://travis-ci.org/sebkopf/isorunN2O.svg)](https://travis-ci.org/sebkopf/isorunN2O)
[![codecov.io](https://codecov.io/github/sebkopf/isorunN2O/coverage.svg?branch=master)](https://codecov.io/github/sebkopf/isorunN2O?branch=master)

This package facilitates the reading, processing and visualization of N2O isotope data including drift correction, 17O correction and standards calibration.

## Installation

The package is based on the functionality provided by the [`isoread`](https://github.com/sebkopf/isoread#isoread) and [`isotopia`](https://github.com/sebkopf/isotopia#isotopia) packages to load IRMS data directly from their raw data files. All three can be installed directly from GitHub using the `devtools` package. Additionally, the package uses interactive plotting tools provided by [plotly](https://github.com/ropensci/plotly), which may not yet be available officially but can also be installed from GitHub.

```{r, eval = FALSE}
install.packages("devtools")
devtools::install_github("sebkopf/isotopia", build_vignettes = TRUE)
devtools::install_github("sebkopf/isoread", build_vignettes = TRUE)
devtools::install_github("ropensci/plotly")
devtools::install_github("sebkopf/isorunN2O", build_vignettes = TRUE)
```

## How to use?

The package includes a tutorial that introduces most functionality and provides examples of how it works and can be used. After the `isorunN2O` package is installed, the newest version of this tutorial can always be loaded up as a vignette directly in R by calling `vignette("N2O_data_reduction_tutorial")` in the command line. The RMarkdown file underlying the vignette is also available [directly in this repository](vignettes/N2O_data_reduction_tutorial.Rmd) including the resulting [HTML output](https://rawgit.com/sebkopf/isorunN2O/master/inst/doc/static/N2O_data_reduction_tutorial.html).

## Additional functionality

This package also includes a browser based application for interactive N2O run monitoring. It can be launched with the function `run_data_viewer()`, additional information is available [here](https://github.com/sebkopf/isorunN2O/tree/master/inst/shiny-apps/data_viewer#n2o-data-viewer).


#### Troubleshooting

On Windows, the above `install_github` calls can fail because of the requirement to create zip archieves during some vignette creation steps. If this is the case, [install Rtools](https://cran.r-project.org/bin/windows/Rtools/), restart R and RStudio and try to run the installation again. 

If you run into any trouble installting the packages that is not resolved easily, try installing them again without the vignettes, which are sometimes the sole culprits, i.e. `devtools::install_github(...., build_vignettes = FALSE)`, which should provide the fully functional package, just without the `vignette` tutorial (which you can still check out online as mentioned in the **How to use?** section).


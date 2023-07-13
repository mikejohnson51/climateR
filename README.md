
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Welcome!

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/158620263.svg)](https://zenodo.org/badge/latestdoi/158620263)
[![R CMD
Check](https://github.com/mikejohnson51/climateR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/climateR/actions/workflows/R-CMD-check.yaml)
[![Dependencies](https://img.shields.io/badge/dependencies-7/25-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/mikejohnson51/climateR/branch/master/graph/badge.svg?token=7zs6C91SDw)](https://codecov.io/gh/mikejohnson51/climateR)
<!-- badges: end -->

`climateR` simplifies the steps needed to get climate data into R. At
its core it provides three main things:

1.  A catalog of over 100,000k datasets from over 2,000 data
    providers/archives. See (`climateR::params`)

``` r
nrow(params)
#> [1] 107857
length(unique(params$id))
#> [1] 2075
length(unique(params$asset))
#> [1] 4653
```

This catalog is an [evolving, federated collection of
datasets](https://github.com/mikejohnson51/climateR-catalogs) that can
be accessed by the data access utilities.

2.  A general [toolkit for accessing remote and local gridded
    data](https://mikejohnson51.github.io/climateR/reference/index.html#data-access)
    files bounded by space, time, and variable constraints (`dap`,
    `dap_crop`, `read_dap_file`)

3.  A set of
    [shortcuts](https://mikejohnson51.github.io/climateR/reference/index.html#shortcuts)
    that implement these methods for a core set of selected catalog
    elements

> :warning: **Python Users**: Access the to data catalogs is available
> through the USGS
> [`gdptools`](https://gdptools.readthedocs.io/en/latest/) package.
> Directly analogous climateR functionality can be found in
> [`climatePy`](https://github.com/LynkerIntel/climatePy)

# Installation

``` r
remotes::install_github("mikejohnson51/AOI") # suggested!
remotes::install_github("mikejohnson51/climateR")
```

# Basic Usage

Finding rainfall in Colorado between October 29,1991 - November 6, 1991.
The source dataset for this example uses the getGridMET shortcut.

``` r
library(AOI)
library(terra)
library(climateR)

AOI = aoi_get(state = "CO", county = "all")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
system.time({
  d = getGridMET(AOI,
               varname = "pr",
               startDate = "1991-10-29",
               endDate  = "1991-11-06")
})
#>    user  system elapsed 
#>   0.257   0.056   1.223
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

# Integration with `zonal`

``` r
library(zonal)

system.time({
  county = zonal::execute_zonal(d, geom = AOI, ID = "fip_code")
})
#>    user  system elapsed 
#>   0.421   0.025   0.494
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />


# manypkgs <img src="man/figures/manypkgslogo.png" align="right" width="220"/>

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![GitHub release (latest by
date)](https://img.shields.io/github/v/release/globalgov/manypkgs)
![GitHub Release
Date](https://img.shields.io/github/release-date/globalgov/manypkgs)
![GitHub
issues](https://img.shields.io/github/issues-raw/globalgov/manypkgs)
<!-- [![HitCount](http://hits.dwyl.com/globalgov/manydata.svg)](http://hits.dwyl.com/globalgov/manydata) -->
[![Codecov test
coverage](https://codecov.io/gh/globalgov/manypkgs/branch/main/graph/badge.svg)](https://codecov.io/gh/globalgov/manypkgs?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/globalgov/manypkgs/badge)](https://www.codefactor.io/repository/github/globalgov/manypkgs)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/4867/badge)](https://bestpractices.coreinfrastructure.org/projects/4867)
<!-- ![GitHub All Releases](https://img.shields.io/github/downloads/jhollway/roctopus/total) -->
<!-- badges: end -->

`{manypkgs}` is a package from the
[manydata](https://github.com/globalgov/manydata) ecosystem. It provides
contributors the necessary tools to put their data in the hands of
users. The package includes many functions to make this easier,
including functions to help set up a package, import their existing
data, and export them in structures consistent with the rest of the data
ecosystem. This facilitates the interoperability, contrast, and
comparison of data. There is a specific coding system that should be
followed by contributors. For more details, please see the
[vignette](https://globalgov.github.io/manypkgs/articles/developer.html).

## Downloading and installing manypkgs

The development version of the package `{manypkgs}` can be downloaded
from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("globalgov/manypkgs")
```

## Cheat Sheet

<a href="https://github.com/globalgov/manypkgs/blob/main/man/figures/cheatsheet.pdf"><img src="https://raw.githubusercontent.com/globalgov/manypkgs/main/man/figures/cheatsheet.png" width="525" height="378"/></a>

## Our ecosystem of packages

The [manydata](https://github.com/globalgov/manydata) universe aimed at
collecting, connecting and correcting network data across issue-domains
of global governance. The `manydata::get_packages()` function can be
used to discover the packages from our ecosystem currently available.

``` r
# remotes::install_github("globalgov/manydata")
manydata::get_packages()
```

Please see [the website](https://globalgov.github.io/manypkgs/) for more
information about how to use `{manypkgs}` as a developer or as a data
contributor. For more information on `{manydata}`, please see [the
website](https://globalgov.github.io/manydata/).


<!-- README.md is generated from README.Rmd. Please edit that file -->

# okara

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/omerkara/okara)
[![star this
repo](https://githubbadges.com/star.svg?user=omerkara&repo=okara&style=default)](https://github.com/omerkara/okara)
[![fork this
repo](https://githubbadges.com/fork.svg?user=omerkara&repo=okara&style=default)](https://github.com/omerkara/okara/fork)
<!-- badges: end -->

[Ömer
Kara](https://drive.google.com/file/d/1riCNALzVc6QsYIGVIlfGYIeEqMLQ_jfQ/preview)

------------------------------------------------------------------------

R/okara is an [R](https://www.r-project.org/) package with miscellaneous
R functions that are useful to me. Additionally, some of these R
functions are frequently used by students in [my
courses](https://akademiekonometri.netlify.app/courses/).

------------------------------------------------------------------------

## Installation

To install `okara` package, you need to install and/or load the
[`devtools`](https://devtools.r-lib.org//index.html) package. The below
code automatically installs and/or loads it for you.

``` r
if("devtools" %in% rownames(installed.packages()) == FALSE) {suppressWarnings(install.packages("devtools"))}
suppressWarnings(library("devtools"))
```

You can install the `okara` package from
[GitHub](https://github.com/omerkara/okara) with the code below.

``` r
devtools::install_github("omerkara/okara")
```

You can also use the below codes to install the `okara` package with
dependencies and vignettes.

``` r
devtools::install_github("omerkara/okara", dependencies = TRUE) ## With dependencies.
devtools::install_github("omerkara/okara", build_vignettes = TRUE) ## Builds the vignette on the fly.
```

## Loading

You can load the `okara` package with the code below.

``` r
library("okara")
```

## Get Help

You can get help about the `okara` package with the code below.

``` r
help(package = "okara")
```

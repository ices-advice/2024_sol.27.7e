2024_sol.27.7e_assessment
================

## Sole (*Solea solea*) in Division 7.e (western English Channel) - WGCSE

This repository recreates the stock assessment (with XSA) for Sole
(*Solea solea*) in Division 7.e (western English Channel) in `R` from
WGCSE 2024.

## R packages

The following R packages from CRAN are required to run the assessment:

``` r
icesTAF
icesAdvice
icesSAG
ggplot2
dplyr
reshape
Cairo
tidyr
foreach
cowplot
patchwork
```

They can be installed with:

``` r
### list with required packages
req_pkgs <- c("icesTAF", "icesAdvice", "icesAdvice", "ggplot2", "dplyr",
              "reshape", "Cairo", "tidyr", "foreach", "cowplot", "patchwork")
### install packages which are not installed on the system
install.packages(req_pkgs[!req_pkgs %in% installed.packages()])
```

Furthermore, the following FLR (<https://www.flr-project.org>,
<https://github.com/flr>) packages are required:

``` r
FLCore
FLAssess
FLXSA
FLash
ggplotFL
```

For exact reproducibility, it is recommended to use exactly the same
package version as used in the assessment. These FLR package are
automatically installed into a local library when running the TAF
assessment (see below) or by calling

``` r
library(icesTAF)
taf.boot()
```

Alternatively, they can be manually installed from GitHub with

``` r
remotes::install_github("flr/FLCore", ref = "93f94d0")
remotes::install_github("flr/FLAssess", INSTALL_opts = "--no-multiarch",
                         ref = "03d018d")
remotes::install_github("flr/FLXSA", INSTALL_opts = "--no-multiarch",
                         ref = "d5cfce1")
remotes::install_github("flr/FLash", INSTALL_opts = "--no-multiarch",
                         ref = "1008c04")
```

The latest release versions are available from
<http://www.flr-project.org/>:

``` r
install.packages(pkgs =  c("FLCore", "FLAssess", "FLXSA", "FLash", "ggplotFL"), 
                 repos = "http://flr-project.org/R")
```

Please note, on Windows `FLAssess`, `FLXSA` and `FLash` can only be
installed and used in the 64-bit version of `R` (previously they only
worked on 32-bit).

## Running the assessment

The easiest way to run the assessment is to clone or download this
repository and run:

``` r
### load the icesTAF package
library(icesTAF)
### load data and install R packages
taf.boot()
### run all scripts
source.all()
```

This code snippet runs the assessment and forecast and creates the
tables and figures presented in the WG report.

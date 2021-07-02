
<!-- README.md is generated from README.Rmd. Please edit that file -->

# protGear

<!-- badges: start -->
<!-- badges: end -->

The goal of protGear is to …

## Installation

You can install the released version of protGear from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("protGear")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Keniajin/protGear")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(protGear)
#> Loading required package: limma
#> Loading required package: magrittr
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.0.5
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.0.5
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: tidyr
#> 
#> Attaching package: 'tidyr'
#> The following object is masked from 'package:magrittr':
#> 
#>     extract
#> Loading required package: data.table
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> Loading required package: ggpubr
#> Loading required package: gtools
#> Warning: package 'gtools' was built under R version 4.0.5
#> Loading required package: gridExtra
#> 
#> Attaching package: 'gridExtra'
#> The following object is masked from 'package:dplyr':
#> 
#>     combine
#> Loading required package: tibble
#> Warning: package 'tibble' was built under R version 4.0.5
#> Loading required package: rlang
#> Warning: package 'rlang' was built under R version 4.0.5
#> 
#> Attaching package: 'rlang'
#> The following object is masked from 'package:gtools':
#> 
#>     chr
#> The following object is masked from 'package:data.table':
#> 
#>     :=
#> The following object is masked from 'package:magrittr':
#> 
#>     set_names
#> Loading required package: rmarkdown
#> Warning: package 'rmarkdown' was built under R version 4.0.5
#> Loading required package: knitr
#> Warning: package 'knitr' was built under R version 4.0.5
#> Loading required package: vsn
#> Loading required package: Biobase
#> Loading required package: BiocGenerics
#> Loading required package: parallel
#> 
#> Attaching package: 'BiocGenerics'
#> The following objects are masked from 'package:parallel':
#> 
#>     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
#>     clusterExport, clusterMap, parApply, parCapply, parLapply,
#>     parLapplyLB, parRapply, parSapply, parSapplyLB
#> The following object is masked from 'package:gridExtra':
#> 
#>     combine
#> The following objects are masked from 'package:dplyr':
#> 
#>     combine, intersect, setdiff, union
#> The following object is masked from 'package:limma':
#> 
#>     plotMA
#> The following objects are masked from 'package:stats':
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from 'package:base':
#> 
#>     anyDuplicated, append, as.data.frame, basename, cbind, colnames,
#>     dirname, do.call, duplicated, eval, evalq, Filter, Find, get, grep,
#>     grepl, intersect, is.unsorted, lapply, Map, mapply, match, mget,
#>     order, paste, pmax, pmax.int, pmin, pmin.int, Position, rank,
#>     rbind, Reduce, rownames, sapply, setdiff, sort, table, tapply,
#>     union, unique, unsplit, which, which.max, which.min
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: 'Biobase'
#> The following object is masked from 'package:rlang':
#> 
#>     exprs
#> Warning: replacing previous import 'data.table::last' by 'dplyr::last' when
#> loading 'protGear'
#> Warning: replacing previous import 'data.table::first' by 'dplyr::first' when
#> loading 'protGear'
#> Warning: replacing previous import 'data.table::between' by 'dplyr::between'
#> when loading 'protGear'
#> Warning: replacing previous import 'magrittr::set_names' by 'purrr::set_names'
#> when loading 'protGear'
#> Warning: replacing previous import 'data.table::transpose' by 'purrr::transpose'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::list_along' by 'rlang::list_along'
#> when loading 'protGear'
#> Warning: replacing previous import 'gtools::chr' by 'rlang::chr' when loading
#> 'protGear'
#> Warning: replacing previous import 'purrr::invoke' by 'rlang::invoke' when
#> loading 'protGear'
#> Warning: replacing previous import 'purrr::flatten_raw' by 'rlang::flatten_raw'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::modify' by 'rlang::modify' when
#> loading 'protGear'
#> Warning: replacing previous import 'purrr::as_function' by 'rlang::as_function'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::flatten_dbl' by 'rlang::flatten_dbl'
#> when loading 'protGear'
#> Warning: replacing previous import 'data.table:::=' by 'rlang:::=' when loading
#> 'protGear'
#> Warning: replacing previous import 'purrr::flatten_lgl' by 'rlang::flatten_lgl'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::flatten_int' by 'rlang::flatten_int'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::%@%' by 'rlang::%@%' when loading
#> 'protGear'
#> Warning: replacing previous import 'purrr::flatten_chr' by 'rlang::flatten_chr'
#> when loading 'protGear'
#> Warning: replacing previous import 'purrr::splice' by 'rlang::splice' when
#> loading 'protGear'
#> Warning: replacing previous import 'purrr::flatten' by 'rlang::flatten' when
#> loading 'protGear'
#> Warning: replacing previous import 'purrr::prepend' by 'rlang::prepend' when
#> loading 'protGear'
#> Warning: replacing previous import 'flexdashboard::valueBox' by
#> 'shinydashboard::valueBox' when loading 'protGear'
#> Warning: replacing previous import 'flexdashboard::renderValueBox' by
#> 'shinydashboard::renderValueBox' when loading 'protGear'
#> Warning: replacing previous import 'flexdashboard::valueBoxOutput' by
#> 'shinydashboard::valueBoxOutput' when loading 'protGear'
#> Warning: replacing previous import 'magrittr::extract' by 'tidyr::extract' when
#> loading 'protGear'
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

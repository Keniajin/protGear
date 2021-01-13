### General information
protGear is a package for protein micro data processing just before the main analysis. 

The package loads the '`gpr`' or '`txt`' file format extracted by the quantification software and merges this with the specific sample identifiers. The package processes multiple files extracted in a batch with their corresponding sample identifier file. The sample identifier file has 2 variables '`v1`' and '`v2`' which indicate the mini-array or block number and sample identifier respectively. The '`gpr`' file and the corresponding sample identifier file have the same file name.  protGear also provides a web based $Shiny^{(R)}$ platform for real time visualization of the data processing. 

There is a package vignette included with this package with a step by step use of the package. 

## Installing the package

``` r
library(devtools)
devtools::install_github("keniajin/protGear/")
```

## To run the shiny application
To use the application locally on your compute install R and launch the app using the following command. 
`protGear::launch_protGear_interactive()`

To Install R: 
 - Open an Internet browser and go to www.r-project.org. 
 - Under "Getting Started" click the "download R" link
 - Select a CRAN location (a mirror site) and click the corresponding link.
 - Download R depending on your operating system. 


## Contribute

Check the Github page for [source 
code](https://github.com/Keniajin/protGear/).
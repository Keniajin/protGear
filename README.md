### General information
protGear is a package for protein micro data processing just before the main analysis. 

The package loads the '`gpr`' or '`txt`' file format extracted by the quantification software and merges this with the specific sample identifiers. The package processes multiple files extracted in a batch with their corresponding sample identifier file. The sample identifier file has 2 variables '`v1`' and '`v2`' which indicate the mini-array or block number and sample identifier respectively. The '`gpr`' file and the corresponding sample identifier file have the same file name.  protGear also provides a web based $Shiny^{(R)}$ platform for real time visualization of the data processing. 

There is a package vignette included with this package with a step by step use of the package. 

## Installing the package
`library(devtools)`
`devtools::install_github("keniajin/protGear/")`

## To run the shiny application

`protGear::launch_protGear_interactive()`



### General information
protGear is a package for protein micro-array data processing just before the main analysis. 

The package loads the '`gpr`' or '`txt`' file format extracted by the quantification software and merges this with the specific sample identifiers. The package processes multiple files extracted in a batch with their corresponding sample identifier file. The sample identifier file has 2 variables '`v1`' and '`v2`' which indicate the mini-array or block number and sample identifier respectively. The '`gpr`' file and the corresponding sample identifier file have the same file name.  protGear also provides a web based $Shiny^{(R)}$ platform for real time visualization of the data processing. 

There is a package vignette included with this package with a step by step use of the package. Check https://keniajin.github.io/protGear/ under articles. 

## Installing the package

``` r
library(remotes) 

remotes::install_github("Keniajin/protGear/")
```

## To run the shiny application
To use the application locally on your compute install R and launch the app using the following command. 
`protGear::launch_protGear_interactive()`

To Install R: 
 - Open an Internet browser and go to www.r-project.org. 
 - Under "Getting Started" click the "download R" link
 - Select a CRAN location (a mirror site) and click the corresponding link.
 - Download R depending on your operating system. 

## protGear help pages

- The main page - https://keniajin.github.io/protgear/ 
- Vignette - https://keniajin.github.io/protGear/articles/protGear_vignette.html

## Contribute

Check the Github page for [source 
code](https://github.com/Keniajin/protGear/).


## Publication 

Mwai K, Kibinge N, Tuju J, Kamuyu G, Kimathi R, Mburu J, Chepsat E, Nyamako L, Chege T, Nkumama I, Kinyanjui S. et al. , protGear: A protein microarray data pre-processing suite. Computational and Structural Biotechnology Journal. 2021 Apr 24.

DOI: https://doi.org/10.1016/j.csbj.2021.04.044 

@article{mwai2021protgear,
  title={protGear: A protein microarray data pre-processing suite},
  author={Mwai, Kennedy and Kibinge, Nelson and Tuju, James and Kamuyu, Gathoni and Kimathi, Rinter and Mburu, James and Chepsat, Emily and Nyamako, Lydia and Chege, Timothy and Nkumama, Irene and others},
  journal={Computational and Structural Biotechnology Journal},
  year={2021},
  publisher={Elsevier}
}

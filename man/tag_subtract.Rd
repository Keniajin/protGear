% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_estimation_tag_functions.R
\name{tag_subtract}
\alias{tag_subtract}
\title{tag_subtract}
\usage{
tag_subtract(
  dataC_mfi,
  tag_antigens,
  mean_best_CV_var,
  tag_file,
  batch_vars,
  sampleID_var = "sampleID",
  antigen_var = "antigen"
)
}
\arguments{
\item{dataC_mfi}{A dataframe}

\item{tag_antigens}{A character vector with the names of proteins or antigens used as TAG.}

\item{mean_best_CV_var}{A character string containing the identifier of the variable with the MFI values.}

\item{tag_file}{A data frame with variables \code{antigen, TAG, TAG_name } to show the TAG for the different antigens or proteins in dataC_mfi}

\item{batch_vars}{A list of characters identifying variables in dataC_mfi for indicating  batch.}

\item{sampleID_var}{A character string containing the name of the sample identifier variable. Default set to 'sampleID'}

\item{antigen_var}{A character string containing the name of the features/protein variable. Default to 'antigen'}
}
\value{

}
\description{
\\\_End_Function_\\\         #
}
\details{
Subtract the purification TAG data
}

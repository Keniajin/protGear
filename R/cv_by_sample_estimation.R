#' Summarise CV by samples
#' @title cv by sample
#'
#' @param cv_variable A character string containing the identifier of the variable with CV values.
#' @param lab_replicates A numeric value indicating the number of lab replicates.
#' @param  sampleID_var A character string containing the name of the sample identifier variable. Default set to 'sampleID'
#' @param dataCV A dataframe
#' @import dplyr tidyr
#' @importFrom tidyr gather
#' @description A function to give the summary of the CV's by the sampleID
#' @return A data frame of CV calculated by sample
#' @export
#'
#' @examples
#' dataC <- readr::read_csv(system.file("extdata", "dataC.csv", package="protGear"))
#' ## this file has 3 lab replicates and the default names
#' dataCV <- cv_estimation(dataC  ,lab_replicates=3)
#' cv_by_sample_estimation(dataCV, cv_variable = "cvCat_all", lab_replicates = 3)
cv_by_sample_estimation <- function(dataCV,cv_variable,lab_replicates, sampleID_var='sampleID'){
  ## creating a summary of the CV's by sampleID for each file
  ## helps in identifying samples with a high CV value
  if(lab_replicates>1){
    iden <- unique(dataCV$iden)
    dataC_cvSample <-  dataCV %>%
      group_by_at(c(sampleID_var, cv_variable)) %>%
      summarise(n= n()) %>%
      mutate(perc = round((n / sum(n))*100,2)) %>%
      #rename(cvCat='get(cv_variable)') %>%
      ungroup() %>%
      gather(variable, value, -c(sampleID_var, cv_variable)) %>%
      unite(temp, !!(cv_variable), variable) %>%
      tidyr::spread(temp, value, fill=0)
  }else{
    dataC_cvSample <- NULL
    warning("The experiment is specified not to have lab replicates")
  }

  return(dataC_cvSample)
}


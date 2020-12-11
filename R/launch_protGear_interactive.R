#' launch_protGear_interactive
#'
#' This is Function is to launch the shiny application
#' @return launches the app
#' @export
#' @examples


launch_protGear_interactive <- function() {
  appDir <- system.file("shiny-examples", "protGear_interactive","protGear_interactive.Rmd", package = "protGear")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `protGear`.", call. = FALSE)
  }

  rmarkdown::run(file = appDir)
}

#' launch_select
#'
#' This is Function is to launch mutiple shiny applications for protGear
#' @param theApp accepts one of the folders containing the shiny appplication
#' @return launches the app you select
#' @export
#' @examples
#' launch_select()

launch_select <- function(theApp) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "protGear"))

  validExamplesMsg <-
    paste0(
      "The available apps in `protGear` are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid folder is given, throw an error
  if (missing(theApp) || !nzchar(theApp) ||
      !theApp %in% validExamples) {
    stop(
      'Please run `launch_select()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", theApp,package = "protGear")
  rmarkdown::run(file=paste0(appDir,"/","index.Rmd" ))
}

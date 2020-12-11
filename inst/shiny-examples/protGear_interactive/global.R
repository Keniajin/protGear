library(shiny)
library(shinydashboard)


# HELP & INTRO DATA ---------------------------------------------------------------

steps <- readr::read_csv2("help.csv")
#intro <- read_csv2("intro.csv")


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}
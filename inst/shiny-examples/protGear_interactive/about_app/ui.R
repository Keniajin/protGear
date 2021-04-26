library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("About protGear"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("protGear is available on Github, so you can install it in the usual way
        from your R console:"),
      code('remotes::install_github("Keniajin/protGear")'),
      br(),
      br(),
      br(),
      br(),
      img(src = "protGear_logo.png"),
      br(),
      "protGear is a developed by ",
      span(a("Keniajin",href = "http://keniajin.com"), style = "color:blue")
    ),
    mainPanel(
      h1("Introducing protGear"),
      p("protGear is a package for protein micro-array data processing just
        before the main analysis. ",
        em("one-stop-shop  "),
        "pre-processing suite for protein microarrays that is compatible
        with data from the major protein microarray scanners."),
      br(),
      p("For an introduction examples, visit the ",
        a("protGear page.",
          href = "https://keniajin.github.io/protGear/")),
      br(),
      h2("Functionalities"),
      p("- Background correction"),
      p("- Normalization"),
      p("- Principal component Analysis"),
      p("- Heatmaps of normalised data")
    )
  )
)




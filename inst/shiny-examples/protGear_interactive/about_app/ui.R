library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("About protGear"),
  sidebarLayout(
    sidebarPanel(
      h2("Installation"),
      p("protGear is available on Github, so you can install it in the usual way from your R console:"),
      code('install_github("keniajin/protGear")'),
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
      p("Shiny is a new package from RStudio that makes it ", 
        em("incredibly easy "), 
        "to build interactive web applications with R."),
      br(),
      p("For an introduction and live examples, visit the ",
        a("Shiny homepage.", 
          href = "http://shiny.rstudio.com")),
      br(),
      h2("Features"),
      p("- Build useful web applications with only a few lines of code—no JavaScript required."),
      p("- Shiny applications are automatically 'live' in the same way that ", 
        strong("spreadsheets"),
        " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
    )
  )
)




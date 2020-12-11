library(shiny)
library(shinyFiles)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Example - Shiny Files Buttons Use"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("First press this ShinyDir Button to locate an existing folder and set
        it to be your new working directory"),
      shinyDirButton("folderChoose","Choose Folder","Choose working directory"),
      
      p("In this second step, press the ShinyFiles Button that should point to the 
        volumes (Just as the previous button does). In my R version, this is where it 
        renders a white interface (see attached picture"),
      shinyFilesButton("filesChoose1","Files Chooser 1","Choose your files",
                       multiple=TRUE),
      
      p("In this third button, pI owuld like to link the ShinyFiles interface with
        the selected folder in button 1. This part I have not been able to solve"),
      shinyFilesButton("filesChoose2","Files Chooser 2","Choose your files",
                       multiple=TRUE)
      
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("path")
    )
      )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  volumes = getVolumes()
  shinyDirChoose(input, "folderChoose", roots = volumes, session = session)
  sel_path <- reactive({return(print(parseDirPath(volumes, input$folderChoose)))})
  shinyFileChoose(input, "filesChoose1", roots = volumes, session = session)
  
  
  setWorkingDir<-eventReactive(input$folderChoose,{
    setwd(sel_path())
  })
  
  output$path<-renderText(sel_path())
}

# Run the application 
shinyApp(ui = ui, server = server)
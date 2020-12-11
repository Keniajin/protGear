#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyFiles)
library(flexdashboard)
library(shinydashboard)
library(dplyr)

shinyUI(
  navbarPage(
    theme = shinytheme("sandstone") ,
    "Data load and processing",
    tabPanel(
      "Load Data",
      # Panel title
      titlePanel("Data Overview"),
      #The shiny app is made up of two columns: the first column houses all of the user interface,
      column(
        3,
        wellPanel(
          # A drop down menu to choose the folder where the data is
          # gets the path of the data directory
          shinyFiles::shinyDirButton("folderChoose",
                                     "Chose directory with the data",
                                     "Upload"),
          #A line break to make the interface clearer
          br(),
          
          # A row with two columns: one to choose the location type, and one to choose a plot type.
          fluidRow(column(12,
                          numericInput(
                            "num",
                            h3("Input the number of mini arrays"),
                            value = 2
                          ))),
          ## close fluidRow
          
          fluidRow(column(12,
                          numericInput(
                            "num",
                            h3("Input the number total number of samples "),
                            value = 2
                          ))),
          ## close fluid row
          fluidRow(column(12, uiOutput("dir")))## close fluid row
        )## close well panel
      ),
      
      mainPanel(
        column(9, ##open main view column 
          fluidRow(
            column(4,  #gauge1
               div(
                   # valueBoxOutput("vbox1"  , width = 2),
                   #flexdashboard::valueBoxOutput("files_in_batch")
                 shinydashboard::valueBoxOutput("vbox1")
                   )),
            column(4,  #gauge2
                   div(
                       # valueBoxOutput("vbox1"  , width = 2),
                       #flexdashboard::valueBoxOutput("files_in_batch"),
                       gauge(4, min = 0, max = 100, gaugeSectors(
                         success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                       )))),
               column(4, 
                      div(
                          # valueBoxOutput("vbox1"  , width = 2),
                          #flexdashboard::valueBoxOutput("files_in_batch"),
                          gauge(90, min = 0, max = 100, gaugeSectors(
                            success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
                          ))))
            ),        
              
            div(class="span6" , "Yearly and Monthly Admissions", 
                                   plotOutput('data_process', width="100%"),
                                    style = "height:80%;background-color: yellow;")
        )#,## close column of the main panel
    
        
      )), ## end main Panel and tabs panel
    tabPanel(
      "Background Plots",
      
      # Application title
      titlePanel("CV calculation"),
      
      #The shiny app is made up of two columns: the first column houses all of the user interface,
      #including disease name selection, location type, location name, and plot options.
      column(
        3,
        wellPanel(
          # A drop down menu to choose the disease name of interest.
          # Reads in disease names from disease_names.txt.  Can choose which name to default to.
          selectInput(
            'disease_name',
            'Disease Name',
            as.character(levels(mtcars$am)),
            selected = "Salmonellosis"
          ),
          
          # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
          # The checkbox is defined in the server.R file
          dateRangeInput(
            'years',
            'Choose date range',
            start = "2015-01-01",
            end = Sys.Date(),
            min = "2008-01-01",
            max = Sys.Date()
          ),
          
          #uiOutput("location"),
          
          
          #A line break to make the interface clearer
          br(),
          
          # A row with two columns: one to choose the location type, and one to choose a plot type.
          fluidRow(column(
            3,
            radioButtons(
              "locty",
              "Location Type",
              c(
                "State" = "state",
                "Single region" = "region",
                "All states within a region" =
                  "stregion",
                "All regions" =
                  "aregion",
                "Country" = "country"
              ),
              selected = "aregion"
            )
          ),
          column(
            3, radioButtons(
              "plotty",
              "Plot Type",
              c(
                "Weekly data" = "week",
                "Weekly data by year" = "weeky",
                "Cumulative data by year" = "cumuy"
              ),
              selected = "week"
            )
          )),
          
          # A row with some plot options. uiOutput("frees") creates a checkbox for
          # whether the y-axis scale should be the same for all plots.
          # This checkbox only appears for certain location type selections, and is defined in the server.R file.
          fluidRow()
        )
      ),
      
      
      
      # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
      h4("output$dir")
    ),
    tabPanel(
      "Buffer Spots",
      # Information about data collection.
      "Data are updated weekly on Thursday at 20:00 CT.",
      br(),
      br(),
      "Please visit",
      a("this site", href = "http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
      "for more information on how the data were collected.  All data are provisional.",
      br(),
      br(),
      a("See the code", href = "https://github.com/NLMichaud/WeeklyCDCPlot"),
      br(),
      br(),
      "Any questions or comments can be sent to",
      br(),
      "Aaron Kite-Powell: " ,
      a("akitepowell@gmail.com", href = "mailto:akitepowell@gmail.com"),
      br(),
      "Nick Michaud: ",
      a("michaud@iastate.edu", href = "mailto:michaud@iastate.edu")
    ),
    tabPanel(
      "Technical Repeats",
      # Information about data collection.
      "Data are updated weekly on Thursday at 20:00 CT.",
      br(),
      br(),
      "Please visit",
      a("this site", href = "http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
      "for more information on how the data were collected.  All data are provisional.",
      br(),
      br(),
      a("See the code", href = "https://github.com/NLMichaud/WeeklyCDCPlot"),
      br(),
      br(),
      "Any questions or comments can be sent to",
      br(),
      "Aaron Kite-Powell: " ,
      a("akitepowell@gmail.com", href = "mailto:akitepowell@gmail.com"),
      br(),
      "Nick Michaud: ",
      a("michaud@iastate.edu", href = "mailto:michaud@iastate.edu")
    ),
    tabPanel(
      "More Information",
      # Information about data collection.
      "Data are updated weekly on Thursday at 20:00 CT.",
      br(),
      br(),
      "Please visit",
      a("this site", href = "http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
      "for more information on how the data were collected.  All data are provisional.",
      br(),
      br(),
      a("See the code", href = "https://github.com/NLMichaud/WeeklyCDCPlot"),
      br(),
      br(),
      "Any questions or comments can be sent to",
      br(),
      "Aaron Kite-Powell: " ,
      a("akitepowell@gmail.com", href = "mailto:akitepowell@gmail.com"),
      br(),
      "Nick Michaud: ",
      a("michaud@iastate.edu", href = "mailto:michaud@iastate.edu")
    )
  ) )
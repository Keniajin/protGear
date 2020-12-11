shinyUI(
  dashboardPage(title="Data load and processing", 
  dashboardHeader(title="xx"),
  dashboardSidebar(width="30%",
    

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
                          h4("Input the number of mini arrays"),
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
        fluidRow(column(12, uiOutput("dir"))),## close fluid row

   
    sidebarMenu(
     # menuItem("Dashboard", tabName = "dashboard"),
      #menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("vbox1")
                #valueBoxOutput("count"),
                #valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Popularity by package (last 5 min)",
                  bubblesOutput("data_process", width = "100%", height = 600)
                ),
                box(
                 # width = 4, status = "info",
                 # title = "Top packages (last 5 min)",
                 # tableOutput("packageTable")
                )
              )
      ),
      tabItem("rawdata"#,
              #numericInput("maxrows", "Rows to show", 25),
              #verbatimTextOutput("rawtable"),
             # downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)
)
shinyUI(
  dashboardPage(#theme = shinytheme("sandstone") ,
    dashboardHeader(title = "Menu Select"),
    dashboardSidebar(width="20%",
                     column(12,
      shinyFiles::shinyDirButton("folderChoose",
                                 "Chose directory with the data",
                                 "Upload"),
      br(),
      uiOutput("batch_select"),
      h4("This will just show the folder named +sampleID+"),
      uiOutput("sampleID_select"),
      br(),
       sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard"),
        menuItem("Raw data", tabName = "rawdata")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem("dashboard",
                # this Row picks the parametes
                fluidRow(
                  column(2,
                         uiOutput('channel_output')
                         ),
                  column(2,
                         uiOutput('total_samples_output')
                  ), 
                  column(2,
                            uiOutput('blockspersample_output')
                  ),
                  column(2,
                         uiOutput('mig_prefix_output')
                  ),
                  column(2,
                         uiOutput('machine_output')
                  ),
                  column(2,
                         uiOutput('date_process_output')
                  )
                  
                ),
                fluidRow(
                  column(3,
                  uiOutput("files_in_batch")
                  ),
                  column(3,
                  uiOutput("check_sampleID_out")  
                    ),
                  column(3,
                    uiOutput("antigens_count")  
                  ),
                  column(3,
                    uiOutput("blocks_count")  
                  )
                ),
                fluidRow(
                  box(
                 #   width = 8, status = "info", solidHeader = TRUE,
                  #  title = "Popularity by package (last 5 min)",
                  #  bubblesOutput("packagePlot", width = "100%", height = 600)
                    div(class="span6" , "Buffer spots overview", 
                        plotOutput('data_process', width="100%"),
                        style = "height:80%;background-color: aqua;")
                  ),
                  box(
                  div(class="span6" , "Data overview", 
                        plotlyOutput('data_box', width="100%"),
                        style = "height:80%;background-color: aqua;"),
                  # slide bar for the 
                  sliderInput("slider_antigen", label = h3("Select the antigen range to view"), min = 0, 
                              max = 120, value = c(40, 60))
                  )
                )
        ),
        tabItem("rawdata",
              #  numericInput("maxrows", "Rows to show", 25),
                #verbatimTextOutput("rawtable"),
                #downloadButton("downloadCsv", "Download as CSV")
              DT::DTOutput("tbl")
        )
      )
    )
  )
)
shinyUI(
  navbarPage("Data process -->",id = 'all_tabs',
             theme = shinytheme("flatly"),

             # shinytheme("sandstone"),
             #'@_______________Background_and_structure____________________________________
             #'
             tabPanel("Array design structure",
                      dashboardPage(#theme = shinytheme("sandstone") ,
                        dashboardHeader(),
                        dashboardSidebar(width="20%",
                                         column(12,
                                                br(),
                                                h4("Select one of the .gpr/text file"),
                                                p(".gpr file extension was developed by Molecular Devices \n and is used for files created using the GenePix Pro Software"),
                                                uiOutput("gpr_file_load"),
                                                br(),
                                                sidebarMenu(id="structure_view",
                                                            menuItem("Structure overview",
                                                                     tabName = "structure", icon = icon("dashboard")),
                                                            menuItem("Overview image",
                                                                     tabName = "structure_img", icon = icon("dashboard")),
                                                            menuItem("Spatial structure",
                                                                     tabName = "spatial_struct", icon = icon("dashboard"))

                                                )## end sidebar menu
                                          )),## end dashbaord side bar
                        dashboardBody(
                          shinyjs::useShinyjs(),
                          # add custom JS code
                          extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }",functions = c("hidehead")),
                          tabItems(
                            tabItem("structure",
                                    column(9,
                                           fluidRow(
                                             column(4,
                                                    uiOutput("wavelength_struct")
                                             )),
                                    fluidRow(
                                      column(3,class='my_textinput',
                                             uiOutput("total_miniarray_struct")
                                      ),
                                      column(3,class='my_textinput',
                                             uiOutput("blockspersample_output")
                                      ),
                                      column(3,class='my_textinput',
                                             uiOutput("select_F_var")
                                      ),
                                      column(3,class='my_textinput',
                                             uiOutput("select_B_var")
                                      )
                                    ),
                                    fluidRow(
                                      box(width="100%",
                                          div(class="span6" , "Structure of the array map",
                                              plotOutput('structure_plot', width="100%",height = "500px"),
                                              style = "height:80%;background-color: aqua;")
                                      ))
                                    ),## end column 1
                                   column(3,
                                          h4(strong("Click here for", style = "font-family: 'times'; font-si16pt")),
                                          h4(menuItem("Demonstration of an array map structure",
                                                      tabName = "structure_img", icon = icon("dashboard")),
                                             style = "list-style-type: none;"),
                                          h4(strong("The image on the left shows the structure of array", style = "font-family: 'times'; font-si16pt")),
                                          p(strong("Blocks"),"The number blocks per mini array might vary as per the design of the experiment. A mini-array holds data
                                          for a single sample with unique features. The blocks in the same mini-array are grouped in their respective mini-array."),
                                          p(strong("Samples"),"Each slide must have a respective plate map or sampleID file. The sampleID file shows which sample is
                                            in each mini-array")
                                   )
                            ),## end of structure tabItem
                            tabItem("structure_img",
                                    img(src='image_slide.png', align = "center")
                            ),## end of structure_img tabItem
                            tabItem("spatial_struct",
                                  fluidRow(
                                      column(3#,class='my_textinput',
                                           #  uiOutput("select_spatial_var")
                                      ),
                                      column(3,class='my_textinput',
                                             uiOutput("select_spatial_var")
                                      ),
                                      column(3,class='my_textinput',
                                             uiOutput("select_spatial_type")
                                      ),
                                      column(3#,class='my_textinput',
                                            # uiOutput("select_B_var")
                                      )
                                  ),
                                  br(),
                                 fluidRow(
                                   column(12,
                                      # div(class="span6" , "Spatial visualization of the array MFI's",
                                      br(),
                                           conditionalPanel(
                                             condition = "input.spatial_type == 'point'",
                                             plotlyOutput('spatial_structure_plot', width="100%",height = "100%")
                                           ),
                                           conditionalPanel(
                                             condition = "input.spatial_type == '2d_array'",
                                             plotOutput('spatial_structure_plot_2d')
                                           ), style = "height:80%;background-color: aqua;")
                                  # )#div close
                                   )
                            )## end of spatial_struct tabItem
                        )## end tab items
                        )## end dashboard body
                      )## end dashboard page
             ),## end tab panel
             #'@_______________Data_load_and_processing____________________________________
             tabPanel("Data load and processing",
                      dashboardPage(#theme = shinytheme("sandstone") ,
                        dashboardHeader(),
                        dashboardSidebar(width="20%",
                                         column(12,
                                                shinyjs::useShinyjs(),
                                                uiOutput('dir_files'),
                                                br(),
                                                uiOutput("batch_select"),
                                                h4("This will just show the folder named +sampleID+"),
                                                uiOutput("sampleID_select"),
                                                br(),
                                                sidebarMenu(id="data_load_tabs",
                                                  menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
                                                  menuItem("Raw data", tabName = "rawdata", icon = icon("th"))
                                                )
                                         )),
                        dashboardBody(
                          shinyjs::useShinyjs(),
                          # add custom JS code
                          #js_file <- ,
                          #extendShinyjs("www/app.js"),
                          extendShinyjs(system.file("shiny-examples/protGear_interactive/www",
                                                    "app.js", package="protGear"),
                                        functions = c('hidehead','hideSidebar')),
                          tabItems(
                            tabItem("dashboard",
                                    # this Row picks the parametes
                                    fluidRow(
                                      column(1#,
                                             #uiOutput('blockspersample_output')
                                      ),
                                      column(3,
                                             uiOutput('channel_output')
                                      ),
                                      column(3,
                                          uiOutput('total_samples_output')
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
                                            plotOutput('data_process', width="100%")%>% withSpinner(color="#0dc5c1"),
                                            style = "height:80%;background-color: aqua;")
                                      ),
                                      box(
                                        div(class="span6" , "Data overview",
                                            plotlyOutput('data_box', width="100%")%>% withSpinner(color="#0dc5c1"),
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
                                    downloadButton("download_Raw_Data", "Download raw data"),
                                    br(),
                                    DT::DTOutput("tbl_all_data") %>% withSpinner(color="#B80f0A")

                            )
                          )
                        )
                      )
             ),

             ### Background correction
             #'@_______________Background_correction____________________________________
             #'
             tabPanel("Background Correction",
                      dashboardPage(#theme = shinytheme("sandstone") ,
                        dashboardHeader(),
                        dashboardSidebar(width="20%",
                                         column(12,

                                                br(),
                                                h4("Select the slide to view the background"),
                                                uiOutput('slide_names_bg'),
                                                br(),
                                                uiOutput('bg_correction_select_1'),
                                                br(),
                                                sidebarMenu(id="bg_tabs",
                                                  menuItem("Background overview",
                                                           tabName = "bg_graphs", icon = icon("dashboard")),
                                                  menuItem("Perform background correction",
                                                           tabName = "plot_correct", icon = icon("th")),
                                                  menuItem("Foreground and Background plot",
                                                           tabName = "fg_bg_plot", icon = icon("th"))
                                                )
                                         )),## end dashbaord side bar
                        dashboardBody(width='80%',
                          shinyjs::useShinyjs(),
                          # add custom JS code
                          extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }",functions = c("hidehead")),
                        tabItems(
                            tabItem("bg_graphs",
                                    fluidRow(
                                      column(1#,

                                      ),
                                      column(3,
                                             div(class="radioSelect",
                                                 #h4("  "),
                                                 uiOutput("select_log_MFI",height="auto")
                                             )
                                      ),
                                      column(3,
                                             div(class="radioSelect",
                                                 #h4(""),
                                                 uiOutput("select_block_antigen",height="auto"))
                                      )),
                                    fluidRow(
                                      column(8,
                                             div(class="span6" , textInput("value_block",""), #paste0("Background plot by ",  uiOutput('value_block')),
                                                 plotlyOutput('bg_plots')%>% withSpinner(color="#0dc5c1"),
                                                 style = "height:800px;background-color: white;width:auto;")
                                             ),
                                      column(4,
                                       p("We have implement different approaches of background correction."),
                                      h2(strong("The implemented approaches are", style = "font-family: 'times'; font-si16pt")),
                                      p(strong("- Local bakground subtraction"),"This method involves subtracting the median of the background pixel
                                        intensities from the foreground signal of a specific spot.  Local background subtraction from the foreground
                                        intensity debatably gives in principle an unbiased estimator of the true signal since it is arguably an
                                        unbiased estimate of the local non-specific intensity "),
                                      p(strong("- Global background subtraction."),"Global background approach involves subtracting a single value for
                                        the background from each individual spot. The global value is the median of all the local background values for
                                        a given array. "),
                                      p(strong("- Moving minimum"),"The background intensities are replaced with the minimum of the local backgrounds
                                        in a given block/mini array. This is utilised mostly if there is a block artefact
                                        observed in the background intensities."),
                                      p(strong("- Half Moving minimum "),"Minimum-half involves a two-step background correction approach and it is
                                        designed to ensure only positive corrected intensities are produced.
                                        In the first place, the local background correction is implemented, then secondly any intensity which is zero or negative
                                        after local background subtraction is set equal to half the minimum of the positive corrected intensities in each block."),
                                      p(strong("- Log-linear background correction (Edwards)"),"The strategy involves adjusting values after local background subtraction to avoid negative intensities.
                                        Background subtraction is replaced by a smooth monotonic function when the difference between the foreground and the local background is
                                        large than a small threshold value "),
                                      p(strong("- Normal and exponential (normexp) background correction") ,"This approach corrects the background using a two mixture model of normal and exponential distribution for
                                        background signals and local background-subtracted foreground intensities respectively.")
                                )
                              )
                            ),## end of bg1 tabItem
                            tabItem("plot_correct",
                                    fluidRow(
                                      h2(strong("Select the BG correction option on the left. The red
                                              line is the zero line", style = "font-family: 'times'; font-si16pt")),
                                      column(12,
                                             div(class="span6" ,
                                                 plotOutput('bg_correct_graphs')%>% withSpinner(color="#0dc5c1"),
                                                 style = "height:800px;background-color: white;width:auto;")

                                      )
                                    )
                            ),## end of tabItem bg 2
                          tabItem("fg_bg_plot",
                              fluidRow(
                                column(12,
                                    plotlyOutput('fg_bg_plots')%>% withSpinner(color="#0dc5c1")

                                )
                              )
                            )## end of tabItem bg 3

                          )## end tab items
                        )## end dashboard body
                      )## end dashboard page
             ),## end tab panel

             ### Coefficient of variation plot
             #'@_______________Coefficient_of_variation____________________________________
             tabPanel("Coefficient of Variation (CV)",
                      dashboardPage(
                        dashboardHeader(),
                        dashboardSidebar(width="20%",
                                         column(12,

                                                sidebarMenu(id="cv_tabs",
                                                            menuItem("Background correction and CV",icon = icon("spinner"), tabName = "bg_correct_cv"),
                                                            #menuItem("Tag subtract", tabName = "tag_plots"),
                                                            menuItemOutput('tag_plots'),
                                                            menuItem("Download data", tabName = "download_bg_correct",icon = icon("download"))
                                                ),
                                                br(),
                                                helpText("Select the method of background correction"),
                                                uiOutput(class="bg_select" ,'bg_correction_select'),
                                                helpText("Select the button below if the proteins have purification TAGs"),
                                                uiOutput(class="btn_width" ,'tag_subtract_select'),
                                                uiOutput(class="bg_select" ,'tag_file_load'),
                                                uiOutput(class="drop_select" ,'tag_antigens_select'),
                                                hr(),
                                                uiOutput(class="bg_select" ,'cv_value_select'),
                                                uiOutput(class="bg_select" ,'replicates_select'),
                                                uiOutput(class="bg_select" ,'minimum_mfi_select')


                                         )),## end dashbaord side bar
                        dashboardBody(
                          tags$head(
                            # Include our custom CSS
                           # style_file <- ,
                            includeCSS(system.file("shiny-examples/protGear_interactive/www", "styles.css", package="protGear"))#"www/styles.css")
                          ),
                          shinyjs::useShinyjs(),
                          # add custom JS code
                          extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }",functions = c("hidehead")),
                          tabItems(
                            tabItem("bg_correct_cv",
                                    fluidRow(
                                      column(6,
                                             div(class="span6" , "Correlation of data replicates coloured by CV",
                                                 plotOutput('cv_corr_plot', width="100%") %>% withSpinner(color="#0dc5c1"),
                                                 style = "height:80%;background-color: aqua;")
                                      ),
                                      column(6,
                                             div(class="span6" , "Violin plots showing the distribution of CV",
                                                 plotOutput('cv_violin_plot', width="100%")%>% withSpinner(color="#0dc5c1"),
                                                 style = "height:80%;background-color: aqua;")
                                      )) ,
                                    fluidRow(
                                      column(6,
                                             div(class="span6" , "Correlation of best 2 replicates ",
                                                 plotOutput('cv_violin_plot_best2', width="100%")%>% withSpinner(color="#0dc5c1"),
                                                 style = "height:80%;background-color: aqua;")
                                      ),
                                      column(6,
                                             div(class="table_view" , h3("A table showing the CV errors using",textOutput('cv_ui', inline = TRUE),"% cut off"),
                                                 DT::dataTableOutput("sample_best2_reactive_tbl") %>% withSpinner(color="#B80F0A"),
                                                 style = "height:100%;background-color: white;")
                                      ))

                            ),## end of cv1 tabItem
                            tabItem("tag_plots",
                                    fluidRow(
                                      column(3,
                                             uiOutput('bg_correct_infobox')
                                             ),
                                      column(3,
                                             uiOutput('tag_infobox')
                                      ),
                                      column(3,
                                             uiOutput('cv_infobox')
                                      ),
                                      column(2,
                                             uiOutput('sample_ID_select'),
                                             hr()
                                             )
                                    ),
                                    fluidRow(
                                      column(6,
                                           div(class="span6" , "Box plot of the TAG antigens",
                                               plotOutput('tag_box', width="100%") %>% withSpinner(color="#0dc5c1"),
                                          style = "height:80%;background-color: aqua;")
                                    ),
                                    column(6,
                                          div(class="span6" , "Box plot of the TAG antigens",
                                          plotOutput('tag_box_sample', width="100%") %>% withSpinner(color="#0dc5c1"),
                                          style = "height:80%;background-color: aqua;")

                                    )
                                    ),
                                    fluidRow(class='bg_gray',
                                      column(2
                                            ),
                                      column(3,
                                             uiOutput('tag_antigen_radio_select')

                                      ),
                                      column(3,
                                                uiOutput('antigen_tag_specific_select')
                                            ),
                                      hr()
                                      ),

                                    fluidRow(
                                      column(12,

                                             div(class="span6" , "Box plot of the TAG antigens",
                                                 plotOutput('tag_antigens_box', width="100%") %>% withSpinner(color="#0dc5c1"),
                                                 style = "height:100%;")
                                      )
                                    )

                            ),## end of ttag_plots
                            tabItem("download_bg_correct",
                                    fluidRow( downloadButton("download_dataCV", "Download CV corrected data")),
                                    br(),
                                    fluidRow(
                                      #DT::DTOutput("tbl_data_cv")
                                      DT::dataTableOutput('tbl_data_cv')  %>% withSpinner(color="#B80f0A")
                                      )
                            )## end of download_bg_correct
                          )## end tab items
                        )## end dashboard body
                      )## end dashboard page
             ),## end tab panel
             #'@_______________Normalisation____________________________________
             #'
             tabPanel("Normalisation",value = "normalise_panel",
                      dashboardPage(#theme = shinytheme("sandstone") ,
                        dashboardHeader(),
                        dashboardSidebar(width="20%",
                                         column(12,
                                             sidebarMenu(id="norm_tabs",
                                                 menuItem("Normalisation", tabName = "normalisation"),
                                                # menuItemOutput('normalisation'),
                                                  menuItem("Compare normalisations", tabName = "norm2"),
                                                  menuItem("Array heatmap of normalised data", tabName = "heatmap_norm")
                                                )
                                         ),
                                         br(),
                                         helpText("Select the method of normalisation"),
                                         uiOutput(class="bg_select" ,'normalisation_select'),
                                         hr(),
                                         uiOutput(class="bg_select" ,'rlm_antigens_select'),
                                         #uiOutput(class="drop_select" ,'tag_antigens_select'),
                                         hr()
                                         ),## end dashbaord side bar
                        dashboardBody(
                          shinyjs::useShinyjs(),
                          # add custom JS code
                          extendShinyjs(text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }",functions = c("hidehead")),
                          tabItems(
                            tabItem("normalisation",
                                    fluidRow(
                                      column(3,
                                           uiOutput('normalisation_infobox')
                                      ),
                                      column(3,
                                             uiOutput('bg_correct_infobox2')
                                      ),
                                      column(3,
                                             uiOutput('tag_infobox2')
                                      ),
                                      column(3,
                                             uiOutput('cv_infobox2')
                                      )
                                    ),
                                    fluidRow(
                                      column(9,
                                      box(width="100%",
                                          div(class="span6" , "Normalisation",
                                              plotOutput('normalised_sd_plot', width="100%",height = "600px"),
                                              style = "height:80%;background-color: aqua;")
                                      )),
                                      column(3,
                                             p("We have implement different approaches of normalisation."),
                                             h2(strong("The implemented approaches are", style = "font-family: 'times'; font-si16pt")),
                                             p(strong("- Log 2 Normalisation"),"The matrix of background-corrected signal levels is log2 transformed."),
                                             p(strong("- VSN"),"This approach calibrates for sample-to-sample variations through shifting and scaling
                                               using asinh function"),
                                             p(strong("- Cyclic Loess Normalisation"),"Cyclic loess performs pairwise normalization on all distinct pairs of
                                             arrays utlising MA plot and local regression (LOESS) smoothing. MA plot in single-colour microarrays for a pair
                                             of arrays is the scatter plot of average log intensity values [A] from both arrays vs.
                                               difference in log expression values [M] of the same arrays"),
                                             p(strong("- Cyclic Loess and Log Normalisation"),"This approach performs Cyclic Loess normalisation after log transformaing the data"),
                                             p(strong("- Robust Linear Model Normalisation"),'RLM was specifically developed for protein micro arrays.
                                             It incorporates a set of control proteins in the mini arrays to be used as normalization factor using a robust linear models')#,
                                            # p("- Normexp background correction")
                                      )
                                      )
                            ),## end of Norm1 tabItem
                            tabItem("norm2",
                                fluidRow(
                                  column(4

                                  ),
                                  column(3,
                                    uiOutput('normalisation_drop_down')
                                  )
                                ),
                                fluidRow(
                                      box(width="100%",
                                          div(class="span6" , "Normalisation techniques comparison",
                                              plotOutput('mutiple_plot', width="100%",height = "600px"),
                                              style = "height:80%;background-color: aqua;")
                                  ))
                            ),## end of tabItem
                            tabItem("heatmap_norm",
                                    fluidRow(
                                      column(4

                                      ),
                                      column(3,
                                           uiOutput('select_heatmap')
                                      )
                                    ),
                                    fluidRow(
                                      box(width="100%",
                                          div(class="span6" , "Heatmap of normalised data",
                                              plotOutput('heatmap_normalised', width="100%",height = "600px"),
                                              style = "height:80%;background-color: aqua;")
                                      ))
                            )## end of tabItem
                          )## end tab items
                        )## end dashboard body
                      )## end dashboard page
             ),## end tab panel
             #tags$li(actionLink("openModal", label = "", icon = icon("info")),class = "dropdown"),
             tabPanel("Overview", icon = icon("question"))
            )## end Nav bar page
)### end shiny UI

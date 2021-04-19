#library(shinySignals)   # devtools::install_github("hadley/shinySignals")
#library(bubbles)        # devtools::install_github("jcheng5/bubbles")


## document -->http://www.cryer.co.uk/file-types/a/atf/genepix_file_formats.htm
## http://www.cryer.co.uk/file-types/a/atf/genepix_file_formats.htm#example

## this loads packages from CRAN
pacman::p_load(shiny,shinyFiles,DT,tidyverse,shinydashboard,shinyjs,
               factoextra,FactoMineR)

## this loads or installs packages from github
pacman::p_load_gh("hadley/shinySignals","jcheng5/bubbles","GuangchuangYu/ggplotify")



#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
shinyServer(function(input, output, session) {
  js$hidehead('none')
  wd_this <- getwd()
  volumes = getVolumes()()
  dir.create("processed_data")

  shinyFiles::shinyDirChoose(input, "folderChoose",  roots = volumes, #c(home = wd_this),
                 session = session)
## this works where
sel_path <- reactive({
    return(print(parseDirPath(volumes, input$folderChoose)))
})

#'@_______________structure_of_array_data____________________________________
observe({ # called only once at app init
    updateTabItems(session, "structure_view", "structure")
})


#'@______________________load_intro_________________________
#show intro modal
observeEvent("", {
  showModal(modalDialog(
    #intro_html <- ,
    includeHTML(system.file("shiny-examples/protGear_interactive/", "intro_text.html", package="protGear" ,
                            mustWork = T)),
    easyClose = TRUE,
    footer = tagList(
      actionButton(inputId = "intro", label = "DISMISS (INTRODUCTION TOUR)", icon = icon("info-circle"))
    )
  ))
})


observeEvent(input$intro,{
  removeModal()
})

#'@_____________________________check_this___________________
# show intro tour
## to be activated
# observeEvent(input$intro,
#              ## check on radar example
#             # introjs(session, options = list("nextLabel" = "Continue",
#                                             # "prevLabel" = "Previous",
#                                             # "doneLabel" = "Alright. Let's go"))
#             showModal(
#               modalDialog(title = "Just clicked on intro tour!",
#                           p("Spanner in the works.Coming soon!!"))
#             )
#          #  shinyalert::shinyalert("Just clicked on intro tour!", "Spanner in the works.Coming soon!!", type = "warning")
# )


# use action buttons as tab selectors
update_all <- function(x) {
  updateSelectInput(session, "tab",
                    choices = c("Structure overview", "Overview image"),
                    label = "",
                    selected = x
  )
}

observeEvent(input$structure, {
  update_all("Structure overview")
})
observeEvent(input$structure_img, {
  update_all("Overview image")
})


# update confirm button

observeEvent(input$confirm, {
  updateButton(
    session,
    inputId = "confirm",
    label = "CONFIRM SELECTION",
    icon = icon("bar-chart-o"),
    style = "primary")
})

# hide the underlying selectInput in sidebar for better design
observeEvent("", {
  hide("tab")
})

# initiate reactive value storage
rv = reactiveValues()

observeEvent(input$all_tabs, {
  # store last tab
  rv$last_tab <- rv$current_tab
  rv$current_tab <- input$all_tabs

  if (input$all_tabs == "Overview") {
  showModal(
    modalDialog(title = "Overview",
                headerText = strong("What to do"),
                icon = icon("question"),
                badgeStatus = NULL,
                notificationItem(
                  text = (steps$text[1]),
                  icon = icon("spinner")
                ),
                br(),
                notificationItem(
                  text = steps$text[2],
                  icon = icon("address-card")
                ),
                br(),
                notificationItem(
                  text = steps$text[3],
                  icon = icon("calendar")
                ),
                br(),
                notificationItem(
                  text = steps$text[4],
                  icon = icon("user-md")
                ),
                br(),
                notificationItem(
                  text = steps$text[5],
                  icon = icon("ambulance")
                ),
                br(),
                notificationItem(
                  text = steps$text[6],
                  icon = icon("flask")
                ),
                br(),
                notificationItem(
                  text = strong(steps$text[7]),
                  icon = icon("exclamation")
                ))
  )
    # immediately navigate back to previous tab
    updateTabsetPanel(session, "all_tabs",
                      selected = rv$last_tab)
  }## end if
})


#'@-------------------------------
## a button to load the file
output$gpr_file_load <- renderUI({
    fileInput("gpr_file", "Choose one of the GPR files",
              buttonLabel = "Browse...",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")

    )
    # shinyFileChoose(input, 'gpr_file', defaultRoot = 'wd',
    #                   filetypes = c('gpr', "txt"))
})


## read file for the gpr
gpr_file_reactive <- reactive({
  # input$file will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns.
  inFile <- input$gpr_file

  if (is.null(inFile)){
    return(NULL)
  }else{
    x <- grep('Block.*Column|Column.*Block', readLines(inFile$datapath))
    # d_f <- read.table(inFile$datapath,skip=x-1, header = T)
    d_f <- data.table::fread(inFile$datapath,skip=x-1, header=TRUE)
    d_f <- d_f %>%
      group_by(Block) %>%
      mutate(meanX=mean(X),meanY=mean(Y),maxY=max(Y),maxX=max(X),minY=min(Y),minX=min(X))
    return(d_f)
  }

})


## reading the gpr file
gpr_header_reactive <- reactive({
  #inFile <- input$gpr_file
  inFile <- input$gpr_file
  if (is.null(inFile))    return(NULL)
  header_gpr <- readGPRHeader(file=inFile$datapath)


  return(header_gpr)

})


## empty structure
empty_strucuture <- reactive({
  text = paste("\nLoad one of the file that has MFI data\n",
               "       to visualize the structure ")
  plot_empty2 <- ggplot() +
    annotate("text", x = 4, y = 25, size=8, label = text) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  return(plot_empty2)
})


## a plot for the structure of the arrays
output$structure_plot <- renderPlot({
  gpr_file_reactive <- gpr_file_reactive()

  if(!is.null(gpr_file_reactive)){
    gpr_header <- gpr_header_reactive()

    gpr_header_wl <- unlist(strsplit(gpr_header$Wavelengths,"\\r|\\n|\\t"))
    gpr_header_wl <- gsub('\"',"",gpr_header_wl, fixed=TRUE)
    gpr_header_wl <- gpr_header_wl[gpr_header_wl!=""]
      p <- ggplot(gpr_file_reactive, aes(x=X, y=-Y, colour=as.factor(Block))) +
        #geom_rect(aes(xmin = minX, xmax = maxX, ymin = -minY, ymax = -maxY),color = "black",alpha=0.0001,fill="blue") +
        geom_point(size=.1) +
        theme_void()+
        theme(legend.position = "none") +
        # scale_color_brewer(palette="Set3") +
        geom_text(aes(x=meanX, y=-meanY, label=paste("Block",Block)), color="black",size=4) +
        ggtitle(paste("Scanned at", gpr_header_wl ,"Wavelength(s)"))


    }else if(is.null(input$gpr_file)){
      p <- empty_strucuture()

    }else{
      p <- empty_strucuture()

    }
  return(p)
})



## select the variable to plot
## selectig the spatial variable to plot
output$select_spatial_var <- renderUI({
  gpr_header <- gpr_header_reactive()
  gpr_file_reactive <- gpr_file_reactive()

  if(!is.null(gpr_file_reactive)){
    var_names <- names(gpr_file_reactive)
    var_names <- var_names[grepl("[Mm]edian|[Mm]ean",var_names)]
    tagList(
      useShinyFeedback(), # include shinyFeedback  # inclusion here is ideal; b/c inside module
      selectInput(inputId = "select_spatial",
                  "Select the variable to visualize",
                  choices = var_names,
                  selected = var_names[grep(paste0("^[B]635.*Median$"),var_names)]
      )
    )
  }
})



output$select_spatial_type <- renderUI({
  graphs <- c("Point graph"='point',
              "Array 2D graph"='2d_array')
  # graphs_id <- c("bar_chart","ridge_plot")
  prettyRadioButtons(inputId="spatial_type",
                     label = 'Plot an array 2D plot or a point graph:',
                     choices =  graphs,
                     inline=T, animation = "jelly",
                     status = "default",
                     shape = "curve",bigger = T)

})


## a plot for the spatial structure of the arrays
output$spatial_structure_plot <- renderPlotly({
  gpr_file_reactive <- gpr_file_reactive()
  inFile = input$gpr_file
  MFI_var=input$select_spatial
  if(!is.null(gpr_file_reactive)){
    m <- list(
      l = 20,
      r = 50,
      b = 50,
      t = 50,
      pad = 20
    )

      p <- visualize_slide(infile=inFile,
                           MFI_var =MFI_var, interactive=T ,d_f=gpr_file_reactive)
      p <- p  %>%
        layout(title = paste("Spatial visualization of", MFI_var ," MFI"),
               margin = m)
      #ggtitle(paste("Spatial visualization of", MFI_var ," MFI"))


  }else if(is.null(input$gpr_file)){
    p <- empty_strucuture()
    p <- ggplotly(p)
  }else{
    p <- empty_strucuture()
    p <- ggplotly(p)
  }
  return(p)
})




output$spatial_structure_plot_2d <- renderPlot({
  gpr_file_reactive <- gpr_file_reactive()
  inFile = input$gpr_file
  MFI_var=input$select_spatial
  if(!is.null(gpr_file_reactive)){


      p <- visualize_slide_2d(infile=inFile,
                              MFI_var =MFI_var,d_f=gpr_file_reactive)
      p <- p  + ggtitle(paste("Spatial visualization of", MFI_var ," MFI")) #%>% layout(title = paste("Spatial visualization of", MFI_var ," MFI"))


  }else if(is.null(input$gpr_file)){
    p <- empty_strucuture()

  }else{
    p <- empty_strucuture()

  }
  return(p)
})


## indicating the scanning wavelength
output$wavelength_struct <- renderInfoBox({
  gpr_header <- gpr_header_reactive()

  if(is.null(gpr_header)){
   infoBox("Select the a gpr/text",
            subtitle = paste0("LOAD A FILE") ,
            # icon = shiny::icon("user-md"),
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = TRUE)
  }else{
    gpr_header_wl <- unlist(strsplit(gpr_header$Wavelengths,"\\r|\\n|\\t"))
    gpr_header_wl <- gsub('\"',"",gpr_header_wl, fixed=TRUE)
    gpr_header_wl <- gpr_header_wl[gpr_header_wl!=""]
    infoBox("The file(s) were scanned at:", paste(gpr_header_wl,collapse = ';'),
            subtitle = paste0("Wavelength(s)") ,
            icon = shiny::icon("check-circle"),
            color = "orange",width = 4,fill = TRUE)
  }

})

## miniarray
output$total_miniarray_struct <- renderUI({
  gpr_header <- gpr_header_reactive()
  if(!is.null(gpr_header)){
    tagList(
      useShinyFeedback(), # include shinyFeedback  # inclusion here is ideal; b/c inside module
      numericInput(inputId = "total_miniarray",
                   "Input the total number of mini-arrays as per the graph below . Each miniarray appears as a rectangle",
                   value = ''
      )
    )
  }
})

#### warning for the selecting the number of mini arrays
observeEvent(input$total_miniarray, {
  req(input$total_miniarray)
  gpr_file_reactive <- gpr_file_reactive()

  if(!is.null(gpr_file_reactive)){
    blocks <- max(gpr_file_reactive$Block, na.rm = T)
  }else{
    blocks = 0
  }

  if (input$total_miniarray >blocks |input$total_miniarray <0  ) {
      showFeedbackWarning(inputId = "total_miniarray",
                          text = "The value is more or less than available blocks",color = "#FF0000")
    } else {
      hideFeedback("total_miniarray")
    }
})


## observer event for foreground  name
observeEvent(input$select_F, {
  req(input$select_F)
  if (!grepl('^F',input$select_F)  ) {
    showFeedbackWarning(inputId = "select_F",
                        text = "The variable selected does not start with F",color = "#FF0000")
  } else {
    hideFeedback("select_F")
  }
})


## observer event for background name
observeEvent(input$select_B, {
  req(input$select_B)
  if (!grepl('^B',input$select_B)  ) {
    showFeedbackWarning(inputId = "select_B",
                        text = "The variable selected does not start with B",color = "#FF0000")
  } else {
    hideFeedback("select_B")
  }
})


## observer event for input of samples per block
observeEvent(input$blockspersample_param, {
  req(input$blockspersample_param)
  req(input$total_miniarray)
  gpr_file_reactive <- gpr_file_reactive()
  if(!is.null(gpr_file_reactive)){
    mini_arrays=input$total_miniarray
    blocks <- max(gpr_file_reactive$Block, na.rm = T)
    value_check <- blocks/mini_arrays

  }else{
    value_check=0
  }
  #alue_check=2
  if (input$blockspersample_param>value_check |input$blockspersample_param<value_check    ) {
    showFeedbackWarning(inputId = "blockspersample_param",
                        text = "This does not match well with the mini array",color = "#FF0000")
  } else {
    hideFeedback("blockspersample_param")
  }
})



## select the input for the foreground var
output$select_F_var <- renderUI({
  gpr_header <- gpr_header_reactive()
  gpr_file_reactive <- gpr_file_reactive()

  if(!is.null(gpr_file_reactive)){
    var_names <- names(gpr_file_reactive)
    var_names <- var_names[grepl("[Mm]edian",var_names)]
    tagList(
      useShinyFeedback(), # include shinyFeedback  # inclusion here is ideal; b/c inside module
      selectInput(inputId = "select_F",
                   "Select the FOREGROUND variable, mostly the variable starts with F followed by wavelength",
                  choices =var_names,
                  selected = var_names[grep(paste0("^[F]635.*Median$"),var_names)]
      )
    )
  }
})

## selectig the background variable
output$select_B_var <- renderUI({
  gpr_header <- gpr_header_reactive()
  gpr_file_reactive <- gpr_file_reactive()

  if(!is.null(gpr_file_reactive)){
    var_names <- names(gpr_file_reactive)
    var_names <- var_names[grepl("[Mm]edian",var_names)]
    tagList(
      useShinyFeedback(), # include shinyFeedback  # inclusion here is ideal; b/c inside module
      selectInput(inputId = "select_B",
                  "Select the BACKGROUND variable, the variable starts with B followed by wavelength",
                  choices = var_names,
                  selected = var_names[grep(paste0("^[B]635.*Median$"),var_names)]
      )
    )
  }
})


## input the numbe of blocks
output$blockspersample_output <- renderUI({
  gpr_file_reactive <- gpr_file_reactive()
 if(!is.null(input$total_miniarray)){
   mini_arrays=input$total_miniarray
   blocks <- max(gpr_file_reactive$Block, na.rm = T)
   value_check <- blocks/mini_arrays

 }else{
   value_check=1
 }
  if(!is.null(gpr_file_reactive)){

    tagList(
      useShinyFeedback(), # include shinyFeedback  # inclusion here is ideal; b/c inside module
      numericInput("blockspersample_param",
        "Input the number blocks per sample (rectangle) as per the graph below",
        value = value_check
      )
    )
  }


})


#'@_______________end_structure_of_array_data____________________________________


#'@_______________parameters_of_array_data____________________________________
#'
## input for the parameters
## select  the folders



output$dir_files <- renderUI({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
    if(!is.na(input$total_miniarray) | !is.null(input$total_miniarray)){
      shinyFiles::shinyDirButton("folderChoose",
                                 "Chose directory with the data",
                                 "Upload")
    }else{
      stop("Define the mini-array number to load files")
    }
  }else{
    stop("Define the array structure to load files")
  }
  #shinyalert("Oops!", "Something went wrong.", type = "error")
})

## check why disable button not working????
# observe({
#   useShinyjs()
#   #if(!is.null(input$gpr_file)){
#    # shinyjs::enable("folderChoose")
#   #} else {
#     shinyjs::disable("folderChoose")
#   #}
# })

## define the tab to load on intialisation
observe({ # called only once at app init
  updateTabItems(session, "data_load_tabs", "dashboard")
})


## observer event for input of the channel
observeEvent(input$channel_param, {
  req(input$channel_param)
  req(gpr_header_reactive())
  gpr_header <- gpr_header_reactive()
  gpr_header_wl <- unlist(strsplit(gpr_header$Wavelengths,"\\r|\\n|\\t"))
  gpr_header_wl <- gsub('\"',"",gpr_header_wl, fixed=TRUE)
  gpr_header_wl <- gpr_header_wl[gpr_header_wl!=""]
if (input$channel_param %ni% gpr_header_wl &!is.null(gpr_header)  ) {
  showFeedbackDanger(inputId = "channel_param",
                        text = "This does not match with what was entered on the structure",color = "#FF0000")
  } else {
    hideFeedback("channel_param")
  }
})


## observer event for input of the samples per slide
observeEvent(input$totsamples_param, {
  req(input$totsamples_param)
  req(input$total_miniarray)

  if (input$totsamples_param!=input$total_miniarray  ) {
    showFeedbackDanger(inputId = "totsamples_param",
                       text = "This does not match with what was entered on the structure. Expects each sample per mini-array",color = "#FF0000")
  } else {
    hideFeedback("totsamples_param")
  }
})

## if the intial file is not laoded the user has the freedom of selecting the wavelength
output$channel_output <- renderUI({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
    gpr_header <- gpr_header_reactive()
    gpr_header_wl <- unlist(strsplit(gpr_header$Wavelengths,"\\r|\\n|\\t"))
    gpr_header_wl <- gsub('\"',"",gpr_header_wl, fixed=TRUE)
    gpr_header_wl <- gpr_header_wl[gpr_header_wl!=""]
    if(!is.null(gpr_header)){
      tagList(
        useShinyFeedback(),
        selectInput("channel_param",
                    h4("Input the channel used for scanning the data"),
                    c("635" = "635",
                      "532" = "532",
                      "488" = "488",
                      "594 " = "594 "),
                    selectize = FALSE,
                    selected =   gpr_header_wl[[1]]))
    }
  }else {
    tagList(
      useShinyFeedback(),
      selectInput("channel_param",
                  h4("Input the channel used for scanning the data"),
                  choices =  c("635" = "635",
                               "532" = "532",
                               "488" = "488",
                               "594 " = "594 "),
                  selectize = FALSE)
    )
    }
})

## the total samples per slide
output$total_samples_output <- renderUI({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
    if(is.null(input$total_miniarray)){
      numericInput(
        "totsamples_param",
        h4("Input the  total number of samples per slide "),
        value = ''
      )
    }else{
      numericInput(
        "totsamples_param",
        h4("Input the  total number of samples per slide "),
        value = input$total_miniarray
      )
    }

  }else{
    numericInput(
      "totsamples_param",
      h4("Input the  total number of samples per slide "),
      value = ''
    )
  }


  })


output$mig_prefix_output <- renderUI({
  textInput(
    "mig_suffix_param",
    h4("Input the prefix used to identify the MIG files"),
    placeholder = "_first"
  )
})

output$machine_output <- renderUI({
  textInput(
    "machine_param",
    h4("Hybridization machine used before expression"),
    placeholder = "m1"
  )
})


output$date_process_output <- renderUI({
  textInput(
    "date_process_param",
    h4("Input the month day and month of processing"),
    placeholder = "0505"
  )
})




#'@_______________End_definition_of_parameters____________________________________
##################################################################################


#'@_______________Path_of_data_definition_and_working_directory____________________________________
# sel_path <- reactive({
#     return(print(parseDirPath( c(home = wd_this) , input$folderChoose)))
#   })
#
setWorkingDir<- eventReactive(input$folderChoose,{
    setwd(sel_path())
})


## output all the directory
output$batch_select <- renderUI({
  ## the path
  folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
  paths <- list.dirs(path =sel_path() , full.names = TRUE)
  #paths <- paths[grepl("machine" , paths)]
  path_toutput <- gsub(paste0(getwd(),"/"),"", paths)

  selectInput(inputId='chip_path_param',
              label='Select the batch of data to be processed:',
              choices=path_toutput)

})


output$sampleID_select <- renderUI({
  ## the path
  folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
  paths <- list.dirs(path =sel_path() , full.names = TRUE)
  #paths <- paths[grepl("sample[Ii][Dd]" , paths)]
  path_toutput <- gsub(paste0(getwd(),"/"),"", paths)

  selectInput('sampleID_path_param',
              'Select the folder with sampleID\'s :',
              path_toutput)

})



#'@_______________End_Path_of_data_definition____________________________________
  ##################################################################################

#'@_______________function_to_collect_all_the_parametes____________________________________
  ## specify the the parameters to process the data
genepix_vars <- reactive({
  genepix_vars_ls <- array_vars(channel=input$channel_param ,
                            FG= input$select_F,
                            BG=input$select_B,
                             chip_path = input$chip_path_param , #sel_path , #"data/array_data",
                             totsamples = input$totsamples_param,
                             blockspersample = input$blockspersample_param,  # 2,
                             sampleID_path = input$sampleID_path_param ,#"data/array_sampleID/",
                             mig_prefix = input$mig_suffix_param,
                             machine = input$machine_param ,#1,
                             date_process = input$date_process_param )
    return(genepix_vars_ls)
})

#'@_______________end_all_parameter_collection____________________________________
##################################################################################


#'@_______________count_all_files_in_the_path_selected____________________________________
#'
#'
## first check all files have sample ID

check_sampleIDs <- reactive({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
    if(input$sampleID_path_param=="" | is.null(input$sampleID_path_param ) ){
      check_result <- NULL
    }else(
      check_result <- check_sampleID_files(genepix_vars())

    )

  }else{
    check_result <- NULL
  }


})

output$check_sampleID_out <- renderInfoBox({

  missing_id <- length(check_sampleIDs())
  if(missing_id==0){
    infoBox("Select the directory",
            subtitle = paste0("with sampleID's") ,
           # icon = shiny::icon("user-md"),
           icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = TRUE)
  }else{
    infoBox("Files with missing sampleID .csv:", missing_id,
            subtitle = paste0("The files are saved in the log file") ,
            icon = shiny::icon("user-md"),
            color = "fuchsia",width = 4,fill = TRUE)
  }

})


## lists all the files in the selected directory
all_files <- reactive({
  inFile <- input$gpr_file

  if(!is.null(inFile)){
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
   filesInDir <- NULL
 }else if(!is.null(input$chip_path_param)){
   pth_full <- file.path(input$chip_path_param)
   pth <-input$chip_path_param
   #path_toutput <- gsub(paste0(getwd(),"/"),"", input$chip_path_param)
   ### list the files in the directory
   filesInDir <- list.files(pth_full)

 }
  }else{
    filesInDir <- NULL
  }
   return(filesInDir)
})


##total number of files
output$files_in_batch <- shinydashboard::renderInfoBox({
  total_files <- length(all_files())
  if(total_files==0){
    infoBox("Select the folder with array data",
            paste0("NA"),
            icon = icon("exclamation-triangle"),
            color = "red")
  }else if(total_files>0){
    infoBox("Total files in batch",
      paste0(total_files),
      subtitle = paste0("Files ending with .txt or .gpr"),
      icon = icon("list-ul"),
      color = "olive"
    )
  }


})

#'@_______________end_all_count_of_files____________________________________
##################################################################################


#'@_______________Display_some_key_info____________________________________
#'
#'
#### read in all the datasets
### list all the file names under data folder
filenames_reactive <- reactive({
  genepix_vars <- genepix_vars()
  filenames_return <- list.files(file.path(genepix_vars$chip_path),
                        pattern="*.txt$|*.gpr$", full.names=F)


  return(filenames_return)
})


data_files_reactive  <-  reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    data_files <- NULL
  }else {
    genepix_vars <- genepix_vars()
    filenames <- filenames_reactive()

    data_files <- purrr::map(.x = filenames,
                             .f = read_array_files,
                             data_path=genepix_vars$chip_path ,
                             genepix_vars=genepix_vars)
    data_files <- set_names(data_files, purrr::map(filenames, name_of_files))
  }

  return(data_files)
})

dfs <- reactive({
  dfs_names <- names(data_files_reactive())
  return(dfs_names)
})




params_display <- reactive({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    params_df = NULL
  }else{
    data_files <- data_files_reactive()
    genepix_vars <- genepix_vars()
    df_1 <- data_files[[1]]

    blocks <- max(df_1$Block)
    spotsperblock <- table(df_1$Block)[[1]]
    antigens <- length(unique(df_1$Name))
    antigen_list <- c(unique(as.character(df_1$Name)))
    antigen_list <- gsub(" ","",antigen_list)
    params_df <- list(blocks=blocks,
                      spotsperblock=spotsperblock,
                      antigens=antigens,
                      antigen_list=antigen_list)
  }
    }else{
      params_df = NULL
  }

 return(params_df)
})


output$blocks_count <- renderInfoBox({
  #req(input$chip_path_param)
  params_df <- params_display()
  if(is.null(params_df) ){
    infoBox("Select the directory",
            subtitle = paste0("with array data") ,
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = TRUE)
  }else{
    infoBox("Each array file has : ", paste(params_df$blocks, "blocks"),
            subtitle = paste("with",params_df$spotsperblock, "spots per block") ,
            icon = icon("th",lib = "font-awesome"),
            color = "red",width = 4,fill = F)
  }

})


output$antigens_count  <- renderInfoBox({
  params_df <- params_display()
  if(is.null(params_df)){
    infoBox("Select the directory",
            subtitle = paste0("with array data") ,
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = TRUE)
  }else{
    infoBox("Each array file has: ", params_df$antigens,
            subtitle = paste0("antigens") ,
            icon = icon("allergies",lib = "font-awesome"),
            color = "red",width = 4,fill = F)
  }

})




## empty plot for adding to before data load
empty_plot <- reactive({
  inFile <- input$gpr_file
  if(!is.null(inFile)){

    if(is.na(input$total_miniarray)){
      x=0
    }else {x=1}

    if(x==0){
      text = paste("\nPlot will appear after\n",
                   " a) defining the number of mini-arrays in design structure\n",
                   " b) selecting folder with data")
    }else  {
      text = paste("\nPlot will appear after\n",
                   " a) selecting folder with data\n"
      )
    }

  }else{
    text = paste("\nPlot will appear after\n",
                 " a) defining the array design structure then \n",
                 " b) selecting folder with data")
  }

   plot_empty <- ggplot() +
     annotate("text", x = 4, y = 25, size=8, label = text) +
     theme_bw() +
     theme(panel.grid.major=element_blank(),
           panel.grid.minor=element_blank())
   return(plot_empty)
})


empty_plot_error <- reactive({
  text = paste("\nCheck data stucture well.\n",
               "       replicates definition or ")
  plot_empty <- ggplot() +
    annotate("text", x = 4, y = 25, size=8, label = text) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  return(plot_empty)
})


empty_plot_NS <- reactive({
  text = paste("\nNot selected.\n",
               " ")
  plot_empty <- ggplot() +
    annotate("text", x = 4, y = 25, size=8, label = text) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  return(plot_empty)
})


empty_plot_no_rep <- reactive({
  text = paste("\nThe experiment did not have\n",
               "       lab replicates or \n",
               "Select the no of lab replicates")
  plot_empty <- ggplot() +
    annotate("text", x = 4, y = 25, size=8, label = text) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  return(plot_empty)
})

### plot of the buffer spots
## filenams
merged_dfs_reactive <- reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    sample_ID_merged_dfs <- NULL
  }else {
    genepix_vars <- genepix_vars()
    filenames <- filenames_reactive()
    data_files <-  data_files_reactive()
    dfs <- dfs()
    sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files ,
                                         genepix_vars, method="subtract_local")
    sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))



  }

  return(sample_ID_merged_dfs)
})



# table of a sample dataset of the file
output$tbl_all_data <-  DT::renderDT({
  folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
  if(!rlang::is_empty(sel_path) ){#input$folderChoose!=''
    all_datas <- list.files(sel_path() , recursive = T)
    sample_ID_merged_dfs <-  merged_dfs_reactive()
    d_f <- plyr::ldply(sample_ID_merged_dfs)
    write_csv(d_f, paste("processed_data/raw_data-", Sys.Date(), ".csv", sep=""))
    df <- DT::datatable(d_f)

  }
  #options = list(lengthChange = FALSE,
  #  initComplete = JS('function(setting, json) { alert("done"); }'))
  return( df)
})

## download all data
output$download_Raw_Data <- downloadHandler(
  filename = function() {
    paste("raw_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    sample_ID_merged_dfs <-  merged_dfs_reactive()
    d_f <- plyr::ldply(sample_ID_merged_dfs)
    write_csv(d_f, file)
  }
)



dfs_reactive_bg_correct <- reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    sample_ID_merged_dfs <- NULL
  }else {
    genepix_vars <- genepix_vars()
    filenames <- filenames_reactive()
    data_files <-  data_files_reactive()
    dfs <- dfs()
    sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files ,
                                       genepix_vars, method=input$bg_correct_1)
    sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))
}

  return(sample_ID_merged_dfs)
})


## function with bg correct input under CV tab
merged_dfs_reactive_bg_correct <- reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    sample_ID_merged_dfs <- NULL
  }else {
    genepix_vars <- genepix_vars()
    filenames <- filenames_reactive()
    data_files <-  data_files_reactive()
    dfs <- dfs()
   sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files ,
                                         genepix_vars, method=input$bg_correct)
  sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))



  }

  return(sample_ID_merged_dfs)
})


all_data_reactive <- reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    allData <- NULL
  }else {
    dfs <- dfs()
    filenames <- filenames_reactive()
    data_files <-  data_files_reactive()
    genepix_vars <- genepix_vars()

    allData <- purrr::map(.x=dfs, .f=extract_bg,data_files=data_files,genepix_vars)
    allData <- set_names(allData, purrr::map(filenames, name_of_files))
    allData <- plyr::ldply(allData)
  }
 return(allData)
})


buffers_dfs_reactive <- reactive({
  if(is.null(input$chip_path_param) | input$chip_path_param==""){
    buffers <- NULL
  }else {
    sample_ID_merged_dfs <-  merged_dfs_reactive()
    filenames <- filenames_reactive()
    buffer_transp <- purrr::map(.x=sample_ID_merged_dfs, .f=buffer_spots)

    buffer_transp <- set_names(buffer_transp, purrr::map(filenames, name_of_files))

    buffers <- plyr::ldply(buffer_transp)
  }

  return(buffers)
})

 ## plot the buffer spots
output$data_process <- renderPlot({#DT::renderDT
  inFile <- input$gpr_file
  if(!is.null(inFile)){
  if(input$chip_path_param!="") {

    buffers <-  buffers_dfs_reactive()
    merged_dfs <- merged_dfs_reactive()
    dfs <- dfs()

    p <-  plot_buffer(buffers,buffer_names="antigen",buffer_mfi="FMedianBG_correct",slide_id=".id")

     }else{
     p <- empty_plot()

   }
  }else{
    p <- empty_plot()

  }
  return(p)
 })


output$data_box <- renderPlotly({
  inFile <- input$gpr_file
  if(!is.null(inFile)){
  if(input$chip_path_param!="") {
    sample_ID_merged_dfs <-  merged_dfs_reactive()

    merged_dfs <- plyr::ldply(sample_ID_merged_dfs)
    merged_dfs <- merged_dfs %>%
      filter(!grepl("[Bb][Uu][Ff][Ff][Ee][Rr]",antigen))

    antigens_list <- unique(merged_dfs[["antigen"]])
    antigens_list_plot <- antigens_list[input$slider_antigen[[1]]:input$slider_antigen[[2]]]

    merged_dfs <- merged_dfs %>%
      filter(antigen %in% antigens_list_plot)

    p <- plot_ly(merged_dfs, y = ~FMedianBG_correct, color = I("blue"),
                 alpha = 0.1, boxpoints = "suspectedoutliers",width=0.1 )
    p <- p %>%
      add_boxplot(x = ~antigen)  %>%
      layout(xaxis = list(title = "", tickangle = -45))
    p
  }else{
    p <- empty_plot()
    p <- ggplotly(p)
  p
  }
  }else{
    p <- empty_plot()
    p <- ggplotly(p)
    p
  }
})
#'@_______________End_display_some_key_info____________________________________

#'@_______________Background_correction____________________________________

## define which tab to load on opening the tab items
observe({ # called only once at app init
  updateTabItems(session, "bg_tabs", "bg_graphs")
})


output$slide_names_bg <- renderUI({
  if(input$chip_path_param!="") {
 dfs_names <- list('Slides'=dfs())
 dfs <- dfs()
 pickerInput(inputId="slide",
             label="Choose a slide:",
             choices= dfs,
             options = list(
               `actions-box` = TRUE,
               size = 10,
               `selected-text-format` = "count > 3"
             ),
             selected = paste(dfs),
             multiple = T)
  }
})


output$select_log_MFI <- renderUI({
  graphs <- c("Log the MFI"=TRUE,
              "Raw MFI"=FALSE)
  # graphs_id <- c("bar_chart","ridge_plot")
  prettyRadioButtons(inputId="log_mfi",
                     label = 'Plot with log or raw MFI:',
                     choices =  graphs,
                     inline=T, animation = "jelly",
                     status = "default",
                     shape = "curve",bigger = T)

})

output$select_block_antigen <- renderUI({
  graphs <- c("Block"="Block",
              "Antigen"="antigen")
  awesomeRadio("block_antigen",
               label = 'Select block or antigen:',
               choices =  graphs,
               # choiceValues=all_var_prefix,
               #selected = character(0),
               inline=T)

})

output$value_block <- renderUI({
  paste(input$block_antigen)
})


observe({
  x <- input$block_antigen
  y <- 'Background plot by'

  updateTextInput(session, "value_block", value = paste(y, x))


})

output$bg_plots <- renderPlotly({
  if(input$chip_path_param!="") {
    ## load the default parameted
    allData_bg <-  all_data_reactive()
    allData_bg <- allData_bg %>%
      filter(.id %in% input$slide)

    p<- plot_bg(df=allData_bg, x_axis=input$block_antigen,bg_MFI="BG_Median",
            log_mfi=input$log_mfi)
    p <- ggplotly(p) %>%  layout(height = 800)
    p
  }else{
    p <- empty_plot()
    p <- ggplotly(p)
    p
  }

})


output$fg_bg_plots <- renderPlotly({
  if(input$chip_path_param!="") {
    ## load the default parameted
    allData_bg <-  all_data_reactive()

    allData_bg <- allData_bg %>%
      filter(.id %in% input$slide)
    p <- plot_FB(allData_bg,antigen_name="antigen",bg_MFI="BG_Median",FG_MFI="FBG_Median")
    p <- ggplotly(p, tooltip = "text")%>%  layout(height = 800)
    p
  }else{
    p <- empty_plot()
    p <- ggplotly(p)
    p
  }

})




output$bg_correction_select_1 <- renderUI({
  bg_correct_approaches <- c("None"="none",
                             "Local bakground subtraction"="subtract_local",
                             "Global background subtraction"="subtract_global",
                             "Moving mininimum"="movingmin_bg",
                             "Half Moving mininimum"="minimum_half")
  selectInput(inputId = 'bg_correct_1',
              label = 'Background correction',
              selected = 'subtract_local',
              choices = bg_correct_approaches)
})


output$bg_correct_graphs <- renderPlot({
  if(input$chip_path_param!="") {
    bg_correct_df <- dfs_reactive_bg_correct()
    bg_correct_df <- plyr::ldply(bg_correct_df)
    all_df <- bg_correct_df %>%
      select(antigen,FMedian,FMedianBG_correct) %>%
      gather(var,mfi,-antigen)

    p <- ggplot(all_df,aes(x=antigen, y=mfi))+
      geom_boxplot()+
      facet_wrap(~var, nrow = 2)+
      geom_hline(yintercept = 0, color='red')+
      theme_light()+
    #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ylab("MFI") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
            strip.background = element_rect(fill="black",size = 3),
            strip.text.x = element_text(size=12, color="white",
                                        face="bold.italic")) +
      ggtitle(paste0(input$bg_correct_1," background correction"))


  }else{
    p <- empty_plot()

  }
  return(p)
},height=800)

#'@_______________End_Background_correction____________________________________
#'
#'
#'
#'@_______________CV_calculation____________________________________

observe({ # called only once at app init
  updateTabItems(session, "cv_tabs", "bg_correct_cv")
})


output$bg_correction_drop_down <- renderUI({
  bg_correct_approaches <- c("None"="none",
              "Local bakground subtraction"="subtract_local",
              "Global background subtraction"="subtract_global",
              "Moving mininimum"="movingmin_bg",
              "Half Moving mininimum"="minimum_half")

  dropdownButton(
    tags$h3("List of Inputs"),
    selectInput(inputId = 'bg_correct', label = 'Background correction', choices = bg_correct_approaches),
    selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
    sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
  circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
    tooltip = tooltipOptions(title = "Click to see inputs !")
  )
})



output$bg_correction_select <-  renderUI({
  if(!is.null(input$bg_correct_1)){
    selected_bg <- input$bg_correct_1
  }else{
    selected_bg <- 'subtract_local'
  }
  bg_correct_approaches <- c("None"="none",
                             "Local bakground subtraction"="subtract_local",
                             "Global background subtraction"="subtract_global",
                             "Moving mininimum"="movingmin_bg",
                             "Half Moving mininimum"="minimum_half")
  selectInput(inputId = 'bg_correct',
              label = 'Background correction',
              selected = selected_bg,
              choices = bg_correct_approaches)
})

output$cv_value_select <- renderUI({
  sliderInput(inputId = 'cv_value', label = 'Select CV cut off', value = 20, min = 1, max = 100)
})

output$cv_ui <- renderText({ input$cv_value })


output$minimum_mfi_select <- renderUI({
  sliderInput(inputId = 'cv_value_mfi', label = 'Select MFI for CV cut off', value = 1000, min = 100, max = 10000)
})



output$replicates_select <- renderUI({
  sliderInput(inputId = 'lab_replicates',
              label = 'Select the number of lab replicates',
              value = 1, min = 1, max = 5)
})


data_CV_reactive <- reactive({
  if(input$chip_path_param!="") {
  sample_ID_merged_dfs <-  merged_dfs_reactive_bg_correct()
  filenames <- filenames_reactive()
  dataCV <- purrr::map(.x=sample_ID_merged_dfs, .f=cv_estimation ,lab_replicates=input$lab_replicates,
                       cv_cut_off=input$cv_value)

  dataCV <- set_names(dataCV, purrr::map(filenames, name_of_files))
  }else {
    dataCV <- NULL
  }
  return(dataCV)
})


data_CV_best2_reactive <- reactive({
  if(input$chip_path_param!="") {
    dataCV <- data_CV_reactive()
    filenames <- filenames_reactive()
    dataCV_best2 <- purrr::map(.x=dataCV, .f=best_CV_estimation , slide_id="iden" ,
                               lab_replicates=input$lab_replicates,cv_cut_off=input$cv_value)


# # give the names to the returned list
    dataCV_best2 <- set_names(dataCV_best2, purrr::map(filenames, name_of_files))
  }else {
    dataCV_best2 <- NULL
  }
  return(dataCV_best2)
})

output$cv_corr_plot <- renderPlot({
  dataCV <- data_CV_reactive()
  dataCV <- plyr::ldply(dataCV)
  if(input$chip_path_param!="") {
  if(max(dataCV$replicates) <=input$lab_replicates & input$lab_replicates!=1){

    ggpairs(dataCV,aes(color=cvCat_all) ,
            columns = paste(1:input$lab_replicates), title = "",  axisLabels = "show") +
      theme_light()
  }else if(input$lab_replicates==1){
    p <- empty_plot_no_rep()
    p
  }else{
    p <- empty_plot_error()
    p
  }
  }else{
    p <- empty_plot()
    p
  }

})

dataCV_all_sample_reactive <- reactive({
  if(input$lab_replicates>1) {
  dataCV <- data_CV_reactive()
  filenames <- filenames_reactive()

  dataCV_sample <- purrr::map(.x=dataCV, .f=cv_by_sample_estimation , cv_variable="cvCat_all" ,
                              lab_replicates=input$lab_replicates)
  dataCV_sample <- set_names(dataCV_sample, purrr::map(filenames, name_of_files))
  all_cv_sample <- plyr::ldply(dataCV_sample)
  }else {
  all_cv_sample <- NULL
  }
  return(all_cv_sample)
})



dataCV_sample_best2_reactive <- reactive({
  if(input$lab_replicates>1) {
    dataCV_best2 <- data_CV_best2_reactive()
    filenames <- filenames_reactive()

    dataCV_sample_best2 <- purrr::map(.x=dataCV_best2, .f=cv_by_sample_estimation , cv_variable="best_CV_cat" ,
                                      lab_replicates=input$lab_replicates)
    dataCV_sample_best2 <- set_names(dataCV_sample_best2, purrr::map(filenames, name_of_files))
    all_cv_sample_best2 <- plyr::ldply(dataCV_sample_best2)
  }else {
    all_cv_sample_best2 <- NULL
  }
  return(all_cv_sample_best2)
})


output$cv_violin_plot <- renderPlot({
  all_cv_sample <- dataCV_all_sample_reactive()
  cv_cut_off <- input$cv_value

  if(input$chip_path_param!="") {
    if(!is.null(all_cv_sample) & !plyr::empty(all_cv_sample)){
      ## plot only the CV perccentages
      less_20 <- rlang::sym(paste0("CV <= ",cv_cut_off, "_perc"))
      gt_20 <- rlang::sym(paste0("CV > ",cv_cut_off, "_perc"))

      less_20_per <-  rlang::sym(paste0("% CV <=",cv_cut_off))
      gt_20_per <-  rlang::sym(paste0("% CV >",cv_cut_off))

      p <- ggplot(all_cv_sample)+
        geom_violin(aes_string(".id",less_20,color=shQuote(less_20_per))) +
        geom_violin(aes_string('.id',gt_20, color=shQuote(gt_20_per))) +
        geom_violin(aes(.id,Others_perc,color="Others")) +
        ylab("% of CV") +
        theme_light() +
        ggtitle(paste0("% of CV >", input$cv_value ,"or <=",input$cv_value," for each sample all repeats considered"))


    }else if(plyr::empty(all_cv_sample)){
      p <- empty_plot_no_rep()

    }else{
      p <- empty_plot_error()

    }
  }else{
    p <- empty_plot()

  }
  return(p)
})


output$cv_violin_plot_best2 <- renderPlot({
  all_cv_sample_best2 <- dataCV_sample_best2_reactive()

  cv_cut_off <- input$cv_value

  if(input$chip_path_param!="") {
    if(!is.null(all_cv_sample_best2) & !plyr::empty(all_cv_sample_best2)){
      ## plot only the CV perccentages
      if('Others_perc' %ni% names(all_cv_sample_best2)){
        all_cv_sample_best2['Others_perc'] <- 0
        all_cv_sample_best2['Others_n'] <- 0
      }
      less_20 <- rlang::sym(paste0("CV <= ",cv_cut_off, "_perc"))
      gt_20 <- rlang::sym(paste0("CV > ",cv_cut_off, "_perc"))

      less_20_per <-  rlang::sym(paste0("% CV <=",cv_cut_off))
      gt_20_per <-  rlang::sym(paste0("% CV >",cv_cut_off))
      p <- ggplot(all_cv_sample_best2)+
        geom_violin(aes_string(".id",less_20,color=shQuote(less_20_per))) +
        geom_violin(aes_string('.id',gt_20, color=shQuote(gt_20_per))) +
        geom_violin(aes(.id,Others_perc,color="Others")) +
        ylab("% of CV") +
        theme_minimal() +
        ggtitle(paste0("% of CV >", input$cv_value ,"or <=",input$cv_value," for each sample all repeats considered"))
      p

    }else if(plyr::empty(all_cv_sample_best2)){
      p <- empty_plot_no_rep()
      p
    }else{
      p <- empty_plot_error()
      p
    }
  }else{
    p <- empty_plot()
    p
  }

})



# table of a sample dataset of the file
output$sample_best2_reactive_tbl <-  DT::renderDT({
  dataCV_sample_best2 <- dataCV_sample_best2_reactive()
 if(!rlang::is_empty(sel_path) ){#input$folderChoose!=""

   all_vars <- names(dataCV_sample_best2)
   percs <- all_vars[grepl("perc",all_vars)]

   divide_100 <- function(x, na.rm = FALSE) (x/100)
   dataCV_sample_best2 <- dataCV_sample_best2 %>% mutate_at(percs,divide_100 )

   sample_best2_df <-  datatable(dataCV_sample_best2)  %>%
     formatPercentage( columns = percs, 2) %>%
     formatStyle(
       columns = percs,
       background = styleColorBar(0:1, 'red'),
       backgroundSize = '100% 90%',
       backgroundRepeat = 'no-repeat',
       backgroundPosition = 'center'
     )
    sample_best2_df
 }else {
   sample_best2_df <- data.frame()
  }

})
#'@_______________End_CV_calculation____________________________________



#' @________________________________subtract_the_tag_values_______________________________________

output$tag_subtract_select <- renderUI({
  #materialSwitch(inputId = "tag_subtract_btn", label = "Tag", status = "danger")
  # switchInput(
  #   inputId = "tag_subtract_btn",
  #   value = FALSE,
  #   size = "mini",
  #   #label = "Purification TAGS",
  #   onLabel = "TAG",
  #   offLabel = "No TAG",
  #   onStatus = "success",
  #   offStatus = "danger",
  #   #labelWidth = "100px",
  #   width='auto'
  # )

  materialSwitch(inputId = "tag_subtract_btn",
                 label = "Select if purification TAG(s) was used",
                 right = TRUE,
                 value = FALSE,
                 status = "success"
                 )


})



output$tag_file_load <- renderUI({
  if(input$tag_subtract_btn==T){
    fileInput("tag_file", "Choose CSV File with tag antigens",
              buttonLabel = "Browse...",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"))
  }else {
    warning("No purification TAGs used")
  }

 # shinyFileChoose(input, 'tag_file', defaultRoot = 'wd',
 #                   filetypes = c('csv', "txt"))
})


tag_file_reactive <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  inFile <- input$tag_file

  if (is.null(inFile))
    return(NULL)
  df_tag <- read_csv(inFile$datapath)
  return(df_tag)
})





output$tag_antigens_select <- renderUI({
  tag_file <- tag_file_reactive()
 if(!is.null(tag_file)) {
   params_df <- params_display()
   antigens <-   params_df$antigen_list

   if('TAG_name' %in% names(tag_file)){
     list_tag <- antigens[antigens %in% unique(tag_file$TAG_name)]
   }else{
     list_tag <- ""
   }
   antigen_list  <- list('Antigens'=antigens)
    selectInput(inputId="tag_antigens",
                  label="Select the TAG antigens:",
                  choices= antigen_list,
                  selected = list_tag ,
                  multiple = T)
    }
})

output$sample_ID_select <- renderUI({
  if(input$chip_path_param!="") {

    tag_data <- tag_data_reactive()
     sample_list <- unique(tag_data$sampleID)
    sample_list  <- list('Antigens'=sample_list)
    selectInput(inputId="sample_tag",
                label="Choose a sample to compare TAG values:",
                choices= sample_list,
                # selected = "" ,
                multiple = F)
  }
})


output$tag_antigen_radio_select <- renderUI({
  tag_antigens <- input$tag_antigens
  awesomeRadio("tag_antigen_radio",
               label = 'Select the antigen to visualize:',
               choices =  tag_antigens,
             selected = tag_antigens[[1]],
               inline=T)

})




## tag subtracted data
dataCV_tag_reactive <- reactive({
  inFile <- input$tag_file
  if(input$chip_path_param!="" & !is.null(inFile) & input$tag_subtract_btn==T ) {
    dataCV_best2 <- data_CV_best2_reactive()
    filenames <- filenames_reactive()
    tag_antigens <- input$tag_antigens
    tag_file <- tag_file_reactive()
    genepix_vars <- genepix_vars()
    batch_vars <- list(machine= genepix_vars$machine,
                       day =genepix_vars$date_process)


  dataCV_tag <- purrr::map(.x=dataCV_best2, .f=tag_subtract ,
                         tag_antigens=tag_antigens,
                         mean_best_CV_var="mean_best_CV",
                         tag_file=tag_file,
                         batch_vars=batch_vars)

  dataCV_tag <- set_names(dataCV_tag, purrr::map(filenames, name_of_files))
  dataCV_tag <- plyr::ldply(dataCV_tag)



  }else if(input$chip_path_param!="" & is.null(inFile) & input$tag_subtract_btn==F) {
    dataCV_best2 <- data_CV_best2_reactive()
    filenames <- filenames_reactive()

    genepix_vars <- genepix_vars()
    batch_vars <- list(machine= genepix_vars$machine,
                       day =genepix_vars$date_process)
    dataCV_best2 <- set_names(dataCV_best2, purrr::map(filenames, name_of_files))
    dataCV_tag <- plyr::ldply(dataCV_best2)

    dataCV_tag <- dataCV_tag %>% ungroup() %>%
      dplyr::select(sampleID ,sample_array_ID, antigen , everything()) %>%
      mutate(mean_best_CV_tag=mean_best_CV,TAG_mfi=0,TAG=NA,TAG_name='') %>%
      mutate(machine=batch_vars[["machine"]] , day=batch_vars[["day"]])
  }else{
    dataCV_tag <- NULL
  }
  return(dataCV_tag)
})



dataCV_tag_specific_reactive <- reactive({
  dataCV_tag <- dataCV_tag_reactive()
  if(!is.null(dataCV_tag)){

    df <- dataCV_tag %>%
      filter(TAG_name==input$tag_antigen_radio)
  }else{
    df <- NULL
  }
 return(df)
})



output$antigen_tag_specific_select <- renderUI({
  df_tag_specific <- dataCV_tag_specific_reactive()
  antigens <- unique(df_tag_specific$antigen)
  antigen_list  <- list('Antigens'=antigens)
  pickerInput(inputId="tag_specific_antigens",
              label="Choose the TAG antigens to visualize:",
              choices= antigen_list,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              selected = paste(antigens),
              multiple = T)
})



tag_data_reactive <- reactive({
  dataCV_tag <- dataCV_tag_reactive()
  if(!is.null(dataCV_tag)){
   tag_antigens <- input$tag_antigens
    tag_data_wide <- dataCV_tag %>%
      filter(antigen==tag_antigens[[2]])
    tag_data_wide <- tag_data_wide %>%
      select(.id, sampleID,tag_antigens) %>%
      gather(tag_antigen, MFI, -c(.id, sampleID))
  }else{
    tag_data_wide <- NULL
  }
  return(tag_data_wide)
})


### create a table and download the dataCV corrected data
## display the data
# table of a sample dataset of the file
output$tbl_data_cv<-  DT::renderDT({
  folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
  if(!rlang::is_empty(sel_path)){#input$folderChoose!=""){
    dataCV_tag <- dataCV_tag_reactive()
    dataCV_tag <- dataCV_tag %>% mutate_if(is.numeric, ~round(., 3))

    write_csv(dataCV_tag, paste("processed_data/data_CV-", Sys.Date(), ".csv", sep=""))

    df <- DT::datatable(dataCV_tag)


  }
  #options = list(lengthChange = FALSE,
  #  initComplete = JS('function(setting, json) { alert("done"); }'))
  return(df)
}, filter = 'top')

## download all data
output$download_dataCV <- downloadHandler(
  filename = function() {
    paste("raw_data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    dataCV_tag <- dataCV_tag_reactive()

    write_csv(dataCV_tag, file)
  }
)



output$tag_box <- renderPlot({
  tag_data <- tag_data_reactive()
  if(input$chip_path_param!="") {
    if(!is.null(tag_data)){
    p <- ggplot(tag_data, aes(tag_antigen,MFI)) +
        geom_violin(alpha=0.3) +
        geom_boxplot(width = 0.2) +
        geom_jitter(aes(color=.id),
                    position = position_jitter(width = .15, height=-0.7),
                    size=2)+
        theme_light()
    p

    }else{
      p <- empty_plot_error()
      p
    }
  }else{
    p <- empty_plot()
    p
  }
})

output$tag_box_sample <- renderPlot({
  tag_data <- tag_data_reactive()
  if(input$chip_path_param!="") {

    if(!is.null(tag_data)){
      tag_data <- tag_data %>%
        filter(sampleID %in% input$sample_tag)
      p <- ggplot(tag_data, aes(tag_antigen,MFI)) +
        geom_violin(alpha=0.3) +
        geom_boxplot(width = 0.2) +
        geom_jitter(aes(color=.id),
                    position = position_jitter(width = .15, height=-0.7),
                    size=2)+
        theme_light()
      p

    }else{
      p <- empty_plot_error()
      p
    }
  }else{
    p <- empty_plot()
    p
  }
})



output$tag_antigens_box  <- renderPlot({
  dataCV_tag_specific <- dataCV_tag_specific_reactive()
  if(!is.null(input$tag_specific_antigens)){
    df_plot <- dataCV_tag_specific %>%
      dplyr::select(.id,sampleID,antigen,before_tag=mean_best_CV,after_tag=mean_best_CV_tag) %>%
      filter(antigen %in% input$tag_specific_antigens)
  }else{
    df_plot <- dataCV_tag_specific %>%
      dplyr::select(.id,sampleID,antigen,before_tag=mean_best_CV,after_tag=mean_best_CV_tag)
  }
  df_plot <- df_plot %>%
    gather(measure,mfi,-c(.id:antigen))
  ggplot(df_plot,aes(as.factor(antigen),mfi,color=measure))  +
    geom_boxplot(aes(fill=measure),alpha=0.5)+
    theme_light() +
     xlab("antigen name")+
    ggtitle("Before and after TAG subtraction") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

## info boxes to show the selected paramaeter
output$bg_correct_infobox <- output$bg_correct_infobox2 <- renderInfoBox({
  if(is.null(input$bg_correct)){
    infoBox("Select the background correct approach",
            subtitle = paste0("") ,
            # icon = shiny::icon("user-md"),
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = F)
  }else{
    bg_correct_approaches <- c("None"="none",
                               "Local bakground subtraction"="subtract_local",
                               "Global background subtraction"="subtract_global",
                               "Moving mininimum"="movingmin_bg",
                               "Half Moving mininimum"="minimum_half")
    bg_app <- names(which(bg_correct_approaches == input$bg_correct))
    infoBox("Background correction",bg_app ,
            subtitle = paste0("") ,
            icon = shiny::icon("layer-group"),
            color = "aqua",width = 4,fill = F)
  }

})


output$tag_infobox <- output$tag_infobox2 <- renderInfoBox({
  if(is.null(input$tag_antigens)){
    infoBox("Select the TAG antigens",
            subtitle = paste0("") ,
            # icon = shiny::icon("user-md"),
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = F)
  }else{
    antigens <- paste(input$tag_antigens,collapse=" ,")
    infoBox("TAG antigens",antigens ,
            subtitle = '' ,
            icon = shiny::icon("tag"),
            color = "aqua",width = 4,fill = F)
  }

})


output$cv_infobox <- output$cv_infobox2 <- renderInfoBox({
  if(is.null(input$cv_value)){
    infoBox("Select the CV cutoff",
            subtitle = paste0("") ,
            # icon = shiny::icon("user-md"),
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = F)
  }else{
    infoBox("Selected CV cutoff",paste(input$cv_value,"% with") ,
            subtitle = paste0(input$lab_replicates," lab replicates") ,
            icon = shiny::icon("cut"),
            color = "aqua",width = 4,fill = F)
  }

})

### hide the menu item for TAG visuals
### hide the menu item for TAG visuals
output$tag_plots <- renderMenu({
  req(input$tag_subtract_btn)
  if(input$tag_subtract_btn == T)

    menuItem("Tag subtract", tabName = "tag_plots", icon = icon("bell")#,
             #menuSubItem("No option",tabName="RO_00"),
             #menuSubItem("Option 1",tabName="RO_01")
    )
})







#'
#'
#'@________________________________end_subtract_the_tag_values_______________________________________
#'
#'
#'@________________________________Normalisation_______________________________________
observe({ # called only once at app initialisation
  updateTabItems(session, "norm_tabs", "normalisation")
})


output$normalisation_select <- renderUI({
  normalisation_approaches <- c("Log2 Normalisation"="log2",
                                "VSN"="vsn",
                                "Cyclic Loess"="cyclic_loess",
                                "Cyclic Loess (log)"="cyclic_loess_log",
                                "RLM Normalisation"="rlm")
  selectInput(inputId = 'normalisation_method',
              label = 'Select normalisation method',
              selected = 'log2',
              choices = normalisation_approaches)
})

## select the control antigens to use with RLM
output$rlm_antigens_select <- renderUI({
  params_df <- params_display()
  antigens <-   params_df$antigen_list
  antigen_list  <- list('Antigens'=antigens)
  if(input$normalisation_method=="rlm") {
    selectInput(inputId="rlm_antigens",
                label="Choose the RLM antigens:",
                choices= antigen_list,
                multiple = T)
  }
})


output$normalisation_infobox <- renderInfoBox({
  if(is.null(input$normalisation_method)){
    infoBox("Select the normalisation method",
            subtitle = paste0("") ,
            # icon = shiny::icon("user-md"),
            icon = icon("exclamation-triangle"),
            color = "red",width = 4,fill = F)
  }else{
    normalisation_approaches <- c("Log2 Normalisation"="log2",
                                  "VSN"="vsn",
                                  "Cyclic Loess"="cyclic_loess",
                                  "Cyclic Loess (log)"="cyclic_loess_log",
                                  "RLM Normalisation"="rlm")
    normal_app <- names(which(normalisation_approaches == input$normalisation_method))
    infoBox("Normalisation approach",paste(normal_app) ,
            subtitle = "" ,
            icon = shiny::icon("cut"),
            color = "aqua",width = 4,fill = F)
  }

})


## normalisation function
to_normalise_reactive <- reactive({
  dataCV_tag <- dataCV_tag_reactive()
  if(!is.null(dataCV_tag)){
    batch_vars_name <- c("machine","day")
    df_to_normalise <-  dataCV_tag  %>%
      ungroup() %>%
      select(slide=.id,sampleID,sample_array_ID,antigen,batch_vars_name,mean_best_CV) %>%
      group_by(sampleID,machine, slide)

    df_to_normalise$sample_index <- group_indices(.data =df_to_normalise )

    ###
    to_normalise <- df_to_normalise %>%
      ungroup() %>% select(-slide,-sampleID,-sample_array_ID) %>%
      select(antigen, machine,day,sample_index, everything()) %>%
      gather(variable, value, -(antigen:sample_index)) %>%
      unite(temp, antigen ) %>%
      select(-variable) %>%
      spread(temp, value)

    row.names(to_normalise) <- to_normalise$sample_index
  }else {
    to_normalise <- NULL
  }
  return(to_normalise)
})

array_matrix_reactive <- reactive({
  dataCV_tag <- dataCV_tag_reactive()
  if(!is.null(dataCV_tag) & !is.null(input$rlm_antigens)){
    batch_vars_name <- c("machine","day")
    df_to_normalise <-  dataCV_tag  %>%
      ungroup() %>%
      dplyr::select(slide=.id,sampleID,sample_array_ID,antigen,batch_vars_name,mean_best_CV) %>%
      group_by(sampleID,machine, slide)

    df_to_normalise$sample_index <- group_indices(.data =df_to_normalise )

    array_matrix <- df_to_normalise %>%
      filter(antigen==input$rlm_antigens[[1]]) %>%
      ungroup() %>%
      select(sample_array_ID,sample_index,slide)
  }else{
    array_matrix <- NULL
  }
  return(array_matrix)
})

## returning non normalised data
non_normalised_list_reactive <- reactive({
  to_normalise <- to_normalise_reactive()
  params_df <- params_display()
  antigens <-   params_df$antigen_list
  antigens <- antigens[antigens %in% names(to_normalise)]
  if(!is.null(to_normalise)){
    matrix_antigen <-  to_normalise %>%
      select(antigens) %>%
      as.matrix(.)
    normalise_list <- matrix_normalise(matrix_antigen=matrix_antigen,
                                       method = "none",
                                       array_matrix=array_matrix,
                                       return_plot = T,
                                       control_antigens=control_antigens)
  }else {
    normalise_list <- NULL
  }


  return(normalise_list)

})

antigens_norm_react <- reactive({
  dataCV_tag <- dataCV_tag_reactive()
  if(!is.null(dataCV_tag)){
    batch_vars_name <- c("machine","day")
    df_to_normalise <-  dataCV_tag  %>%
      ungroup() %>%
      select(slide=.id,sampleID,sample_array_ID,antigen,batch_vars_name,mean_best_CV) %>%
      group_by(sampleID,machine, slide)
    to_normalise_antigens <- c(df_to_normalise$antigen)
  }
  return(to_normalise_antigens)
})

## returning a list of the normalised data
normalised_list_reactive <- reactive({
  to_normalise <- to_normalise_reactive()

  params_df <- params_display()
  antigens <-   params_df$antigen_list
  control_antigens <- input$rlm_antigens
  ## check consistency of names
  #antigens <- antigens[antigens %like% names(to_normalise)]
  antigens <- antigens_norm_react()


  if(!is.null(to_normalise) & input$normalisation_method!="rlm"){
    matrix_antigen <-  to_normalise %>%
    ## check consistency of names
    select(antigens) %>%
      as.matrix(.)

    normalise_list <- matrix_normalise(matrix_antigen=matrix_antigen,
                                       method = input$normalisation_method,
                                       array_matrix=array_matrix,
                                       return_plot = T,
                                       control_antigens=control_antigens)
  }else if(!is.null(to_normalise) & input$normalisation_method=="rlm"){
    array_matrix <- array_matrix_reactive()
    print(row.names(to_normalise))
    matrix_antigen <-  to_normalise %>%
    #  rownames_to_column(.data, var = "rowname") %>%
      ## check consistency of names
    select(antigens) %>%
      as.matrix(.)
  ## check why it looses row names above
  row.names(matrix_antigen) <- row.names(to_normalise)
    normalise_list <- matrix_normalise(matrix_antigen=matrix_antigen,
                                       method = input$normalisation_method,
                                       array_matrix=array_matrix,
                                       return_plot = T,
                                       control_antigens=control_antigens)
  }else {
    normalise_list <- NULL
  }


  return(normalise_list)

})

output$normalised_sd_plot <- renderPlot({
  normalised_list <- normalised_list_reactive()
  if(input$chip_path_param!="") {
    if(!is.null(normalised_list)){
      p <- normalised_list$plot_normalisation
      p

    }else{
      p <- empty_plot_error()
      p
    }
  }else{
    p <- empty_plot()
    p
  }
})


output$normalisation_drop_down <- renderUI({
  dropdownButton(
    tags$h3("List of Normalisation"),
    checkboxInput("log2", "Log2 Normalisation", TRUE),
    checkboxInput("vsn", "VSN", TRUE),
    checkboxInput("cyclic_loess_log", "Cyclic Loess (log)", TRUE),
    checkboxInput("rlm", "RLM Normalisation",FALSE),
    checkboxInput("cyclic_loess", "Cyclic Loess", FALSE)
  )
})


normalised_list_all_reactive <- reactive({
  to_normalise <- to_normalise_reactive()
  params_df <- params_display()
  antigens <-   params_df$antigen_list
  control_antigens <- input$rlm_antigens
  array_matrix <- array_matrix_reactive()
  ## due to different structures
 # antigens <- antigens[antigens %in% names(to_normalise)]
  antigens <- antigens_norm_react()
  matrix_antigen <-  to_normalise %>%
    select(antigens) %>%
    as.matrix(.)

  ## no normalisation
  normalise_list_none <- matrix_normalise(matrix_antigen=matrix_antigen,
                                         method = "none",
                                         array_matrix=array_matrix,
                                         return_plot = T,
                                         control_antigens=control_antigens)
  names(normalise_list_none) <- c("matrix_antigen_none" ,"plot_none")

  ## log normalisation
  if(input$log2==T){
    normalise_list_log <- matrix_normalise(matrix_antigen=matrix_antigen,
                                           method = "log2",
                                           array_matrix=array_matrix,
                                           return_plot = T,
                                           control_antigens=control_antigens)
    names(normalise_list_log) <- c("matrix_antigen_log" ,"plot_log")
  }else if(input$log2==F){
    p <- empty_plot_NS() + ggtitle("Log normalisation")
    normalise_list_log <- list(plot_log=p, matrix_antigen_log=NULL)
  }


  if(input$vsn==T){
    normalise_list_vsn <- matrix_normalise(matrix_antigen=matrix_antigen,
                                           method = "vsn",
                                           array_matrix=array_matrix,
                                           return_plot = T,
                                           control_antigens=control_antigens)
    names(normalise_list_vsn) <- c("matrix_antigen_vsn" ,"plot_vsn")
  }else if(input$vsn==F){
    p <- empty_plot_NS() + ggtitle("VSN normalisation")
    normalise_list_vsn <- list(plot_vsn=p, matrix_antigen_vsn=NULL)
  }


  if(input$cyclic_loess_log==T){
    normalise_list_cyclic_loess_log <- matrix_normalise(matrix_antigen=matrix_antigen,
                                                        method = "cyclic_loess_log",
                                                        array_matrix=array_matrix,
                                                        return_plot = T,
                                                        control_antigens=control_antigens)
    names(normalise_list_cyclic_loess_log) <- c("matrix_antigen_cyclic_loess_log" ,"plot_cyclic_loess_log")

  }else if(input$cyclic_loess_log==F){
    p <- empty_plot_NS() + ggtitle("Cyclic Loess Log")
    normalise_list_cyclic_loess_log <- list(plot_cyclic_loess_log=p, matrix_antigen_cyclic_loess_log=NULL)
  }
  if(input$cyclic_loess==T){
    normalise_list_cyclic_loess <- matrix_normalise(matrix_antigen=matrix_antigen,
                                                    method = "cyclic_loess",
                                                    array_matrix=array_matrix,
                                                    return_plot = T,
                                                    control_antigens=control_antigens)
    names(normalise_list_cyclic_loess) <- c("matrix_antigen_cyclic_loess" ,"plot_cyclic_loess")

  }else if(input$cyclic_loess==F){
    p <- empty_plot_NS() + ggtitle("Cyclic Loess")
    normalise_list_cyclic_loess <- list(plot_cyclic_loess=p, matrix_antigen_cyclic_loess=NULL)
  }


  if(input$rlm==T){
    normalise_list_rlm <- matrix_normalise(matrix_antigen=matrix_antigen,
                                                  method = "rlm",
                                                  array_matrix=array_matrix,
                                                  return_plot = T,
                                                  control_antigens=control_antigens)
    names(normalise_list_rlm) <- c("matrix_antigen_rlm" ,"plot_rlm")

  }else if(input$rlm==F){
    p <- empty_plot_NS() + ggtitle("RLM")
    normalise_list_rlm <- list(plot_rlm=p, matrix_antigen_rlm=NULL)
  }

  normalise_list <- c(normalise_list_none , normalise_list_log,normalise_list_vsn,normalise_list_cyclic_loess_log,
                      normalise_list_cyclic_loess,normalise_list_rlm)
  return(normalise_list)

})


## plot showing all the normalisation techniques
output$mutiple_plot <- renderPlot({
  normalised_list <- normalised_list_all_reactive()
  normalised_list_plot <- normalised_list[grepl("plot",names(normalised_list))]

  p <- do.call("grid.arrange", c(normalised_list_plot, ncol=2))
  p
})



## output for heatmap
output$select_heatmap<- renderUI({
  heat_map_c <- c("Plot both before and after normalisation"=TRUE,
              "Plot only normalised data"=FALSE)
  prettyRadioButtons(inputId="heat_both",
                     label = 'Plot with log or raw MFI:',
                     choices =  heat_map_c,
                     inline=T,
                     selected = FALSE,
                     animation = "jelly",
                     status = "default",
                     shape = "curve",bigger = T)

})

## heatmap plot
output$heatmap_normalised_see <- renderPlot({

  p3 <- pheatmap::pheatmap(cars)
  p3 <-ggplotify::as.ggplot(p3)
  p3 <- p3 +  theme_void()
  return(p3)
})


output$heatmap_normalised <- renderPlot({
  normalised_list <- normalised_list_reactive()
  non_normalised_list_reactive <- non_normalised_list_reactive()
  control_antigens <- input$rlm_antigens
  norm_df <- normalised_list$matrix_antigen_normalised
  norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))


  if(!is.null(normalised_list) & input$normalisation_method=="rlm"){
    norm_df <- normalised_list$matrix_antigen_normalised
    norm_df <- norm_df %>% select(-control_antigens)
    norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))
    non_norm_df <- non_normalised_list_reactive$matrix_antigen_normalised
    non_norm_df <- non_norm_df %>%  select(-control_antigens)
    non_norm_df <- data.frame(apply(non_norm_df, 2, function(x){rescale(x, to =c(0,1))}))

  }else{
    norm_df <- normalised_list$matrix_antigen_normalised
    norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))
    non_norm_df <- non_normalised_list_reactive$matrix_antigen_normalised
    non_norm_df <- data.frame(apply(non_norm_df, 2, function(x){rescale(x, to =c(0,1))}))
  }
  #test sections
  #print(paste(str(norm_df)))
  #print(paste(names(norm_df)))
  #print(paste(colSums(is.na(norm_df))))

  if(input$heat_both==T){
    p_non_norm <- pheatmap::pheatmap(non_norm_df ,
                                     scale = "none",
                                     cluster_rows = F ,
                                     main="Non normalised data",
                                     silent = T)
    p_non_norm <- ggplotify::as.ggplot(p_non_norm)+ theme_void()
    p2 <- pheatmap::pheatmap(norm_df ,scale = "none", cluster_rows = F ,
                             main=paste(input$normalisation_method,"Normalised Data"),
                             silent = T)
    p2 <- ggplotify::as.ggplot(p2) + theme_void()
    #p <- gridExtra::grid.arrange(grobs = list(p2[[4]],p_non_norm[[4]]))
    p <- ggpubr::ggarrange(p_non_norm,p2, nrow = 2)

  }else{
    p3 <- pheatmap::pheatmap(norm_df ,scale = "none", cluster_rows = F,
                            main=paste(input$normalisation_method,"Normalised Data"),
                            silent = T)
    p3 <- ggplotify::as.ggplot(p3)
    p <- p3 +  theme_void()

  }

  return(p)

})


## output for heatmap
output$slider_pca<- renderUI({

  sliderInput(inputId = 'vars_pca', label = 'Select variables to plot on the PCA',
              value = 20,
              min = 10, max = 50)

})


## plot a PCA

output$PCA_normalised <- renderPlot({
  normalised_list <- normalised_list_reactive()
  non_normalised_list_reactive <- non_normalised_list_reactive()
  control_antigens <- input$rlm_antigens
  norm_df <- normalised_list$matrix_antigen_normalised
  norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))

  if(!is.null(input$vars_pca)){
    vars_visualise <- input$vars_pca
  }else{
    vars_visualise=20
  }

  if(!is.null(normalised_list) & input$normalisation_method=="rlm"){
    norm_df <- normalised_list$matrix_antigen_normalised
    norm_df <- norm_df %>% select(-control_antigens)
    norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))
    non_norm_df <- non_normalised_list_reactive$matrix_antigen_normalised
    non_norm_df <- non_norm_df %>%  select(-control_antigens)
    non_norm_df <- data.frame(apply(non_norm_df, 2, function(x){rescale(x, to =c(0,1))}))

  }else{
    norm_df <- normalised_list$matrix_antigen_normalised
    norm_df <- data.frame(apply(norm_df, 2, function(x){rescale(x, to =c(0,1))}))
    non_norm_df <- non_normalised_list_reactive$matrix_antigen_normalised
    non_norm_df <- data.frame(apply(non_norm_df, 2, function(x){rescale(x, to =c(0,1))}))
  }

  ## compute PCA
  res_pca <- prcomp( norm_df, scale = TRUE)
  var <- get_pca_var(res_pca)

  #Visualize the PCA
  p1 <- fviz_pca_ind(res_pca,
                    col.var = "contrib", # Color by contributions to the PC
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE     # Avoid text overlapping
  )+    theme_minimal()

  #p2 <- fviz_cos2(res_pca, choice='var',axes=1:2)
  # Select the top vars_visualise contributing variables
  p2 <-fviz_pca_biplot(res_pca, label="var",
                    select.var = list(contrib = vars_visualise)) +
    theme_minimal()

  # Total cos2 of variables on Dim.1 and Dim.2
  p3 <-     fviz_cos2(res_pca, choice = "var", axes = 1:2 , top = vars_visualise)


  # Color by cos2 values: quality on the factor map
 p4 <-  fviz_pca_var(res_pca, col.var = "cos2",
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               select.var = list(contrib = vars_visualise),
               repel = TRUE # Avoid text overlapping
  )

  p <- gridExtra::grid.arrange(p1,p2,p3,p4, ncol=2 )
  p
})


# function to hide or show or hide tabs with loading
observe({

  tag_file_reactive <- tag_file_reactive()

  req(input$tag_subtract_btn)
  if(is.null(tag_file_reactive) & input$tag_subtract_btn==T){
  hideTab(inputId = "all_tabs", target = "normalise_panel")
  }else  if(input$tag_subtract_btn==F & is.null(tag_file_reactive)){
    showTab(inputId = "all_tabs", target = "normalise_panel")
  }else{
    showTab(inputId = "all_tabs", target = "normalise_panel")
  }
})


#if(input$chip_path_param==""){hideTab(inputId = 'all_tabs',target ='normalise_panel') }

# ### hide tabs
# observe({
#  # hide(selector = "#navbar li a[data-value=normalise_panel]")
#   if(input$chip_path_param==""){hideTab(inputId = 'all_tabs',target ='normalise_panel') }
# })
#
# observeEvent(input$variableXX=='am', {
#   toggle(selector = "#navbar li a[data-value=normalise_panel]")
# })

# output$trial <- renderPlotly({
#   if(input$chip_path_param!="") {
#     ## load the default parameted
#     p <- ggplot(cars,aes(speed,dist)) +geom_jitter()
#     p <- ggplotly(p)
#     p
#
#   }else{
#     p <- empty_plot()
#     p <- ggplotly(p)
#     p
#   }
#
# })
#
#
output$trial2 <- renderPrint({
  print(paste(str(input$tag_antigens), "seeeee", dataCV_tag_reactive()))
})

})

library(shiny)
library(shinyFiles)
library(DT) ## making tables
shinyServer(function(input, output, session) {
   
  wd_this <- getwd()
  volumes = getVolumes()
  shinyDirChoose(input, "folderChoose",  roots = c(home = wd_this), session = session)
  
  ## this works where 
  #sel_path <- reactive({
    #return(print(parseDirPath(volumes, input$folderChoose)))

   # })
  
  sel_path <- reactive({
    return(print(parseDirPath( c(home = wd_this) , input$folderChoose)))
    })
  
  setWorkingDir<-eventReactive(input$folderChoose,{
    setwd(sel_path())
  })
  
  # table of te first file
  output$tbl <-  DT::renderDT({
    folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
    if(input$folderChoose!=""){
    all_datas <- list.files(sel_path() , recursive = T)
    x <- grep("Flags", readLines(  paste0(folder_choose,"/",all_datas[[1]])  ))-1
    d_f <- read.table( paste0(folder_choose,"/",all_datas[[1]]),skip=x, header=TRUE)
    df <- DT::datatable(head(d_f,n=20))
  df
    }
     #options = list(lengthChange = FALSE,
                  #  initComplete = JS('function(setting, json) { alert("done"); }'))
  })
 
  
  ## output all the directory
  ## think of how to load the data and normalise 
 output$dir <- renderUI({
   ## the path
  folder_choose <- parseDirPath(c(home = wd_this) ,input$folderChoose)
   paths <- list.dirs(path =sel_path() , full.names = TRUE)
   paths <- paths[grepl("machine" , paths)]
   
   path_toutput <- gsub(paste0(getwd()),"", paths)
   selectInput('path', 'Select the btch of data to be processed:',  
               path_toutput )
  
 })


 ## empty plot for adding to before data load
empty_plot <- reactive({
   text = paste("\n   Plot will appear after.\n",
                "       selecting folder with data\n",
                "       and data processing done")
   plot_empty <- ggplot() + 
     annotate("text", x = 4, y = 25, size=8, label = text) + 
     theme_bw() +
     theme(panel.grid.major=element_blank(),
           panel.grid.minor=element_blank())
   return(plot_empty)
 })
 

all_files <- reactive({
  pth_full <- paste0(getwd(),input$path)
  pth <-input$path
  path_toutput <- gsub(paste0(getwd()),"", input$path)
  ### list the files in the directory
  filesInDir <- list.files(pth_full)
  return(filesInDir)
})

##total number of files
output$files_in_batch <- shinydashboard::renderValueBox({
   total_files <- length(all_files())
   #flexdashboard::valueBox(
    #paste0("Total files Batch", total_files),
     #total_files, icon = "fa-comments",
    #color = "olive")
   shinydashboard::valueBox(
     paste0(total_files), "Progress", icon = icon("list"),
     color = "olive"
   )
   #shinydashboard::valueBox(total_files, "New Orders", icon = icon("credit-card"))

  #valueBox( "Coss", total_files)
})

output$vbox1 <- renderValueBox({ valueBox( "One","Yes", width = 2, icon = icon("stethoscope"))})
output$vbox2 <- renderValueBox({ valueBox( paste0(1000), width = 2, icon = icon("stethoscope"))})

 ## get the date and machine from selected path 
 date_machine <- reactive({
   pth_full <- paste0(getwd(),input$path)
   pth <-input$path
   filesInDir <- all_files()
   firstFile <- filesInDir[grepl("_first",filesInDir) & !grepl("prescan",filesInDir) ]
   firstFile <- gsub(".txt|.gpr","",firstFile)
   machine <- sub(paste('.+(?=.{', 1, '})', sep=''), '', pth, perl=T)
   
   ### get the date of proccessing
   date_process <-gsub("data/data_with_prescan/","", pth)
   date_process <- gsub("(.+?)(\\/.*)", "\\1", date_process)
   date_process <- gsub("_","",date_process)
   date_process <- gsub("2017","",date_process)
   ## added on shiny only
   date_process <- gsub("\\/","", date_process)
   return(list(firstFile ,machine,date_process ))
 })
 
 
 ## output the processed datas
 output$data_process <- renderPlot({#DT::renderDT
   
   if(input$path!="") {
     pth_full <- paste0(getwd(),input$path)
     pth <-input$path
     
     ## add a date to use within the funciton call
     dp <- gsub("data/data_with_prescan/","", pth)
     dp <- gsub("(.+?)(\\/.*)", "\\1", dp)
     dp <- gsub("_","",dp)
     dp <- gsub("2017","",dp)
     dp <- gsub("\\/","",dp)
     mp <- sub(paste('.+(?=.{', 1, '})', sep=''), '', pth, perl=T)
     mp <- paste("m",mp)
     ## throwing back a recursiver error argument
     
     ## a list holding the date and machine of the 
     date_machine_lst <- date_machine()
     
     ### Define the following
     ## include this path since the data with prescan is put in a different folder
     ## decide not to subtract prescan 
     data_path <- paste0(substring(pth, 6),"/") #"/data_with_prescan/24_07_2017/machine1/"
     
     ##
     # list the files in the selected folder
     #### read in all the datasets
     ### list all the file names under data folder
     filenames <- list.files(file.path(pth_full), 
                             pattern="*.txt$|*.gpr$", full.names=F)
     #all_files <- list.files(pth_full , recursive = T)
     ## Ensure the file with _first is processed first
     ## utlise the soFun package https://github.com/mrdwab/SOfun install https://github.com/mrdwab/overflow-mrdwab first
     ## utlising the order files function on data functions
     filenames <- order_files(filenames=filenames ,suffix_mig= "[Ff][Ii][Rr][Ss][Tt]")
     
     #' @________________________________read_in_the_files_with_the_text_data _______________________________________ 
     ### read all the data files and save them in a list 
     data_files <- return_files(filenames, data_path = data_path)
     
     
     ### 
     ## remove the prescans from the the files
     dfs <- df_names(names_of_files = names(data_files) , 
                     suffix_prescan ="prescan",
                     suffix_mig = "[Ff][Ii][Rr][Ss][Tt]")
     
     
     #' @________________________________merge_data_with_sample_ids_______________________________________ 
     # allData <- merge_datasets(dfs=dfs , data_files=data_files,
     #                           totsamples=21, blockspersample=2 , spotsperblock=192 ,
     #                           machine=NULL , firstFile=NULL , filenames=filenames, date_process=date_process,
     #                           norm_data=F , mp=mp, dp=dp)
     buffers <- buffer_datasets(dfs=dfs,  suffix_prescan ="prescan",
                                data_files=data_files , filenames=filenames , mp=mp , dp=dp) 
     buffers <- plyr::ldply(buffers)
     p <- ggplot(data = buffers,aes(x=antigen, y=F635MedianB635)) + 
       geom_jitter(aes(x=antigen, y=F635MedianB635, color=.id))+
       geom_boxplot(aes(x=antigen, y=F635MedianB635), alpha = 0.2) +
       ggrepel::geom_text_repel(data=filter(buffers, F635MedianB635>5000),aes(label=.id), size=3)+
       theme_classic()
     
     
     print(p)
     
   }else{
     p <- empty_plot()
     print(p)
   }
   
   ## the paths 
   
   
 })
  
})
#########################################################################################################################################################
################## Script for dataformating the multicenter data to get the sample IDS##########################################################################
#######################   Analysing data for the multicenter data   ###########################################################################################
## has different functions for doing the different purposes as commented in teh beginning of each function#########################################################
##**************************    Author: KMwai  ################################################################################################



###___________________________________________________
#' Title -- this returns the list of all the files as data frames
#'
#' @param filenames 
#' @return
#' @export
#'
#' @examples
return_files <- function(filenames,data_path ){
  dfs <- purrr::map(filenames, read_files,data_path=data_path)
  set_names(dfs, purrr::map(filenames, name_of_files))
}



###___________________________________________________
# 
#' Title -- a function to read the chip file
#' @param file_names a list of files to be sorted
#' @return a number of dataframes in the global environment 
#'
#' @export
#' @examples
#' 
#' 
read_array_files <- function(i,data_path,genepix_vars){
  ###loop through all the data to read
  # skip- the number of lines of the data file to skip before beginning to read data ---
  ##the 33 are files that contain the file details
  if(length(grep("Block.*Column|Column.*Block", readLines( file.path(data_path,i)   ))-1) ==1) {
    x <- grep("Block.*Column|Column.*Block", readLines(  file.path(data_path,i)  ))-1
    print(paste0(x,"_",i))
    #d_f <- read.table( file.path(data_path,i),skip=x, header=TRUE)
    d_f <- data.table::fread(file.path(data_path,i),skip=x, header=TRUE)
    ## arrange block to ensure the order is maintaines
    d_f <- d_f %>% arrange(Block)
    ## calculate the different background  methods
    if(paste0(genepix_vars$BG) %ni% names(d_f)){
      genepix_vars$BG <- names(d_f)[names(d_f) %like% paste0(genepix_vars$BG,'$')]
    }
    
    if(paste0(genepix_vars$FG) %ni% names(d_f)){
      genepix_vars$FG<- names(d_f)[names(d_f) %like% paste0(genepix_vars$FG,'$')]
    }
    d_f <- d_f %>% 
      mutate(global_BGMedian=median(!!genepix_vars$BG,na.rm = T)) %>% 
      ## minimum BG per block and >1 -- Moving minimum approach per block
      group_by(Block) %>% 
      mutate(minimum_BGMedian=minpositive(!!genepix_vars$BG)) %>% 
      ungroup()
      
    
}  else stop(paste0("File " , i," does not have Block Column Specification"))
  
}


#___________________________________________________
# Format and group bi ID & Antigen Name
# Transpose dataset and assign corresponding sample ID
# subtract the prescan 

#'         \\\_Start_Function_Extract_Data\\\         #
#'         
#'         
#'         
merge_datasets <- function( dfs,filenames , data_files ,totsamples, blockspersample ,sampleID_path,bg=F){
  if(bg==T){
    data1_transp <- purrr::map(.x=dfs, .f=transp_bg,
                               totsamples=totsamples,blockspersample=blockspersample,
                               sampleID_path=sampleID_path,data_files=data_files )
  }else{
    data1_transp <- purrr::map(.x=dfs, .f=transp,
                               totsamples=totsamples,blockspersample=blockspersample,
                               sampleID_path=sampleID_path,data_files=data_files )
  }

  
  data1_transp <- set_names(data1_transp, purrr::map(filenames, name_of_files))
  return(data1_transp)
}
#'         \\\_End_function\\\         #
#'         
#'      


### Background 
extract_bg <- function(iden,data_files , genepix_vars=genepix_vars)
{
  ## read in the sample ID files
  ## this can be pulled from a mysql table

  
  
  if(file.exists(paste0(genepix_vars$sampleID_path,iden ,".csv"))){
   # arraynames <- read.csv(paste0(genepix_vars$sampleID_path,iden ,".csv") ,
                          # header = T , stringsAsFactors = F , colClasses="character")
    arraynames <- read.csv(paste0(genepix_vars$sampleID_path,"/",iden ,".csv") ,
                           header = T , stringsAsFactors = F , colClasses="character")
  }else{
    warning(paste0(iden, " Not found in the sampleID files"))
    arraynames <- data.frame(v1=(1:genepix_vars$totsamples) , v2=paste0("SID_gen",1:genepix_vars$totsamples),barcode=iden)
    
  }
  ## replicate to the number of blocks
  ## make sure the block is arranged before merging with the data file
  arraynames <- arraynames %>% 
    dplyr::select(v1,v2) %>% 
    arrange(as.numeric(v1))
  
  ## capture errors for same sample ID in a slide
  if(length(unique(arraynames$v2)) <genepix_vars$totsamples) { 
    sink("errors/error_replicates.txt" , append = T)
    print(paste0("Most likely there is a repeated sample name for " , iden))
    sink()
    arraynames <- arraynames %>% 
      group_by(v2) %>% 
      mutate(index=1:n()) %>% 
      mutate(v2=ifelse(index>1,paste0(v2,"_",index),v2)) %>% 
      select(-index)
  }
  
  
  ## get the data from the loop
  ## extract the specific data from the data files
  data <- data_files[[iden]]
  
  ## pick the spots per block from the data file
  spotsperblock <- table(data$Block)[[1]]
  sampleID <- rep(arraynames$v2,each=spotsperblock*genepix_vars$blockspersample)
  
  
  
  Data1 <- data %>% 
    # assign respective sample number to each row
    mutate(sample = rep(1:genepix_vars$totsamples,each=spotsperblock*genepix_vars$blockspersample),
           # Bring in the sampleIDs..192 each sample
           sampleID = sampleID,
           # Abit of formating the Antigen names and concentration
           Name= gsub(':','',Name),
           Name= gsub('\n','',Name),
           Name= gsub(' ','',Name))  %>% 
    ##remove uneccessary concs
    ## filter(!grepl('Landmark|Buffer|IgG', Name))   %>%
    # group by iden and antigen name
    group_by(sampleID,Name)
  if(paste0(genepix_vars$BG) %ni% names(Data1)){
    genepix_vars$BG <- names(Data1)[names(Data1) %like% paste0(genepix_vars$BG,'$')]
  }
  
  if(paste0(genepix_vars$FG) %ni% names(Data1)){
    genepix_vars$FG<- names(Data1)[names(Data1) %like% paste0(genepix_vars$FG,'$')]
  }
  
  #----------------------------------------------------------------------------------------------------
  ##save the MFI values of the Background
  data1_bg <- Data1 %>% dplyr::select( sampleID, antigen=Name,Block,FBG_Median=!!genepix_vars$FG,BG_Median=!!genepix_vars$BG) %>% 
    mutate(replicate = 1:n() ) %>%    
    filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   
  # %>%     spread(antigen, F635Median)
  # combine Name and replicate
  ## save in the background data folder files with higher background values
  create_dir(path = "data/raw_BG/")
  write_csv(data1_bg ,paste0("data/raw_BG/",iden,"_rawBG.csv"))
  #----------------------------------------------------------------------------------------------------
  return(data1_bg)
}


## plot background
#' Title
#'
#' @param df 
#' @param antigen_name 
#' @param bg_MFI 
#' @param log_mfi 
#'
#' @return
#' @export
#'
#' @examples
plot_bg <- function(df, x_axis="antigen",bg_MFI="B635_Median",
                    log_mfi=T){
  ## create an id to help in having a numeric sample ID to sort your data 
  ## this is because all sampleIDs from the samples were not unique
  ## rename the original sampleID sampleID2
  bg_MFI_sys <- rlang::sym(bg_MFI) 
  bg_plot <- df %>%
    rename(slide=.id) %>% 
   # rename(sampleID2=sampleID) %>% 
  #  group_by(sampleID2, slide) %>% 
    mutate(log_bg =    log2(!!bg_MFI_sys))
  
 # bg_plot$sampleID <- group_indices(.data =bg_plot )
  
  if(log_mfi==T){
    p_pubr <- ggboxplot(data = bg_plot , 
                        x=x_axis , y="log_bg",
                        facet.by = "replicate",ncol=1)
    p_pubr <- ggpar(p_pubr,font.tickslab = c(6,"#993333"),
                    xtickslab.rt = 45 , ylab = "Background of the replicates (log2)")
    p_bg <- ggplot(data = bg_plot ,aes_string(  x=x_axis , y="log_bg"))+
      geom_boxplot()+
      facet_wrap(~replicate, ncol=1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1)) 
    
     
    
  }else if(log_mfi==F){
    p_pubr <- ggboxplot(data = bg_plot , 
              x=x_axis , y=bg_MFI,
              facet.by = "replicate",ncol=1)
    p_pubr <- ggpar(p_pubr,font.tickslab = c(6,"#993333"),
          xtickslab.rt = 45 , ylab = "Background of the replicates raw") 
   
    p_bg <- ggplot(data = bg_plot ,aes_string(  x=x_axis , y=bg_MFI))+
      geom_boxplot()+
      facet_wrap(~replicate, ncol=1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1)) 
  }else {
    # p_pubr <- ggboxplot(data = bg_plot , 
    #                     x="antigen" , y=bg_MFI,
    #                     facet.by = "replicate",ncol=1)
    # p_pubr <- ggpar(p_pubr,font.tickslab = c(8,"#993333"),
    #                 xtickslab.rt = 45 , ylab = "Background of the replicates raw") 
    
    p_bg <- ggplot(data = bg_plot ,aes_string(  x=x_axis , y=bg_MFI))+
      geom_boxplot()+
      facet_wrap(~replicate, ncol=1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1)) 
  }
  return(p_pubr)
}

#___________________________________________________

#' Title
#'
#' @param df 
#' @param antigen_name 
#' @param bg_MFI 
#' @param FG_MFI 
#' @param log_mfi 
#'
#' @return
#' @export
#'
#' @examples
plot_FB <- function(df, antigen_name="antigen",bg_MFI="BG_Median",FG_MFI="FBG_Median",
                    log_mfi=F){
  ## create an id to help in having a numeric sample ID to sort your data 
  ## this is because all sampleIDs from the samples were not unique
  ## rename the original sampleID sampleID2
  bg_MFI_sys <- rlang::sym(bg_MFI) 
  FB_MFI_sys <- rlang::sym(FG_MFI)
  bg_plot <- df %>%
    rename(slide=.id) %>% 
    mutate(log_bg =log2(!!bg_MFI_sys),
           log_fb=log2(!!FB_MFI_sys))
  
  
  
  if(log_mfi==T){
    
    
    p <- ggplot(bg_plot , aes(log_fb,log_bg,
                              text = paste("Antigen: ", antigen,
                                           "<br>FG: $", FBG_Median,
                                           "<br>B: $", BG_Median))) +
      xlab("Foreground MFI") + ylab("Background MFI") +
      geom_jitter() +
      theme_light()
    
    
    
  }else if(log_mfi==F){
    
    p <- ggplot(bg_plot , aes(FBG_Median,BG_Median,
                              text = paste("Antigen: ", antigen,
                                           "<br>FG: $", FBG_Median,
                                           "<br>B: $", BG_Median))) +
      xlab("Foreground MFI") + ylab("Background MFI") +
      geom_jitter() +
      theme_light()
  }else {
    p <- ggplot(bg_plot , aes(FBG_Median,BG_Median,
                              text = paste("Antigen: ", antigen,
                                           "<br>FG: $", FBG_Median,
                                           "<br>B: $", BG_Median))) +   
      xlab("Foreground MFI") + ylab("Background MFI") +
      geom_jitter() +
      theme_light()
  }
  return(p)
}



#' Title
#'
#' @param iden 
#' @param Data1 
#'
#' @return
#' @export
#'
#' @examples
bg_correct <- function(iden,Data1,genepix_vars,method="subtract_local"){
  #----------------------------------------------------------------------------------------------------
  
  if(paste0(genepix_vars$BG) %ni% names(Data1)){
    genepix_vars$BG <- names(Data1)[names(Data1) %like% paste0(genepix_vars$BG,'$')]
  }
  
  if(paste0(genepix_vars$FBG) %ni% names(Data1)){
    genepix_vars$FBG<- names(Data1)[names(Data1) %like% paste0(genepix_vars$FBG,'$')]
  }
  ##save the MFI values without formating the background
  data1_full_bg <- Data1 %>%
    dplyr::select(sampleID, antigen=Name,FMedian=!!genepix_vars$FG) %>% 
    mutate(replicate = 1:n()) %>% 
    ## removing Land mark and Buffer
    filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   
  # %>%     spread(antigen, F635Median)
  # combine Name and replicate
  ## save in the background data folder files with higher background values
  create_dir(path = "processed_data/raw_MFI_BG/")
  write_csv(data1_full_bg ,paste0("processed_data/raw_MFI_BG/",iden,"_raw_MFI_BG",".csv"))
  #----------------------------------------------------------------------------------------------------
  
  if(method=="none"){
    #----------------------------------------------------------------------------------------------------
    ##MFI values without subtracting the background
    Data1 <- Data1 %>% 
      dplyr::select( sampleID,sample_array_ID, antigen=Name,FMedian=!!genepix_vars$FG,BGMedian=!!genepix_vars$BG,FMedianBG_correct=!!genepix_vars$FG,
                    Block, Column, Row) %>% 
      mutate(replicate = 1:n()) #%>%    
      
    #----------------------------------------------------------------------------------------------------
    
  }else if(method=="subtract_local"){
    ## this approach subracts the local backgroud estimated by the Array Jet Machine
    #----------------------------------------------------------------------------------------------------
    ##save the MFI values with subtracting the background

    Data1 <- Data1 %>% 
      mutate(FMedianBG_correct=!!genepix_vars$FG - !!genepix_vars$BG) %>% 
      dplyr::select( sampleID,sample_array_ID,antigen=Name,FMedian=!!genepix_vars$FG,BGMedian=!!genepix_vars$BG, FMedianBG_correct, Block, Column, Row) %>% 
      mutate(replicate = 1:n()) #%>% 
   
     # filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   
    #----------------------------------------------------------------------------------------------------
    
  }else if(method=="subtract_global"){
    ## this approach subracts the median of the backgrounds in a slide
    #----------------------------------------------------------------------------------------------------
    ##save the MFI values with subtracting the background
    Data1 <- Data1 %>% 
      mutate(FMedianBG_correct=!!genepix_vars$FG-global_BGMedian) %>% 
      dplyr::select( sampleID, sample_array_ID,antigen=Name,FMedian=!!genepix_vars$FG,BGMedian=!!genepix_vars$BG,FMedianBG_correct,Block, Column, Row) %>% 
      mutate(replicate = 1:n()) #%>%    
     # filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   
    #----------------------------------------------------------------------------------------------------
  }else if(method=="movingmin_bg"){
    ## this is subtracte 
    Data1 <- Data1 %>% 
      mutate(FMedianBG_correct=!!genepix_vars$FG-minimum_BGMedian) %>% 
      dplyr::select( sampleID,sample_array_ID, antigen=Name,FMedian=!!genepix_vars$FG,BGMedian=!!genepix_vars$BG,FMedianBG_correct,Block, Column, Row) %>% 
      mutate(replicate = 1:n())# %>%    
     # filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))  
    
  }else if(method=="minimum_half"){
    ## this approach ensures all the MFI values are positive
    ## if the MFI <0 after subtraction the MFI is set to the half of the minimum corrected intenisities
    #----------------------------------------------------------------------------------------------------
    ##save the MFI values with subtracting the background
    Data1 <- Data1 %>% 
      mutate(FMedianBG_correct=!!genepix_vars$FG - !!genepix_vars$BG) %>% 
      dplyr::select( sampleID, sample_array_ID,antigen=Name,FMedian=!!genepix_vars$FG,BGMedian=!!genepix_vars$BG,FMedianBG_correct,Block, Column, Row) %>% 
      group_by(Block) %>% 
      mutate(FMedianBG_correct=ifelse(FMedianBG_correct<0.1,(minpositive(FMedianBG_correct)/2),FMedianBG_correct)) %>%
      group_by(sampleID,antigen) %>% 
      mutate(replicate = 1:n()) #%>%    
    #  filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   
    #----------------------------------------------------------------------------------------------------
  }else if(method=="edwards"){
    #a log-linear interpolation method is used to adjust lower intensities as in Edwards (2003).
    Data1 <- Data1 %>% 
      mutate(FMedianBG_correct=!!genepix_vars$FG-!!genepix_vars$BG)
    one <- matrix(1, nrow(Data1), 1)
    delta.vec <- function(d, f = 0.1) {
      quantile(d, mean(d < 1e-16, na.rm = TRUE) * (1 + f), na.rm = TRUE)
    }
    #delta <- one %*% apply(as.matrix(Data1[['FMedianBG_correct']]), 2, delta.vec)
    ## no need to multiply with 1 since its returning the same value and we want to implement in a data frame
    delta <-  apply(as.matrix(Data1[['FMedianBG_correct']]), 2, delta.vec)
    Data1 <- Data1 %>% 
      dplyr::select( sampleID, sample_array_ID,antigen=Name,FMedian=!!genepix_vars$FG, 
                     FMedianBG_correct,BGMedian=!!genepix_vars$BG,Block, Column, Row) %>% 
      group_by(Block) %>% 
      mutate(FMedianBG_correct=ifelse(FMedianBG_correct<delta,
                                      (delta * exp(1 - (BGMedian + delta)/FMedian)),FMedianBG_correct)) #%>% 
      #select(-c(BGMedian))
    
  }else if(method=="normexp"){
    ##a convolution of normal and exponential distributions is fitted to the foreground intensities using 
    #the background intensities as a covariate, and the expected signal given the observed foreground becomes 
    #the corrected intensity. This results in a smooth monotonic transformation of the background subtracted 
    #intensities such that all the corrected intensities are positive.
    ##Both norm exp and edwards are implemented in Limma for DNA micro array data
  }
  #Data1 <- Data1 %>% rename(F635MedianB635=F635.Median...B635)
  return(Data1)
}




#' Title
#'
#' @param iden 
#' @param data_pre 
#' @param totsamples 
#' @param blockspersample 
#' @param spotsperblock 
#' @param machine 
#' @param firstFile 
#' @param migfiles 
#' @param date_process 
#' @param norm_data 
#' @param data_files 
#' @param subtract_prescan 
#'
#' @return
#' @export
#'
#' @examples
merge_sampleID <- function(iden,data_files,genepix_vars,method)
{
  ## read in the sample ID files
  ## this can be pulled from a mysql table
  if(file.exists(paste0(genepix_vars$sampleID_path,"/",iden ,".csv"))){
    print(paste0(iden, "sampleID file found"))
    arraynames <- read.csv(paste0(genepix_vars$sampleID_path,"/",iden ,".csv") ,
                           header = T , stringsAsFactors = F , colClasses="character")
  }else{
    warning(paste0(iden, " Not found in the sampleID files",genepix_vars$sampleID_path,"/",iden ,".csv"))
    arraynames <- data.frame(v1=(1:genepix_vars$totsamples) , v2=paste0("SID_gen",1:genepix_vars$totsamples),barcode=iden)
    
  }
  
  ## replicate to the number of blocks
  ## make sure the block is arranged before merging with the data file
  arraynames <- arraynames %>% 
    dplyr::select(v1,v2) %>% 
    arrange(as.numeric(v1))
  
  ## capture errors for same sample ID in a slide
  if(length(unique(arraynames$v2)) <genepix_vars$totsamples) { 
    sink("errors/error_replicates.txt" , append = T)
    print(paste0("Most likely there is a repeated sample name for " , iden))
    sink()
    arraynames <- arraynames %>% 
      group_by(v2) %>% 
      mutate(index=1:n()) %>% 
      mutate(v2=ifelse(index>1,paste0(v2,"_",index),v2)) %>% 
      select(-index)
  }
  
  
  ## get the data from the loop
  ## extract the specific data from the data files
  data <- data_files[[iden]]
  
  ## pick the spots per block from the data file
  spotsperblock <- table(data$Block)[[1]]
  sampleID <- rep(arraynames$v2,each=spotsperblock*genepix_vars$blockspersample)
  sample_array_ID <- rep(arraynames$v1,each=spotsperblock*genepix_vars$blockspersample)
  
  
  Data1 <- data %>% 
    # assign respective sample number to each row
    mutate(sample = rep(1:genepix_vars$totsamples,each=spotsperblock*genepix_vars$blockspersample),
           # Bring in the sampleIDs..192 each sample
           sampleID = sampleID,
           sample_array_ID=sample_array_ID,
           # Abit of formating the Antigen names and concentration
           Name= gsub(':','',Name),
           Name= gsub('\n','',Name),
           Name= gsub(' ','',Name))  %>% 
    ##remove uneccessary concs
    ## filter(!grepl('Landmark|Buffer|IgG', Name))   %>%
    # group by iden and antigen name
    group_by(sampleID,Name)
  
  ## DO the background correction
  ## specify the 
  Data1 <- bg_correct(iden, Data1 ,genepix_vars, method=method)
  
  Data1 <-Data1 %>%  mutate(iden=iden)
  return(Data1)
}

#___________________________________________________


#'         \\\_Start_Function_\\\         #
#'
#' @param dataC 

cv_estimation <- function(dataC  ,lab_replicates, cv_cut_off){
  if(lab_replicates==1){
    dataC <- dataC %>% 
      dplyr::filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))  %>%
      filter(replicate<=lab_replicates)  %>%
      # combine Name and replicate
      tidyr::unite(col=antigen,antigen,replicate) 
    dataC <- dataC %>% 
      dplyr::select(sampleID,sample_array_ID, antigen,iden,FMedianBG_correct) 
    ## get the name of the file
    iden <-unique(dataC$iden)
    # pick the values after the last underscore
    ## to get the replicate
    dataC$replicate <- sub(".*_(.*)", "\\1", dataC$antigen)
    
    ## create a wide data to
    sink("log_file.txt" , append = T)  
    if(length(unique(dataC$replicate ))>lab_replicates)  {
      try( stop(paste0("Some antigens seems to be repeated in a mini array for", iden)) , outFile = stdout())
      error_replicates(iden)
    }
    sink()
    
    dataC <- dataC %>% 
      select(antigen, sampleID, sample_array_ID,everything())
    
  }else if(lab_replicates>1){
  ## Exclude the land mark and Buffer
  dataC <- dataC %>% 
    dplyr::filter(!grepl('^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))  %>%
    filter(replicate<=lab_replicates)  %>%
    # combine Name and replicate
    tidyr::unite(col=antigen,antigen,replicate) 
  dataC <- dataC %>% dplyr::select(sampleID,sample_array_ID, antigen,iden,FMedianBG_correct) 
  ## get the name of the file
  iden <-unique(dataC$iden)
  # pick the values after the last underscore
  ## to get the replicate
  dataC$replicate <- sub(".*_(.*)", "\\1", dataC$antigen)
  ### pick the nname of the antigen
  ## values until the last underscore
  dataC$antigen <- sub("\\_[^\\_]*$" , "", dataC$antigen)
  
  ## create a wide data to
  sink("log_file.txt" , append = T)  
  if(length(unique(dataC$replicate ))>lab_replicates)  {
    try( stop(paste0("The replicates per antigen per sample are more than expected for ", iden)) , outFile = stdout())
    error_replicates(iden)
  }else(print("The replicates are as expected per sample per antigen"))
  sink()
  
  ## reshaping the data
  Data3 <- dataC %>%  
    dplyr::select(sampleID,sample_array_ID, FMedianBG_correct,iden, replicate,antigen) %>% 
    spread(replicate, FMedianBG_correct) 
  
  
  ### group the antigens and calculate their CV values
  ## the CV values > 20, get the minimum mean of each of the two mfi values and use it to calculate CV
  ## select the one with lowest CV
  dataC <- dataC %>% 
    group_by(antigen , sampleID) %>% 
    ###mean and then for each grouping of 2
    summarize(meanX=mean(FMedianBG_correct , na.rm=T),
              meanX2_X3=mean(FMedianBG_correct[-1] , na.rm=T),
              meanX1_X3=mean(FMedianBG_correct[-2] , na.rm=T),
              meanX1_X2=mean(FMedianBG_correct[-3] , na.rm=T),
              ###standard deviation and then for each grouping of 2
              sdX=round(sd(FMedianBG_correct , na.rm=T),2),
              sdX2_X3=round(sd(FMedianBG_correct[-1] , na.rm=T),2),
              sdX1_X3=round(sd(FMedianBG_correct[-2] , na.rm=T),2),
              sdX1_X2=round(sd(FMedianBG_correct[-3] , na.rm=T),2),
              ### cv
              CVX=(round(sdX/meanX,4))*100 ,
              CVX2_X3=(round(sdX2_X3/meanX2_X3,4))*100 ,
              CVX1_X3=(round(sdX1_X3/meanX1_X3,4))*100 ,
              CVX1_X2=(round(sdX1_X2/meanX1_X2,4))*100) %>% 
    mutate(cvCat_all = ifelse(CVX>0 & CVX<=cv_cut_off , paste0("CV <= ",cv_cut_off),
                          ifelse(CVX >cv_cut_off & CVX <101, paste0("CV > ",cv_cut_off),"Others")))   %>% 
    mutate(cvSelected_all = ifelse(CVX>0 & CVX<=cv_cut_off , CVX,
                               ifelse(CVX >cv_cut_off | CVX<0 ,pmin(CVX2_X3, CVX1_X3,CVX1_X2, na.rm = T),NA)))
  
  
  dataC <- dataC %>% 
    left_join(Data3, by=c("sampleID","antigen")) %>% 
    select(antigen, sampleID, sample_array_ID,everything())
  }
  return(dataC)
}


#'         \\\_Start_Function_\\\         #
#'         
#' 
best_CV_estimation <- function(dataCV,slide_id, lab_replicates, cv_cut_off) {
  if(lab_replicates>1){
  ### Get the mean that corresponds to the lowest CV
  iden <- unique(dataCV[[slide_id]])
  data_best_CV <- as.data.frame(dataCV)
  
  ## changing NaN values to 0 to facilitate computation
  is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))
  data_best_CV[is.nan.data.frame(data_best_CV)] <- 0
  
  ## get the minumum cv and put the value of the as a string variable
  ## ie CVX3 selected meanX3 value will be created
  ## might bring issues here if the subtraction has a NA or its missing for the prescan
  data_best_CV <- data_best_CV %>%  
    mutate(x=colnames(.[,c("CVX2_X3","CVX1_X3","CVX1_X2")])
           [apply(.[,c("CVX2_X3","CVX1_X3","CVX1_X2")],1,which.min)])  %>% 
    dplyr::mutate(xbar=paste0("meanX", gsub("CVX", "", x))) 
  
  ## get the actual value of the mean that corresponds to that
  #http://stackoverflow.com/questions/43762869/get-row-value-corresponding-to-a-column-name
  data_best_CV <- data_best_CV %>%
    mutate(row=1:n()) %>% 
    # select(-c(`1`,`2`,`3`,iden)) %>% 
    gather(prop, val, meanX1_X2:meanX2_X3) %>% 
    group_by(row) %>% 
    mutate(selected=val[xbar==prop]) %>%
    spread(prop, val) %>% dplyr::select(-row)  
  
  
  ##create the final selected mean in the data set
  data_best_CV <-  data_best_CV %>% 
    mutate(meanSelected = ifelse(CVX>=0 & CVX<=cv_cut_off , meanX,
                                 ifelse((CVX >cv_cut_off & CVX <101)  | CVX<=0 ,selected,NA)),
           mean_best_CV =selected) %>% 
    dplyr::select(-xbar,-selected)
  data_best_CV <-  data_best_CV %>% 
    mutate(best_CV = pmin(CVX2_X3,CVX1_X3,CVX1_X2, na.rm = T) ,
           best_CV_cat  = ifelse(best_CV>=0 & best_CV<=cv_cut_off , paste0("CV <= ",cv_cut_off),
                             ifelse(best_CV >cv_cut_off & best_CV<101, paste0("CV > ",cv_cut_off),"Others")))
  
}else if(lab_replicates<2){
  ### Get the mean that corresponds to the lowest CV
  iden <- unique(dataCV[[slide_id]])
  data_best_CV <- as.data.frame(dataCV)
  
  ## changing NaN values to 0 to facilitate computation
  is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))
  data_best_CV[is.nan.data.frame(data_best_CV)] <- 0
  
  ### generate the variabrl
  data_best_CV <- data_best_CV %>%  
    mutate(mean_best_CV=FMedianBG_correct)
}
  ## return the dataset of interest
  return(data_best_CV)
} 
#'      
#'      
#'         \\\_End_Function_\\\         #
#'         
#'         




#'         \\\_Start_Function\\\         #
#'
#'
#' @param iden 
#' @param dataCV 
cv_by_sample_estimation <- function(dataCV,cv_variable,lab_replicates ) {
  ## creating a summary of the CV's by sampleID for each file
  ## helps in identifying samples with a high CV value
  if(lab_replicates>1){
  iden <- unique(dataCV$iden)
  dataC_cvSample <-  dataCV %>%
    group_by(sampleID, get(cv_variable)) %>% 
    summarise(n= n()) %>% 
    mutate(perc = round((n / sum(n))*100,2)) %>% 
    rename(cvCat='get(cv_variable)') %>% 
    ungroup() %>%
    gather(variable, value, -c(sampleID, cvCat)) %>% 
    unite(temp, cvCat, variable) %>%
    tidyr::spread(temp, value, fill=0) 
  }else{
    dataC_cvSample <- NULL
    warning("The experiment is specified not to have lab replicates")
  }
  return(dataC_cvSample)
}
#'  
#'         
#'          \\\_End_function\\\         #
#'         
#' @param tag_antigens 
#' @param mean_best_CV_var 
#' @param dataC_mfi 
#' @param tag_file 



tag_subtract <- function(dataC_mfi,tag_antigens, mean_best_CV_var,tag_file, batch_vars){
  mean_best_CV_var= rlang::sym(paste0(mean_best_CV_var))
  ## remove the tag for the data
  
  dataC_mfi_tags <- dataC_mfi %>% 
    dplyr::filter(antigen %in% tag_antigens) %>% 
    ungroup() %>% 
    dplyr::select(sampleID, antigen , !!mean_best_CV_var) %>% 
    ## july 2019 --> agreed to change all the negative TAG values to zero 
    mutate(!!mean_best_CV_var:=ifelse(!!mean_best_CV_var<0,0,!!mean_best_CV_var)) %>% 
    spread(antigen , !!mean_best_CV_var) 
  
  ## join the data with the fused antigen name for subtraction
  dataC_mfi <- left_join(dataC_mfi , tag_file, by="antigen")
  
  dataC_mfi <- dataC_mfi %>% ungroup() %>%  
    dplyr::select(sampleID ,sample_array_ID, antigen , TAG, everything() , -row)
  dataC_mfi <- dataC_mfi %>% ungroup() %>%   
    left_join(x =dataC_mfi ,y=dataC_mfi_tags , by="sampleID") %>% 
    mutate(TAG_mfi=NA)
  vars_in <- names(dataC_mfi)
  
  
  ## mutat the tag_mfi variable
  for(i in tag_antigens){
    tag_var <- rlang::sym(paste0(i))
    dataC_mfi <- dataC_mfi %>% 
      mutate(TAG_mfi =ifelse(TAG_name==i & is.na(TAG_mfi),!!tag_var,TAG_mfi))
  }
  
  ## subtract the TAG values depending on the TAG of 
  mean_best_CV_tag_var <- rlang::sym(paste0(mean_best_CV_var,"_tag"))
  dataC_mfi <- dataC_mfi %>% 
    mutate(TAG_mfi=ifelse(is.na(TAG_mfi),0,TAG_mfi)) %>% 
    mutate(!!mean_best_CV_tag_var := !!mean_best_CV_var- TAG_mfi)  %>% 
    dplyr::select(c(vars_in, paste(mean_best_CV_tag_var))) %>% 
    ## add the batch variables to the data 
    mutate(machine=batch_vars[["machine"]] , day=batch_vars[["day"]]) 
  return(dataC_mfi)
}

#########################################################################################################################################################
################## Script for data merging for the CHIP data with the plate samples ##########################################################################
##**************************    Author: KMwai (May 2020)  ################################################################################################
rm(list = ls())
library(pacman)
pacman::p_load(tidyverse,ggpubr, data.table)

## parameters here
chip_path <- "data/array_data"
## here the files are saved as CSV initially
## v1 which is the mini array ID
## v2 which is the sample name in each miniarray
sampleID_path <- "data/array_sampleID/"
##mig file prefix
mig_prefix <- "_first"

## machine of hybiridization
machine <- 1
### get the date of proccessing
date_process <- "0520"
dp <-  "0520"
mp <- "m1"


## specify the number of samples in a single arrray
totsamples <- 21

## how many blocks does each sample take
blockspersample <- 2

if(!file.exists(paste0("images/"))) {
  dir.create(paste0("images/") , recursive = T)
}

## specify the wavelength 635 --> blue channel
#---------------------------------------------------
## https://www.moleculardevices.com/products/additional-products/genepix-microarray-systems-scanners
channel <- "635"

## specify the the parameters to process the data
genepix_vars <- array_vars(channel="635" ,
                           chip_path = "data/array_data",
                           totsamples = 2,
                           blockspersample = 1,
                           sampleID_path = "data/array_sampleID/",
                           mig_prefix = "_first",
                           machine =1,
                           date_process = "0520")



## check 
check_sampleID_files(genepix_vars)




### list the files in the directory
filesInDir <- list.files(genepix_vars$paths[[3]])
#migFile <- filesInDir[grepl(mig_prefix,filesInDir)]
#migFile <- gsub(".txt","",migFile)

## date and machine specification

### Define the following
## include this path since the data with prescan is put in a different folder
## decide not to subtract prescan 
#data_path <- paste0(substring(pth, 5),"/") 


### Get the MIG files of the day
# Load the datasets of interest
## keep the path until the folder that holds the array data
pth_files <- sub("\\/[^\\/]*$" , "", pth)
filenamesMIG <- list.files(file.path(paste0(pth_files,"/")), 
                           pattern=paste0("*",mig_prefix,".*txt$"), full.names=F , recursive = T)


#### read in all the datasets
### list all the file names under data folder
filenames <- list.files(file.path(genepix_vars$paths[[3]]), 
                        pattern="*.txt$|*.gpr$", full.names=F)



#' @________________________________read_in_the_files_with_the_text_data_from_the_chip_______________________________________ 
### read all the data files and save them in a list 
data_path <- paste0(genepix_vars$paths[[3]],"/") 
data_files <- purrr::map(.x = filenames, 
                         .f = read_array_files,
                         data_path=data_path ,
                         genepix_vars=genepix_vars)


data_files <- set_names(data_files, purrr::map(filenames, name_of_files))

dfs <- names(data_files) 
###

#' @________________________________plot_the_background_and_decide_______________________________________ 
## specify the number of samples in a 
allData_bg <- purrr::map(.x=dfs, .f=extract_bg,data_files=data_files,genepix_vars)
allData_bg <- set_names(allData_bg, purrr::map(filenames, name_of_files))
allData_bg <- plyr::ldply(allData_bg)


create_dir("images")
plot_bg(df=allData_bg, antigen_name="antigen",bg_MFI="BG_Median",
        log_mfi=T) +
  ggsave(paste0("images/bg_",sub('.*/\\s*', '', pth),".png") , width = 24 , height = 15) 
plot_bg(df=allData_bg, antigen_name="antigen",bg_MFI="BG_Median",
        log_mfi=F) + 
  ggsave(paste0("images/bg_",sub('.*/\\s*', '', pth),"_raw.png") , width = 24 , height = 15) 


#' @________________________________merge_data_with_sample_ids_______________________________________ 
## specify the number of samples in a 
## if the loacl bg is T the local background per spot is subtracted
##
## merge with sampleID and specify the method of background correction
sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files , 
                             genepix_vars, method="subtract_local")

sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))

all_df <- plyr::ldply(sample_ID_merged_dfs)
all_df <- all_df %>% 
  select(antigen,FMedian,FMedianBG_correct) %>% 
  gather(var,mfi,-antigen)

ggplot(all_df,aes(x=antigen, y=mfi))+
  geom_boxplot()+
  facet_wrap(~var, nrow = 2)+
  geom_hline(yintercept = 0, color='red')+
  theme_light()+
  ylab("MFI") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_rect(fill="black",size = 3),
        strip.text.x = element_text(size=12, color="white",
                                    face="bold.italic")) 





### check your buffer perfomance
buffer_transp <- purrr::map(.x=sample_ID_merged_dfs, .f=buffer_spots)

buffer_transp <- set_names(buffer_transp, purrr::map(filenames, name_of_files))

buffers <- plyr::ldply(buffer_transp)
plot_buffer(buffers,buffer_names="antigen",buffer_mfi="FMedianBG_correct",slide_id=".id")





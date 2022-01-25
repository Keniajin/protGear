## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
library(ggpubr)
library(gtools)
library(purrr)
library(scales)
library(pheatmap)
library(data.table)
library(kableExtra)
library(gridExtra)
library(png)
library(knitr)
library(grid)
library(styler)
library(FactoMineR)
library(factoextra)
library(magick)
library(rlang)
library(GGally)
library(ggplotify)
library(remotes)

knitr::opts_chunk$set(echo = TRUE, message=FALSE,warning = FALSE,
                      fig.align = 'center',#tidy = T,tidy.opts=list(arrow=TRUE, indent=2),
                      dev = "png", #dev.args = list(type = "cairo-png"),
                       tidy='styler', tidy.opts=list(strict=TRUE))




## .custom-inline {

##   color: red;

##   font-weight: 700

## }


## ---- eval=FALSE----------------------------------------------------------------------------------------------------------------
## ## make sure devtools package is installed
## remotes::install_github('Keniajin/protGear', dependencies=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
## load the package 
library(protGear)


## -------------------------------------------------------------------------------------------------------------------------------
## specify the the parameters to process the data
genepix_vars <- array_vars(channel="635" ,
                           chip_path = system.file("extdata/array_data/", package="protGear"),
                           totsamples = 21,
                           blockspersample = 2,
                           sampleID_path = system.file("extdata/array_sampleID/", package="protGear"),
                           mig_prefix = "_first",
                           machine =1,
                           ## optional 
                           date_process = "0520")


## -------------------------------------------------------------------------------------------------------------------------------
header_gpr <- readLines(system.file("extdata/array_data/machine1/KK2-06.txt", package="protGear"),
                        n=40)
header_gpr <- gsub("\"", "", header_gpr[1:32])
header_gpr[1:32]


## ----chunk6, fig.align='left'---------------------------------------------------------------------------------------------------
visualize_slide(infile=system.file("extdata/array_data/machine1/KK2-06.txt", package="protGear"),
                MFI_var ='B635 Median' )



## ----chunk7, fig.align='left'---------------------------------------------------------------------------------------------------
visualize_slide_2d(infile =system.file("extdata/array_data/machine1/KK2-06.txt", package="protGear"), 
                   MFI_var ='F635 Median' )


## -------------------------------------------------------------------------------------------------------------------------------
#### read in all the datasets
### list all the file names under data folder
filenames <- list.files(file.path(genepix_vars$paths[[2]]), 
                        pattern="*.txt$|*.gpr$", full.names=FALSE)
#' @___________________read_in_the_files_with_the_text_data_from_the_chip_____________________________
### read all the data files and save them in a list 
data_path <- paste0(genepix_vars$paths[[2]],"/") 
data_files <- purrr::map(.x = filenames, 
                         .f = read_array_files,
                         data_path=data_path ,
                         genepix_vars=genepix_vars)
data_files <- set_names(data_files, purrr::map(filenames, name_of_files))


## -------------------------------------------------------------------------------------------------------------------------------
## utilising the map package we process a number of files  under data_files list
dfs <- names(data_files) 
allData_bg <- purrr::map(.x=dfs, .f=extract_bg,data_files=data_files,genepix_vars)
allData_bg <- set_names(allData_bg, purrr::map(filenames, name_of_files))
allData_bg <- plyr::ldply(allData_bg)


## -------------------------------------------------------------------------------------------------------------------------------
p1 <- plot_FB(allData_bg,antigen_name="antigen",
              bg_MFI="BG_Median",FG_MFI="FBG_Median",log=FALSE)


p1



## -------------------------------------------------------------------------------------------------------------------------------
p2 <- plot_bg(df=allData_bg, x_axis="Block",bg_MFI="BG_Median",
        log_mfi=TRUE) 
p2


## -------------------------------------------------------------------------------------------------------------------------------
sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files , 
                             genepix_vars, method="subtract_local")
sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))


## -------------------------------------------------------------------------------------------------------------------------------
buffer_transp <- purrr::map(.x=sample_ID_merged_dfs, .f=buffer_spots ,  buffer_spot="buffer")

buffer_transp <- set_names(buffer_transp, purrr::map(filenames, name_of_files))

buffers <- plyr::ldply(buffer_transp)
plot_buffer(buffers,buffer_names="antigen",buffer_mfi="FMedianBG_correct",slide_id=".id")



## -------------------------------------------------------------------------------------------------------------------------------
#' @________________________________calculated_cv_for_each_data_file_______________________________________
#' data without the selected mean for the best 2 CVs 
dataCV <- purrr::map(.x=sample_ID_merged_dfs, .f=cv_estimation ,lab_replicates=3  ,
                     cv_cut_off=20,
                     sampleID_var='sampleID', antigen_var='antigen' ,replicate_var='replicate' ,
                     mfi_var='FMedianBG_correct')

lab_replicates=3
dataCV <- set_names(dataCV, purrr::map(filenames, name_of_files))

aa <- plyr::ldply(dataCV)
GGally::ggpairs(aa,aes(color=cvCat_all) ,
        columns = paste(1:lab_replicates), title = "",  axisLabels = "show") +
  theme_light()
   


## -------------------------------------------------------------------------------------------------------------------------------


#' @________________________________summary_of_cv_for_each_sample________________________________________ 
#' creates summaries by cv's greater than 20 and less than 20

cv_cut_off <- 20
dataCV_sample <- purrr::map(.x=dataCV, .f=protGear::cv_by_sample_estimation , cv_variable="cvCat_all",
                            lab_replicates=3)
dataCV_sample <- set_names(dataCV_sample, purrr::map(filenames, name_of_files))
all_cv_sample <- plyr::ldply(dataCV_sample)



## -------------------------------------------------------------------------------------------------------------------------------
less_20 <- rlang::sym(paste0("CV <= ",cv_cut_off, "_perc"))
gt_20 <- rlang::sym(paste0("CV > ",cv_cut_off, "_perc"))

less_20_per <-  rlang::sym(paste0("% CV <=",cv_cut_off))
gt_20_per <-  rlang::sym(paste0("% CV >",cv_cut_off))
ggplot(all_cv_sample)+
  geom_violin(aes(.id,`CV <= 20_perc`, color="% CV =<20")) +
  geom_violin(aes(.id,`CV > 20_perc`, color="% CV >20")) +
  geom_violin(aes(.id,Others_perc,color="Others")) +
  ylab("% of CV") +
  theme_minimal() +
  ggtitle("% of CV >20 or <=20 for each slide all repeats considered") 


## -------------------------------------------------------------------------------------------------------------------------------
#' @________________________________data_with_selected_best_2_CV_______________________________________ 
#' data with the selected mean for the best 2 CVs
dataCV_best2 <- purrr::map(.x=dataCV, .f=best_CV_estimation , slide_id="iden",lab_replicates=3,
                           cv_cut_off=20)

## give the names to the returned list
dataCV_best2 <- set_names(dataCV_best2, purrr::map(filenames, name_of_files))


dataCV_sample_best2 <- purrr::map(.x=dataCV_best2, .f=cv_by_sample_estimation , 
                                  cv_variable="best_CV_cat",lab_replicates=3)
dataCV_sample_best2 <- set_names(dataCV_sample_best2, purrr::map(filenames, name_of_files))
all_cv_sample_best2 <- plyr::ldply(dataCV_sample_best2)


## -------------------------------------------------------------------------------------------------------------------------------
## plot only the CV perccentages
ggplot(all_cv_sample_best2)+
  geom_violin(aes(.id,`CV <= 20_perc`, color="% CV =<20")) +
  geom_violin(aes(.id,`CV > 20_perc`, color="% CV >20")) +
  geom_violin(aes(.id,Others_perc,color="Others")) +
  ylab("% of CV") +
  theme_minimal() +
  ggtitle("% of CV >20 or <=20 for each slide") 


## -------------------------------------------------------------------------------------------------------------------------------
tag_file <- read.csv("TAG_antigens.csv")
tag_antigens <- c("CD4TAG" , "GST", "MBP")
batch_vars <- list(machine="m1", day="0520")


## -------------------------------------------------------------------------------------------------------------------------------
tb1 <- data.frame(head(tag_file, n=10))
tb1 %>% 
  kable() %>%
  kable_styling()


## -------------------------------------------------------------------------------------------------------------------------------
#' @________________________________subtract_the_tag_values_______________________________________ 
#'
## tag subtract 
## read in the KILCHip TAG file to substract GST-1, MBP -2 and  CD4TAG - 0 file 
dataCV_tag <- purrr::map(.x=dataCV_best2, .f=tag_subtract , 
                         tag_antigens=tag_antigens, mean_best_CV_var="mean_best_CV",tag_file=tag_file,
                         antigen_var='antigen',
                         batch_vars=batch_vars)
dataCV_tag <- set_names(dataCV_tag, purrr::map(filenames, name_of_files))
dataCV_tag <- plyr::ldply(dataCV_tag)


## -------------------------------------------------------------------------------------------------------------------------------
aaa <- dataCV_tag %>% 
  filter(TAG_name=="GST") 

aaa <- aaa %>% 
dplyr::select(.id,sampleID,antigen,mean_best_CV,mean_best_CV_tag)

aaa <- aaa %>% 
  gather(measure,mfi,-c(.id:antigen))

ggplot(aaa,aes(as.factor(antigen),mfi,color=measure))  +
  geom_boxplot(aes(fill=measure),alpha=0.5)+
  theme_light() +
  xlab("antigen name")+
  ggtitle("Before and after TAG subtraction") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## -------------------------------------------------------------------------------------------------------------------------------
df_to_normalise <-  dataCV_tag  %>%  ungroup() %>%  
  dplyr::select(slide=.id,sampleID,sample_array_ID,antigen,mean_best_CV) %>%  
  group_by(sampleID, slide)
df_to_normalise$sample_index <- group_indices(.data =df_to_normalise )

### 
to_normalise <- df_to_normalise %>%
 ungroup() %>% dplyr::select(-slide,-sampleID,-sample_array_ID) %>% 
  dplyr::select(antigen, sample_index, everything()) %>%  
  gather(variable, value, -(antigen:sample_index)) %>%
  unite(temp, antigen ) %>%  dplyr::select(-variable) %>%
  spread(temp, value) %>% 
  as.data.frame(.)

### get the row names of the machine data
row.names(to_normalise) <- to_normalise$sample_index
#batch_all <- as.factor(paste0(to_normalise$machine,"/",to_normalise$day))
#machines <- as.factor(to_normalise$machine)
#day_batches <- as.factor(to_normalise$day)

## create the matrix to normalise
matrix_antigen <-  to_normalise %>% 
   dplyr::select(-sample_index) %>% 
  as.matrix(.)


## create the matrix to hold the important parameters 
## in place of AMA1 you use one of your features or antigen
array_matrix <- df_to_normalise %>% 
  filter(antigen=="AMA1") %>% 
  ungroup() %>% 
   dplyr::select(sample_array_ID,sample_index,slide)

control_antigens <- c("CommercialHumanIgG","CD4TAG")



## -------------------------------------------------------------------------------------------------------------------------------
normlise_df <- matrix_normalise(matrix_antigen, method = "vsn", array_matrix=array_matrix,
                       return_plot = TRUE,control_antigens=control_antigens)

normlise_df$plot_normalisation



## -------------------------------------------------------------------------------------------------------------------------------
control_antigens <- c("CommercialHumanIgG","CD4TAG")
## no normalisation
normalise_list_none <- matrix_normalise(matrix_antigen=matrix_antigen, 
                                         method = "none", 
                                         array_matrix=array_matrix,
                                         return_plot = TRUE,
                                         control_antigens=control_antigens)
  names(normalise_list_none) <- c("matrix_antigen_none" ,"plot_none")
## log2 normalisation
  normalise_list_log <- matrix_normalise(matrix_antigen=matrix_antigen, 
                                           method = "log2", 
                                           array_matrix=array_matrix,
                                           return_plot = TRUE,
                                           control_antigens=control_antigens)
  names(normalise_list_log) <- c("matrix_antigen_log" ,"plot_log")
## vsn normalisation
   normalise_list_vsn <- matrix_normalise(matrix_antigen=matrix_antigen, 
                                           method = "vsn", 
                                           array_matrix=array_matrix,
                                           return_plot = TRUE,
                                           control_antigens=control_antigens)
    names(normalise_list_vsn) <- c("matrix_antigen_vsn" ,"plot_vsn")
  ## cyclic loess with log
     normalise_list_cyclic_loess_log <- matrix_normalise(matrix_antigen=matrix_antigen, 
                                                        method = "cyclic_loess_log", 
                                                        array_matrix=array_matrix,
                                                        return_plot = TRUE,
                                                        control_antigens=control_antigens)
    names(normalise_list_cyclic_loess_log) <- c("matrix_antigen_cyclic_loess_log" ,
                                                "plot_cyclic_loess_log")
    

     normalise_list_rlm <- matrix_normalise(matrix_antigen=matrix_antigen, 
                                                  method = "rlm", 
                                                  array_matrix=array_matrix,
                                                  return_plot = TRUE,
                                                  control_antigens=control_antigens)
    names(normalise_list_rlm) <- c("matrix_antigen_rlm" ,"plot_rlm")
    
  
    ## create a list after normalisation
 normalised_list <- c(normalise_list_none , 
                      normalise_list_log,
                      normalise_list_vsn,
                      normalise_list_cyclic_loess_log,
                      normalise_list_rlm)
  ##
 normalised_list_plot <- normalised_list[grepl("plot",names(normalised_list))]

  


## ---- fig.align='center', fig.width=12, fig.height=15---------------------------------------------------------------------------
p <- do.call("grid.arrange", c(normalised_list_plot, ncol=2))


## -------------------------------------------------------------------------------------------------------------------------------
norm_df <- normlise_df$matrix_antigen_normalised
norm_df <- norm_df %>% 
   dplyr::select(-control_antigens)
p3 <- pheatmap::pheatmap(norm_df ,scale = "none", cluster_rows = FALSE,
                            main=paste('VSN',"Normalised Data"),
                            silent = TRUE)
#-------
## if you want to save the file
# p3 <- ggplotify::as.ggplot(p3)
# p <- p3 +  theme_void()
# ggsave(p ,
#          filename ="heatmap.PNG" ,
#          width = 16 , height = 12 , 
#          limitsize = FALSE,
#          dpi=200 )
#-------
p3


## ----image_heat, echo=FALSE, fig.cap="PCA analysis", out.width = '100%'---------------------------------------------------------

#files <- list.files(pattern = 'heatmap')

#knitr::include_graphics(files[[1]])
#knitr::include_graphics('heatmap.PNG')


## ----pca, , fig.align='center',fig.width=16,fig.height=12-----------------------------------------------------------------------
norm_df <- normlise_df$matrix_antigen_normalised
res_pca <- prcomp( norm_df, scale = TRUE)
var <- get_pca_var(res_pca)
vars_visualise=20
#Visualize the PCA
## individuals contributing to the PCA
p1 <- fviz_pca_ind(res_pca,
                    col.var = "contrib", # Color by contributions to the PC
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                    repel = TRUE     # Avoid text overlapping
  )+    theme_minimal()
  
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

 
## combine the plots into one grid
## combine the plots into one grid
## combine the plots into one grid
p_pca <- gridExtra::grid.arrange(p1,p2,p3,p4, ncol=2 )
## If you want to save the file
# ggsave(p_pca ,
#          filename ="p_pca.PNG" ,
#          width = 16 , height = 12 , 
#           units = "in",
#          limitsize = FALSE,
#          dpi=300)
p_pca 


## ---- eval=FALSE----------------------------------------------------------------------------------------------------------------
## protGear::launch_protGear_interactive()


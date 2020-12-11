#' @________________________________calculated_cv_for_each_data_file_______________________________________
#' data without the selected mean for the best 2 CVs 
#cv <- calculate_cv(data2)

dataCV <- purrr::map(.x=sample_ID_merged_dfs, .f=cv_estimation ,lab_replicates=1)

#set_names(dfs, purrr::map(filenames, name_of_files))
dataCV <- set_names(dataCV, purrr::map(filenames, name_of_files))


#' @________________________________summary_of_cv_for_each_sample________________________________________ 
#' creates sumamries by cv's greater than 20 and less than 20

#plot_cv_frequency(dataCV, cv_variable="cvCat_all")
lab_replicates =1
dataCV_sample <- purrr::map(.x=dataCV, .f=cv_by_sample_estimation , cv_variable="cvCat_all",lab_replicates)
#set_names(dfs, purrr::map(filenames, name_of_files))
dataCV_sample <- set_names(dataCV_sample, purrr::map(filenames, name_of_files))
all_cv_sample <- plyr::ldply(dataCV_sample)

## plot only the CV perccentages
ggplot(all_cv_sample)+
  geom_violin(aes(.id,`CV <= 20_perc`, color="% CV =<20")) +
  geom_violin(aes(.id,`CV > 20_perc`, color="% CV >20")) +
  geom_violin(aes(.id,Others_perc,color="Others")) +
  ylab("% of CV") +
  theme_minimal() +
  ggtitle("% of CV >20 or <=20 for each sample all repeats considered") 



#' @________________________________data_with_selected_best_2_CV_______________________________________ 
#' data with the selected mean for the best 2 CVs

dataCV_best2 <- purrr::map(.x=dataCV, .f=best_CV_estimation , slide_id="iden",lab_replicates=1)

## give the names to the returned list
dataCV_best2 <- set_names(dataCV_best2, purrr::map(filenames, name_of_files))


dataCV_sample_best2 <- purrr::map(.x=dataCV_best2, .f=cv_by_sample_estimation , cv_variable="best_CV_cat",lab_replicates=1)
dataCV_sample_best2 <- set_names(dataCV_sample_best2, purrr::map(filenames, name_of_files))
all_cv_sample_best2 <- plyr::ldply(dataCV_sample_best2)

## plot only the CV perccentages
ggplot(all_cv_sample_best2)+
  geom_violin(aes(.id,`CV <= 20_perc`, color="% CV =<20")) +
  geom_violin(aes(.id,`CV > 20_perc`, color="% CV >20")) +
  geom_violin(aes(.id,Others_perc,color="Others")) +
  ylab("% of CV") +
  theme_minimal() +
  ggtitle("% of CV >20 or <=20 for each sample") 



#' @________________________________subtract_the_tag_values_______________________________________ 
#'
## tag subtract 
## read in Faith file to substract GST-1, MBP -2 and  CD4TAG - 0 file 
## GK and FO explains the usage of TAG 
tag_file <- read.csv("TAG_antigens.csv")
tag_antigens <- c("CD4TAG" , "GST", "MBP")
batch_vars <- list(machine="m1", day="0520")
dataCV_tag <- purrr::map(.x=dataCV_best2, .f=tag_subtract , 
                         tag_antigens=tag_antigens, mean_best_CV_var="mean_best_CV",tag_file=tag_file,
                         batch_vars)

dataCV_tag <- set_names(dataCV_tag, purrr::map(filenames, name_of_files))
dataCV_tag <- plyr::ldply(dataCV_tag)


summary_all_data <- dataCV_tag 
summary_all_data$see <- gsub("CV","" , summary_all_data$x)
summary_all_data_plot <- summary_all_data %>% 
  dplyr::select(.id,sampleID,antigen,see,"1","2","3") %>% 
  gather(meanVar, mfiVal, "1":"3")

summary_all_data_plot <- summary_all_data_plot %>% 
  filter(see!=meanVar) %>%  
  group_by(.id,sampleID,antigen) %>% 
  mutate(meanVar2=1:n()) %>% 
  dplyr::select(-see, -meanVar) %>%
  spread(meanVar2,mfiVal) %>% 
  rename(s1=`1`,s2=`2`)

ggplot(summary_all_data_plot)+
  geom_jitter(aes(x=s1,y=s2, color=.id)) + 
  theme_classic() + ylab("Dup 1 MFI")+ xlab("Dup 2 MFI") +
  ggtitle("Plot of duplicates selected for controls M2")


##control list
cntrls_prefix <- '^UK|^SWED|^EU|^NAI'
all_prefix <- '^UK|^SWED|^EU|^NAI|BLANK|PHIS'

#'@_______________________
#'Create normalisation datasets
batch_vars_name <- c("machine","day")
df_to_normalise <-  dataCV_tag  %>%  ungroup() %>%  
  select(slide=.id,sampleID,sample_array_ID,antigen,batch_vars_name,mean_best_CV) %>%  
  group_by(sampleID,machine, slide)
df_to_normalise$sample_index <- group_indices(.data =df_to_normalise )

### 
to_normalise <- df_to_normalise %>%
 ungroup() %>% select(-slide,-sampleID,-sample_array_ID) %>% 
  select(antigen, machine,day,sample_index, everything()) %>%  
  gather(variable, value, -(antigen:sample_index)) %>%
  unite(temp, antigen ) %>% select(-variable) %>%
  spread(temp, value) %>% 
  as.data.frame(.)


### get the row names of the machine data
row.names(to_normalise) <- to_normalise$sample_index
batch_all <- as.factor(paste0(to_normalise$machine,"/",to_normalise$day))
machines <- as.factor(to_normalise$machine)
day_batches <- as.factor(to_normalise$day)

## remove the sampleID and the  machine and data variable from the dataset
## reoved the id - 
matrix_antigen <-  to_normalise %>% 
  select(-sample_index,-machine,-day) %>% 
  as.matrix(to_normalise)

array_matrix <- df_to_normalise %>% 
  filter(antigen=="AMA1") %>% 
  ungroup() %>% 
  select(sample_array_ID,sample_index,slide)


control_antigens <- c("CommercialHumanIgG","CD4TAG")
  
##  Batch correction 


##




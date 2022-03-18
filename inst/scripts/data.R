#' Example data for protGear package.
#' dataC.csv
#' ------------------------------------
#' library(protGear)
## define the structure
# genepix_vars <- array_vars(channel="635" ,
#                            chip_path = system.file("extdata/array_data/", package="protGear"),
#                            totsamples = 21,
#                            blockspersample = 2,
#                            sampleID_path = system.file("extdata/array_sampleID/", package="protGear"),
#                            mig_prefix = "_first",
#                            machine =1,
#                            ## optional
#                            date_process = "0520")
#### read in all the datasets
### list all the file names under data folder
# filenames <- list.files(file.path(genepix_vars$paths[[1]]),
#                         pattern="*.txt$|*.gpr$", full.names=FALSE)
### read all the data files and save them in a list
# data_path <- paste0(genepix_vars$paths[[1]],"/")
# data_files <- purrr::map(.x = filenames,
#                          .f = read_array_files,
#                          data_path=data_path ,
#                          genepix_vars=genepix_vars)
# data_files <- set_names(data_files, purrr::map(filenames, name_of_files))
#
# ## this does the background correction after reading the
# ## genepix data file.
# sample_ID_merged_dfs <- purrr::map(.x=dfs, .f=merge_sampleID ,data_files=data_files ,
#                                    genepix_vars, method="subtract_local")
# sample_ID_merged_dfs <- set_names(sample_ID_merged_dfs, purrr::map(filenames, name_of_files))
# dataC <- sample_ID_merged_dfs[[1]]
#' bg_example.csv
#' ----------------------------------------
#' This is a sample of the allData_bg dataset
#' ---------------------------------
#' dfs <- names(data_files)
# allData_bg <- purrr::map(.x=dfs, .f=extract_bg,data_files=data_files,genepix_vars)
# allData_bg <- set_names(allData_bg, purrr::map(filenames, name_of_files))
# allData_bg <- plyr::ldply(allData_bg)
#
#'
#' Data1_sample.csv --
#' -----------------------------------
#' A sample of the dataC.csv file
#'
#'
#' Data1_bg_sample.csv
#'-----------------------------------
#' The data is generated from
#' Data1_bg_sample <- data_files[[1]]
#'
#' matrix_antigen.csv
#' -------------------------------
#' After creating the sample_ID_merged_dfs list
#' As indicated in the vignette
#' 1) Perform Coefficient of Variation (CV)
#' 2) Select the Best replicates
#' 3) If the proteins are tagged perform Tag subtraction
#' 4) Create the to_normalise data frame and then
###
# to_normalise <- df_to_normalise %>%
#   ungroup() %>% dplyr::select(-slide,-sampleID,-sample_array_ID) %>%
#   dplyr::select(antigen, sample_index, everything()) %>%
#   gather(variable, value, -(antigen:sample_index)) %>%
#   unite(temp, antigen ) %>%  dplyr::select(-variable) %>%
#   spread(temp, value) %>%
#   as.data.frame(.)
#
# ### get the row names of the machine data
# row.names(to_normalise) <- to_normalise$sample_index
# #batch_all <- as.factor(paste0(to_normalise$machine,"/",to_normalise$day))
# #machines <- as.factor(to_normalise$machine)
# #day_batches <- as.factor(to_normalise$day)
#
# ## create the matrix to normalise
# matrix_antigen <-  to_normalise %>%
#   dplyr::select(-sample_index) %>%
#   as.matrix(.)

#'
#' array_matrix.csv
#' --------------------------------
#'## create the matrix to hold the important parameters
## in place of AMA1 you use one of your features or antigen
# array_matrix <- df_to_normalise %>%
#   filter(antigen=="AMA1") %>%
#   ungroup() %>%
#   dplyr::select(sample_array_ID,sample_index,slide)
#'
#'
#' buffers_sample2.csv
#' ---------------------------------
#' This is a sample of buffers data frame below
# buffer_transp <- purrr::map(.x=sample_ID_merged_dfs,
# .f=buffer_spots ,  buffer_spot="buffer")
#
# buffer_transp <- set_names(buffer_transp, purrr::map(filenames, name_of_files))
#
# buffers <- plyr::ldply(buffer_transp)
#'
#'
#' TAG_antigens.csv
#' ----------------------
#' This is a file with all the proteins processed and their respective TAG's.
#' Tags are explained in detail under https://www.frontiersin.org/articles/10.3389/fimmu.2018.02866/full
#' Microarray Protein Map
#'
#' array_data/machine1
#' -----------------------------------
##'  KK2-06.txt and BRB001.txt
##'  This is data extracted using GenePix® Pro software (Molecular Devices).
##' This is data sets from from KILchip v1.0 (https://doi.org/10.3389/fimmu.2018.02866),
##' a protein microarray chip designed to enable the simultaneous detection of antibodies
##'  against > 100 Plasmodium falciparum proteins. The slides were printed in
##'  triplicate on a slide divided into mini-arrays, defined as the region allocated to
##'  a discrete sample and can be further divided into blocks.
##'
#'
#' @format An object of class 'ElistRaw' from limma package that
#' contains 5 entries: matrix E, data.frames targets, genes, source and printer.
#' E is a matrix of protein intensities of size 23232 by 28 (number of samples);
#' targets is a data.frame containing original sample file names;
#' genes is a data.frame with information about each protein on the Protoarray
#' source stores a name of method that has produced this dataset
#' and printer contains information about printer
#'
#"rawdata"

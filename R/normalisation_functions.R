#' Normalize  Arrays
#'
#' @param matrix_antigen An object of class matrix with  features/proteins as columns and samples as the rows
#' @param method character string specifying the normalization method. Choices are \code{"none","log2","vsn","cyclic_loess"}
#' \code{"cyclic_loess_log" ,"rlm"}
#' @param batch_correct A logical value indicating whether batch correction should be done or not
#' @param batch_var1 A character or factor vector of size similar to rows of \code{matrix_antigen} indicating the first batch.
#' @param batch_var2 A character or factor vector of size similar to rows of \code{matrix_antigen} indicating the second batch.
#' @param return_plot A logical value indicating whether a plot is returned to show the results of normalisation.
#' @param control_antigens  logical vector specifying the subset of spots which are non-differentially-expressed control spots,
#' for use with \code{method="rlm"}
#' @param array_matrix  An object of class dataframe or matrix used with \code{method='rlm'} indicating the sample index and
#' @param plot_by_antigen Logical to indicate whether to plot by antigen or not
#' slide name for the matrix_antigen object.
#' @import limma tibble
#' @return A data frame of normalised values
#' @export
#'
#' @examples
#' matrix_antigen <- readr::read_csv(system.file("extdata", "matrix_antigen.csv", package="protGear"))
#' #VSN
#' normlise_vsn <- matrix_normalise(as.matrix(matrix_antigen),
#' method = "vsn",
#' return_plot = TRUE
#' )
#' ## log
#' normlise_log <- matrix_normalise(as.matrix(matrix_antigen),
#' method = "log2",
#' return_plot = TRUE
#' )
#' ## cyclic_loess_log
#' normlise_cylic_log <- matrix_normalise(as.matrix(matrix_antigen),
#' method = "cyclic_loess_log",
#' return_plot = TRUE
#' )
matrix_normalise <- function(matrix_antigen, method="log2",batch_correct=FALSE,batch_var1,batch_var2=day_batches,
                             return_plot=FALSE,plot_by_antigen=TRUE,control_antigens=NULL, array_matrix=NULL){
  if(method=="log2"){
    exprs_log <- matrix_antigen
    ## since log can handle negative values , we convert all the negative values to a constant value
    # exprs_log[exprs_log<0] <- 1

    ## any value that is negative is allocated the smallest value in the batch
    min_pos <- minpositive(exprs_log)
    exprs_log <- replace(exprs_log, exprs_log < 1,min_pos)
    exprs_normalised <- log2(exprs_log)


  }else if(method=="none"){
    exprs_normalised <- matrix_antigen

  }else if(method=="vsn"){
    ## this approach utilises the VSN package
    ## structure the data to be normalised
    ## convert it to an Expression Set
    exprs_antigen <-
      Biobase::ExpressionSet(assayData = matrix_antigen)

    if(batch_correct==FALSE){
      ## normalise without adjusting for the strata on VSN
      data_points <- dim(matrix_antigen)[[1]]
      exprs_normalised <-  vsn::justvsn(x=exprs_antigen ,
                                        minDataPointsPerStratum=data_points)
    }else if(batch_correct==TRUE){
      exprs_normalised <-
        vsn::justvsn(x = exprs_antigen , strata = machines ,
                     minDataPointsPerStratum=data_points)
    }


  }else if(method=="cyclic_loess"){
    exprs_normalised <-  limma::normalizeCyclicLoess(matrix_antigen,
                                                     weights = NULL,
                                                     span=0.7,
                                                     iterations = 3,
                                                     method = "fast")
  }else if(method=="cyclic_loess_log"){
    exprs_normalised <-   limma::normalizeCyclicLoess(matrix_antigen,
                                                      weights = NULL,
                                                      span=0.7,
                                                      iterations = 3,
                                                      method = "fast")
    ## any value that is negative is allocated the smallest value in the batch
    min_pos <- minpositive(exprs_normalised)
    exprs_normalised <- replace(exprs_normalised, exprs_normalised < 1,min_pos)
    exprs_normalised <- log2(exprs_normalised)
  }else if(method=="rlm"){
    if(!is.null(control_antigens)){
      exprs_df <- rlm_normalise_matrix(matrix_antigen=matrix_antigen, array_matrix=array_matrix,
                                       control_antigens=control_antigens)
      exprs_normalised <-   rlm_normalise(exprs_df)
      exprs_normalised <-  exprs_normalised  %>%
        gather(variable, value, -(antigen:sampleID2)) %>%
        unite(temp, antigen) %>% select(-variable) %>%
        spread(temp, value) %>%
        column_to_rownames(var = "sampleID2")
    }else if(is.null(control_antigens) | is.null(array_matrix)){
      stop("Specify the control antigens or array_matrix to use RLM")
    }
  }

  cv_val <- round(sd(as.matrix(exprs_normalised),na.rm = TRUE)/mean(as.matrix(exprs_normalised), na.rm = TRUE),4)*100
  cv_val <- paste(cv_val,"%",sep = "")
  #exprs_normalised_df <- as.data.frame(exprs_normalised)

  ## return the data in the structure as it was supplied
  if(method=="vsn"){
    ## this kept if a different structure is considered
    matrix_antigen_normalised <- t(as.data.frame(exprs_normalised))
    row.names(matrix_antigen_normalised) <- as.numeric(gsub("X","",row.names(matrix_antigen_normalised)))
    matrix_antigen_normalised <-  as.data.frame(matrix_antigen_normalised)
  }else{
    matrix_antigen_normalised <-  as.data.frame(exprs_normalised)
    #matrix_antigen_normalised <- exprs_normalised_df
  }

  ## Whether to plot by sample or antigen
  if(plot_by_antigen==TRUE){
    plot_normalisation <- plot_normalised_antigen(exprs_normalised_df=matrix_antigen_normalised,
                                                  method=method,batch_correct=batch_correct)
  }else{
    plot_normalisation <- plot_normalised(exprs_normalised_df=matrix_antigen_normalised,
                                          method=method,batch_correct=batch_correct)
  }



  if(return_plot==TRUE){
    return(list(matrix_antigen_normalised=matrix_antigen_normalised,
                plot_normalisation=plot_normalisation))
  }else {
    return(matrix_antigen_normalised)
  }

}




#' Remove batch effect under development
#'
#' @return a data frame of batch corrected data by limma
#' @export
#' @import limma
#' @examples
#'
remove_batch_effect <- function(){
  ## running the limma batch removal approach after VSN (mathematical effect???)
  ## yet to include limma normalisation -->]
  # which can be included here from the antigen matrix object
  if(length(unique(bts$b_2)) ==1){
    print(paste0("Limmma batch this site ",site, " has one day batch"))
    exprs_vsn_limma <- limma::removeBatchEffect(exprs_antigen_vsn,
                                                batch=bts$b_1)
  }else {
    exprs_vsn_limma <- limma::removeBatchEffect(exprs_antigen_vsn,
                                                batch=bts$b_1, batch2=bts$b_2)
  }
}





#' Nomrmalise using RLM
#' @param matrix_antigen A matrix with antigen data
#' @param array_matrix A matrix with control antigen data
#' @param control_antigens the control antigens for RLM normalisation
#' @description  A function for \code{method='rlm'} from \code{\link{matrix_normalise}}.
#' @return A RLM normalised data frame
#' @import dplyr
#' @export
#'
#' @examples
#'
rlm_normalise_matrix <- function(matrix_antigen, array_matrix,control_antigens){
  array_matrix$sample_index <- as.character(array_matrix$sample_index )

  array_matrix <- array_matrix %>%
    group_by(slide) %>%
    mutate(Array=group_indices())

    rlm_normalise_df <- as.data.frame.matrix(matrix_antigen) %>%
    #dplyr::mutate(sample_index=row.names(matrix_antigen))
    rownames_to_column(var = "sample_index")


  rlm_normalise_df <- rlm_normalise_df %>%
    select(sample_index, everything()) %>%
    gather(antigen, MFI_val,-sample_index) %>%
    left_join(array_matrix, by="sample_index") %>%
    rename(Block=sample_array_ID) %>%
    mutate(Block=as.numeric(Block))


  ## create a variable to indicate the control antigens
  rlm_normalise_df <-   rlm_normalise_df %>%
    #mutate(Description=ifelse(grepl(paste0(control_antigens),antigen),"Control","Sample"))
    mutate(Description=ifelse(antigen %in% all_of(control_antigens) ,"Control","Sample"))
  return(rlm_normalise_df)
}


#' RLM normalisation
#'
#' @param rlm_normalise_df rlm normalised data frame
#' @description  A function for \code{method='rlm'} from \code{\link{matrix_normalise}}.
#' @return an elist of RLM normalisation to be utilised by \code{\link{rlm_normalise_matrix}}
#' @export
#' @examples
#'
rlm_normalise <- function(rlm_normalise_df){
  rlm_normalise_C <- rlm_normalise_df %>%
    filter(grepl("Control", Description))
  rlm_normalise_E <- rlm_normalise_df %>%
    filter( grepl("Sample", Description) )


  ## create the matrix of the control antigens
  rlm_normalise_C <- rlm_normalise_C  %>%
    select(Array, Block,antigen, MFI_val) %>%
    spread(Array,MFI_val )

  ## create the matrix of the sample antigens
  rlm_normalise_E <- rlm_normalise_E  %>%
    select(Array, Block,antigen, MFI_val) %>%
    spread(Array,MFI_val )


  ## create Elist for the controls and samples
  sample_elist <- list(E = rlm_normalise_E %>%
                         select(-c(Block,antigen)) ,
                       genes =  rlm_normalise_E %>% select(Block:antigen))


  controls_elist <- list(E = rlm_normalise_C %>%
                           select(-c(Block,antigen)) ,
                         genes =  rlm_normalise_C %>%
                           select(Block:antigen))


  ## changing less values to the lowest positivie MFI value
  min_pos <- minpositive(controls_elist$E)
  controls_elist$E <- replace(controls_elist$E, controls_elist$E < 1,min_pos/2)

  ## change the control EList to log2 -->
  ## RLM reccommends
  controls_elist$E  <- as.matrix(controls_elist$E)
  controls_elist$E <- log2(controls_elist$E)
  rownames(controls_elist$E) <- controls_elist$genes$antigen

  contr_names <- unique(rownames(controls_elist$E))
  contr_names_len <- length(contr_names)
  contr_mapping <- matrix(nrow = contr_names_len, ncol = 1)
  rownames(contr_mapping) <- contr_names
  contr_mapping[, 1] <- 1:contr_names_len


  ##the number of not control antigens
  p_features <- nrow(sample_elist$E)
  p_controls <- nrow(controls_elist$E)
  n_arrays <- ncol(sample_elist$E)
  n_blocks <- max(controls_elist$genes$Block , na.rm=TRUE)
  y <- c(controls_elist$E)


  ## should be contr_names_len - 3 ... check
  ## should it be changed when the controls are more
  dummies <- matrix(0, ncol = {
    n_arrays + n_blocks + contr_names_len - 3
  }, nrow = length(y))


  rnames <- vector(length = length(y))
  a_cols <- paste("a", 1:{
    n_arrays - 1
  }, sep = "")


  b_cols <- paste("b", 1:{
    n_blocks - 1
  }, sep = "")



  ####changed
  t_cols <- paste("t", 1:{
    contr_names_len - 1
  }, sep = "")



  colnames(dummies) <- c(a_cols, b_cols, t_cols)

  ## matrix to hold the parameters per array for each of the control antigen
  a_params <- matrix(nrow = n_arrays, ncol = 2)
  rownames(a_params) <- colnames(controls_elist$E)

  ## matrix to hold the parameters for the Blocks
  b_params <- matrix(nrow = n_blocks, ncol = 2)
  idx <- 1

  ## putting values to the defined matrix
  # fill the matrix with 1's in such manner that for each row
  # the combination of 1's and 0's is unique
  for (i in 1:n_arrays) {
    a_tmp <- paste("a", i, sep = "")
    a_params[i, 1] <- a_tmp
    for (j in 1:p_controls) {
      rnames[idx] <- rownames(controls_elist$E)[j]
      b_idx <- controls_elist$genes$Block[j]
      b_tmp <- paste("b", b_idx, sep = "")
      b_params[b_idx, 1] <- b_tmp
      t_idx <- contr_mapping[rownames(controls_elist$E)[j],
                             1]
      t_tmp <- paste("t", t_idx, sep = "")
      if (i == n_arrays) {
        dummies[idx, a_cols] <- -1
      }
      else {
        dummies[idx, a_tmp] <- 1
      }
      if (b_idx == n_blocks) {
        dummies[idx, b_cols] <- -1
      }
      else {
        dummies[idx, b_tmp] <- 1
      }
      if (t_idx == contr_names_len) {
        dummies[idx, t_cols] <- -1
      }
      else {
        dummies[idx, t_tmp] <- 1
      }
      idx <- idx + 1
    }
  }

  ## bind with the control spots
  dummies <- cbind(y, dummies)
  rownames(dummies) <- rnames

  ## add the error term
  e <- rnorm(length(y), 0, 1)

  ## run the model of controls and a matrix created out of controls
  rlm_result <- MASS::rlm(y ~ . + e, data = data.frame(dummies))

  ### PAA approach
  a_params[-n_arrays, 2] <- rlm_result$coefficients[a_params[-n_arrays, 1]]
  a_params_sum <- sum(as.numeric(a_params[-n_arrays, 2]))
  a_params[n_arrays, 2] <- -a_params_sum


  b_params[-n_blocks, 2] <- rlm_result$coefficients[b_params[-n_blocks,1]]
  b_params_sum <- sum(as.numeric(b_params[-n_blocks, 2]))
  b_params[n_blocks, 2] <- -b_params_sum


  a <- t(matrix(rep(as.numeric(a_params[, 2]), p_features),
                ncol = p_features))

  b <- matrix(mapply(function(i) {
    as.numeric(b_params[sample_elist$genes$Block[i], 2])
  }, rep(1:p_features, n_arrays)), ncol = n_arrays)



  ### added by ken - to convert all the negatives in the sample antigens to make them to the half of
  ## the lowest positive

  min_pos <- minpositive(sample_elist$E)
  sample_elist$E<- replace(sample_elist$E, sample_elist$E < 1,min_pos/2)

  ###
  ## sample antigens matrix log 2
  sample_elist_log <- as.matrix(log2(sample_elist$E))

  sample_elist_normalised <- sample_elist_log - a -b
  row.names(sample_elist_normalised) <- paste0(sample_elist$genes$Block, "_",sample_elist$genes$antigen)


  sample_elist_normalised <- as.data.frame(sample_elist_normalised) %>%
    rownames_to_column("antigen_name") %>%
    mutate(Block=as.integer(sub('_.*$','', antigen_name)) ,
           antigen=sub("[^_]*(.*)", "\\1", antigen_name),
           antigen=sub('.', '', antigen)) %>%
    select(Block,antigen_name, antigen, everything())

  elist_normalised_df <- sample_elist_normalised %>%
    gather(Array,meanBest2_RLM,-c("Block","antigen_name","antigen")) %>%
    mutate(Array=as.numeric(Array)) %>%
    right_join(rlm_normalise_df, by=c("antigen","Block","Array"))


  elist_normalised_df <- elist_normalised_df %>% group_by(sample_index, slide)

  ## add the sample ID variable
  elist_normalised_df$sampleID2 <- group_indices(.data =elist_normalised_df )

  ##
  elist_normalised_df <- elist_normalised_df %>%
    ungroup() %>%
    select(antigen,sampleID2, meanBest2_RLM)

  return(elist_normalised_df)

}


#' Trend test using Cox–Stuart (C–S) and Mann–Kendall (M–K) trend tests
#'
#' @param name Name of the test
#' @param p_val p value from the test
#' @param z_val the Z value of the test
#'
#' @return A statistics of mean standard deviation trend
#' @export
#'
#' @examples
#' output_trend_stats(name="t.test",p_val=0.001, z_val=5)
output_trend_stats <- function(name, p_val, z_val){
  if(p_val<0.00001){
    prop <- "<0.00001"
    z <- round(z_val,4)
  }else{
    prop <- as.character(round(p_val,4))
    z <- round(z_val,4)
  }
  output <- paste0(name, ' Z-val = ',z,' P-val = ',prop )
}



#' Comparison of normalised data by sample
#'
#' @param exprs_normalised_df a normalised data frame
#' @param method the method of normalisation used
#' @param batch_correct the batch correction
#'
#' @import dplyr   ggplot2
#' @importFrom  readr  read_csv
#' @importFrom Kendall MannKendall
#' @importFrom genefilter rowSds
#' @importFrom plyr .
#' @return A ggplot of normalised data
#' @export
#'
#' @examples
#' matrix_antigen <- readr::read_csv(system.file("extdata", "matrix_antigen.csv", package="protGear"))
#' normlise_vsn <- matrix_normalise(as.matrix(matrix_antigen),
#' method = "vsn",
#' return_plot = FALSE
#' )
#' plot_normalised(normlise_vsn,method="vsn",batch_correct=FALSE)
plot_normalised <- function(exprs_normalised_df,method,batch_correct){
  exprs_normalised_df_plot <-  exprs_normalised_df %>%
    dplyr::mutate(mean_all_anti = rowMeans(., na.rm = TRUE),
                  stdev_all_anti = genefilter::rowSds(as.matrix(.), na.rm = TRUE)) %>%
    dplyr::mutate(rank_mean_all_anti=rank(mean_all_anti) ,
                  method=method, batch_correct=batch_correct) %>%
    arrange(rank_mean_all_anti)

  # perform the trend test using the Cox–Stuart (C–S) and Mann–Kendall (M–K) trend tests for
  #for the null hypothesis of no trend in the transformed standard deviations under several transformation
  #cs_trend <- trend::cs.test(exprs_normalised_df_plot$stdev_all_anti)
 # mk_trend <- trend::mk.test(exprs_normalised_df_plot$stdev_all_anti)

  mk_trend <- Kendall::MannKendall(exprs_normalised_df_plot$stdev_all_anti)



  cs_stuart <- "Cox-Stuart"
  m_kendall <- output_trend_stats('Mann-Kendall (tau stats)',mk_trend$sl[[1]],mk_trend$tau[[1]])



  normalisation_approaches <- c("Log2"="log2",
                                "VSN"="vsn",
                                "Cyclic Loess"="cyclic_loess",
                                "Cyclic Loess (log)"="cyclic_loess_log",
                                "RLM"="rlm")
  norm_method <- names(which(normalisation_approaches == method))

  p_norm <- ggplot(exprs_normalised_df_plot ,  aes(x=rank_mean_all_anti, y=stdev_all_anti)) +
    geom_jitter(color="red") + theme_classic() +
    expand_limits(y=c(0,10))+  stat_cor() +geom_smooth(color='blue',se = FALSE, size=0.5)+
    ggtitle(paste(norm_method,"Normalisation")) + xlab("pooled mean rank (mean of features by sample)") +
    ylab("pooled SD") +
    labs(caption=paste0(m_kendall , "\n", cs_stuart)) +
    theme(
      plot.caption = element_text(hjust = 0, color = "black", face = "italic")
    )
  return(p_norm)
}



#' Comparison of normalised data by feature
#'
#' @param exprs_normalised_df a normalised data frame
#' @param method the method of normalisation used
#' @param batch_correct the batch correction
#'
#' @import dplyr
#' @importFrom Kendall MannKendall
#' @return A ggplot of various normalisation approaches
#' @export
#'
#' @examples
#' matrix_antigen <- readr::read_csv(system.file("extdata", "matrix_antigen.csv", package="protGear"))
#' normlise_vsn <- matrix_normalise(as.matrix(matrix_antigen),
#' method = "vsn",
#' return_plot = FALSE
#' )
#' plot_normalised_antigen(normlise_vsn,method="vsn",batch_correct=FALSE)
plot_normalised_antigen <- function(exprs_normalised_df,method,batch_correct){
  ## how can we combine these two functions and make them the same
  antigen_summ <- exprs_normalised_df %>%
    gather(antigen,MFI) %>%
    group_by(antigen) %>%
    dplyr::summarise(mean_mfi=mean(MFI,na.rm = TRUE), sd_mfi=sd(MFI,na.rm = TRUE)) %>%
    dplyr::mutate(rank_mean_all_anti=rank(mean_mfi) ,
                  method=method, batch_correct=batch_correct) %>%
    arrange(rank_mean_all_anti)

  # perform the trend test using the Cox–Stuart (C–S) and Mann–Kendall (M–K) trend tests for
  #for the null hypothesis of no trend in the transformed standard deviations under several transformation
  #cs_trend2 <- trend::cs.test(antigen_summ$sd_mfi)
  #mk_trend2 <- trend::mk.test(antigen_summ$sd_mfi[!is.na(antigen_summ$sd_mfi)])

  #cs_stuart2 <- output_trend_stats("Cox-Stuart",cs_trend2$p.value, cs_trend2$statistic)
  #m_kendall2 <- output_trend_stats('Mann-Kendall',mk_trend2$p.value,mk_trend2$statistic )

  ## Changed here to use Kendall packages
  mk_trend2 <- Kendall::MannKendall(antigen_summ$sd_mfi[!is.na(antigen_summ$sd_mfi)])
  cs_stuart2 <- "Cox-Stuart" #output_trend_stats("Cox-Stuart",cs_trend2$p.value, cs_trend2$statistic[[1]])
  m_kendall2 <- output_trend_stats('Mann-Kendall (tau stats)',mk_trend2$sl[[1]],mk_trend2$tau[[1]])


  normalisation_approaches <- c("Log2"="log2",
                                "VSN"="vsn",
                                "Cyclic Loess"="cyclic_loess",
                                "Cyclic Loess (log)"="cyclic_loess_log",
                                "RLM"="rlm")
  norm_method <- names(which(normalisation_approaches == method))

  p_norm <- ggplot(antigen_summ ,  aes(x=rank_mean_all_anti, y=sd_mfi)) +
    geom_jitter(color="red") + theme_classic() +
    expand_limits(y=c(0,10))+  stat_cor() +geom_smooth(color='blue',se = FALSE, size=0.5)+
    ggtitle(paste(norm_method,"Normalisation")) + xlab("Pooled mean rank (mean of features)") +
    ylab("pooled SD") +
    labs(caption=paste0(m_kendall2 , "\n", cs_stuart2)) +
    theme(
      plot.caption = element_text(hjust = 0, color = "black", face = "italic")
    )

  return(p_norm)
}




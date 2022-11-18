#' Read multiple array files
#' @title Read array files
#' @param i  The name of the file which the data are to be read from.
#' @param data_path  The path where the file with the data  is located
#' @param genepix_vars  A list of specific definitions of the experiment design.
#' See \code{\link{array_vars}}.
#' @description This helps to read the chip file(s).
#' @return a number of data frames in the global environment
#' @importFrom  data.table fread %like%
#' @importFrom rlang parse_expr :=
#' @importFrom purrr set_names
#' @importFrom magrittr %>%
#' @export
#' @examples
#' ## Not run:
#' genepix_vars <- array_vars(
#' channel = "635",
#' chip_path = system.file("extdata", "array_data/machine1/", 
#' package="protGear"),
#' totsamples = 21,
#' blockspersample = 2,
#' mig_prefix = "_first",
#' machine = 1,
#' date_process = "0520"
#' )
#' file_read <- "KK2-06.txt"
#' read_array_files(i=file_read,
#' data_path=system.file("extdata", "array_data/machine1/",
#' package="protGear"), genepix_vars=genepix_vars)
#' ## End(Not run)
read_array_files <- function(i, data_path, genepix_vars) {
  ###loop through all the data to read
  # skip- the number of lines of the data file to skip before 
  #beginning to read data ---
  if (length(grep("Block.*Column|Column.*Block", 
                  readLines(file.path(data_path, i))) -
             1) == 1) {
    x <-
      grep("Block.*Column|Column.*Block", readLines(file.path(data_path, i))) -
      1
    #print(paste0(x,"_",i))
    d_f <-
      data.table::fread(file.path(data_path, i),
                        skip = x,
                        header = TRUE)
    ## arrange block to ensure the order is maintains
    d_f <- d_f %>% arrange(Block)
    ## calculate the different background  methods
    if (paste0(genepix_vars$BG) %ni% names(d_f)) {
      genepix_vars$BG <-
        names(d_f)[names(d_f) %like% paste0(genepix_vars$BG, '$')]
    }

    if (paste0(genepix_vars$FG) %ni% names(d_f)) {
      genepix_vars$FG <-
        names(d_f)[names(d_f) %like% paste0(genepix_vars$FG, '$')]
    }

    expression_med <-
      paste0("median(`", genepix_vars$BG, "`, na.rm = TRUE)")
    exp_minpos <-  paste0("minpositive(`", genepix_vars$BG, "`)")
    d_f <- d_f %>%

      mutate(global_BGMedian := !!parse_expr(expression_med)) %>%
      ## minimum BG per block and >1 -- Moving minimum approach per block
      group_by(Block) %>%

      mutate(minimum_BGMedian := !!parse_expr(exp_minpos)) %>%
      ungroup()


  }  else
    stop("File " , i, " does not have Block Column Specification")

}




#' Extract the background values
#' @title  extract bg
#' @param iden A character indicating the name of the object to be
#'  used under data_files.
#' @param data_files A list of data objects with names utilised by iden.
#' @param genepix_vars A list of specific definitions of the experiment design.
#'  See \code{\link{array_vars}}.
#' @description A generic function to extract the background 
#' data for micro array data.
#' @return A data frame of background values
#' @importFrom dplyr select arrange
#' @importFrom data.table %like%
#' @importFrom purrr set_names
#' @export
#'
#' @examples
#' ## Not run:
#' genepix_vars <- array_vars(
#' channel = "635",
#' chip_path = system.file("extdata", "array_data/machine1/", 
#' package="protGear"),
#' totsamples = 21,
#' blockspersample = 2,
#' mig_prefix = "_first",
#' machine = 1,
#' ## optional
#' date_process = "0520"
#' )
#' #Define the data path
#' data_path <- paste0(genepix_vars$chip_path)
#' # List the file names to use
#' filenames <- list.files(genepix_vars$chip_path,
#'                        pattern = '*.txt$|*.gpr$', full.names = FALSE
#' )
#' data_files <- purrr::map(
#'  .x = filenames,
#'   .f = read_array_files,
#'   data_path = data_path,
#'   genepix_vars = genepix_vars
#' )
#' data_files <- purrr::set_names(data_files, 
#' purrr::map(filenames, name_of_files))
#' names(data_files)
#' extract_bg(iden ="KK2-06" , data_files=data_files,genepix_vars=genepix_vars)
#' ## End(Not run)
extract_bg <- function(iden, data_files , genepix_vars = genepix_vars)
{
  ## read in the sample ID files
  ## this can be pulled from a mysql table
  ## if the sample ID is not available, we create an automated sampleID
  if (file.exists(file.path(genepix_vars$sampleID_path, 
                            paste0(iden , ".csv")))) {
    arraynames <-
      read.csv(
        file.path(genepix_vars$sampleID_path, paste0(iden , ".csv")) ,
        header = TRUE ,
        stringsAsFactors = FALSE ,
        colClasses = "character"
      )
  } else{
    warning(iden,
            " Not found in the sampleID files here",
            genepix_vars$sampleID_path)
    arraynames <- data.frame(
      v1 = (1:genepix_vars$totsamples) ,
      v2 = paste0("SID_gen", 1:genepix_vars$totsamples),
      barcode = iden
    )

  }


  ## replicate to the number of blocks
  ## make sure the block is arranged before merging with the data file
  arraynames <- arraynames %>%
    dplyr::select(v1, v2) %>%
    arrange(as.numeric(v1))

  ## capture errors for same sample ID in a slide
  if (length(unique(arraynames$v2)) < genepix_vars$totsamples) {
    sink("log_replicates.txt" , append = TRUE)
    warning("Most likely there is a repeated sample name for " , iden)
    sink()
    arraynames <- arraynames %>%
      group_by(v2) %>%
      mutate(index = 1:n()) %>%
      mutate(v2 = ifelse(index > 1, paste0(v2, "_", index), v2)) %>%
      select(-index)
  }


  ## get the data from the loop
  ## extract the specific data from the data files
  data <- data_files[[iden]]

  ## pick the spots per block from the data file
  spotsperblock <- table(data$Block)[[1]]
  sampleID <-
    rep(arraynames$v2, each = spotsperblock * genepix_vars$blockspersample)



  Data1 <- data %>%
    # assign respective sample number to each row
    mutate(
      sample = rep(
        seq_len(genepix_vars$totsamples),
        each = spotsperblock * genepix_vars$blockspersample
      ),
      # Bring in the sampleIDs..192 each sample
      sampleID = sampleID,
      # Abit of formating the Antigen names and concentration
      Name = gsub(':', '', Name),
      Name = gsub('\n', '', Name),
      Name = gsub(' ', '', Name)
    )  %>%
    ##remove uneccessary concs
    ## filter(!grepl('Landmark|Buffer|IgG', Name))   %>%
    # group by iden and antigen name
    group_by(sampleID, Name)

  if (paste0(genepix_vars$BG) %ni% names(Data1)) {
    genepix_vars$BG <-
      names(Data1)[names(Data1) %like% paste0(genepix_vars$BG, '$')]
  }

  if (paste0(genepix_vars$FG) %ni% names(Data1)) {
    genepix_vars$FG <-
      names(Data1)[names(Data1) %like% paste0(genepix_vars$FG, '$')]
  }

  #----------------------------------------------------------------------------
  ##save the MFI values of the Background
  data1_bg <-
    Data1 %>% dplyr::select(
      sampleID,
      antigen = Name,
      Block,
      FBG_Median = !!genepix_vars$FG ,
      BG_Median = !!genepix_vars$BG
    ) %>%
    mutate(replicate = 1:n()) %>%
    filter(
      !grepl(
        '^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]',
        antigen
      )
    )
  # combine Name and replicate
  #----------------------------------------------------------------------------
  return(data1_bg)
}





#' @title Plot background
#'
#' @param df A default dataset to use for plot.
#' @param bg_MFI A numeric \code{variable} describing which is the 
#' background MFI
#' @param x_axis The variable on the x axis
#' @param log_mfi a logical value indicating whether the MFI values should be
#' log transformed or not.
#'
#' @description  A generic function for plotting of R objects.
#' @return A ggplot of background values
#' @export
#' @import ggpubr
#' @examples
#' ## Not run:
#' #After extracting the background using \code{\link{extract_bg}} 
#' #we plot the data using
#' allData_bg <- readr::read_csv(system.file("extdata", "bg_example.csv",
#'  package="protGear"))
#' plot_bg(allData_bg,
#' x_axis = "antigen",
#' bg_MFI = "BG_Median",  log_mfi = TRUE
#' )
#' ## End(Not run)

plot_bg <- function(df,
                    x_axis = "antigen",
                    bg_MFI = "BG_Median",
                    log_mfi = TRUE) {
  ## create an id to help in having a numeric sample ID to sort your data
  ## this is because all sampleIDs from the samples were not unique
  ## rename the original sampleID sampleID2
  bg_MFI_sys <- rlang::sym(bg_MFI)
  ### check if the .id exists and renane it to slide
  if ('.id' %in% names(df)) {
    df <- df %>%  dplyr::rename(slide = .id)
  } else
    df <- df %>%  dplyr::mutate(slide = "slide")

  bg_plot <- df %>%
    dplyr::mutate(log_bg =    log2(!!bg_MFI_sys))

  # bg_plot$sampleID <- group_indices(.data =bg_plot )

  if (log_mfi == TRUE) {
    p_pubr <- ggboxplot(
      data = bg_plot ,
      x = x_axis ,
      y = "log_bg",
      facet.by = "replicate",
      ncol = 1
    )
    p_pubr <- ggpar(
      p_pubr,
      font.tickslab = c(6, "#993333"),
      xtickslab.rt = 45 ,
      ylab = "Background of the replicates (log2)"
    )
    p_bg <-
      ggplot(data = bg_plot , aes_string(x = x_axis , y = "log_bg")) +
      geom_boxplot() +
      facet_wrap( ~ replicate, ncol = 1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1))



  } else if (log_mfi == FALSE) {
    p_pubr <- ggboxplot(
      data = bg_plot ,
      x = x_axis ,
      y = bg_MFI,
      facet.by = "replicate",
      ncol = 1
    )
    p_pubr <- ggpar(
      p_pubr,
      font.tickslab = c(6, "#993333"),
      xtickslab.rt = 45 ,
      ylab = "Background of the replicates raw"
    )

    p_bg <-
      ggplot(data = bg_plot , aes_string(x = x_axis , y = bg_MFI)) +
      geom_boxplot() +
      facet_wrap( ~ replicate, ncol = 1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1))
  } else {
    # p_pubr <- ggboxplot(data = bg_plot ,
    #                     x="antigen" , y=bg_MFI,
    #                     facet.by = "replicate",ncol=1)
    # p_pubr <- ggpar(p_pubr,font.tickslab = c(8,"#993333"),
    #                 xtickslab.rt = 45 , ylab = "Background of the 
    #replicates raw")

    p_bg <-
      ggplot(data = bg_plot , aes_string(x = x_axis , y = bg_MFI)) +
      geom_boxplot() +
      facet_wrap( ~ replicate, ncol = 1) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45 , hjust = 1))
  }
  return(p_pubr)
}

#___________________________________________________


#' Plot foreground and background values
#' @title  plot_FB
#' @param df An object containing the data to which the plot is done.
#' @param antigen_name The \code{variable} describing which features/proteins/
#' antibodies in the data should be used to plot
#' @param bg_MFI A numeric \code{variable} describing which is the 
#' background MFI
#' @param FG_MFI A numeric \code{variable} describing which is the 
#' foreground MFI
#' @param log_mfi a logical value indicating whether the MFI values should be
#' log transformed or not.
#' @description A generic function for plotting the background and foreground
#'  values.
#' @return a ggplot of foreground vs background MFI values
#' @export
#' @import ggplot2 dplyr
#' @examples
#' ## Not run:
#' #After extracting the background using \code{\link{extract_bg}} 
#' #we plot the data using
#' allData_bg <- readr::read_csv(system.file("extdata", 
#' "bg_example.csv", package="protGear"))
#' plot_FB(allData_bg,
#' antigen_name = "antigen",
#' bg_MFI = "BG_Median", FG_MFI = "FBG_Median", log = FALSE
#' )
#' ## End(Not run)
plot_FB <-
  function(df,
           antigen_name = "antigen",
           bg_MFI = "BG_Median",
           FG_MFI = "FBG_Median",
           log_mfi = FALSE) {
    ## create an id to help in having a numeric sample ID to sort your data
    ## this is because all sampleIDs from the samples were not unique
    ## rename the original sampleID sampleID2
    bg_MFI_sys <- rlang::sym(bg_MFI)
    FB_MFI_sys <- rlang::sym(FG_MFI)
    ### check if the .id exists and renane it to slide
    if ('.id' %in% names(df)) {
      df <- df %>%  dplyr::rename(slide = .id)
    } else
      df <- df %>%  dplyr::mutate(slide = "slide")

    bg_plot <- df %>%
      mutate(log_bg = log2(!!bg_MFI_sys),
             log_fb = log2(!!FB_MFI_sys))



    if (log_mfi == TRUE) {
      p <- ggplot(bg_plot , aes(
        log_fb,
        log_bg,
        text = paste(
          "Antigen: ",
          antigen,
          "<br>FG: $",
          FBG_Median,
          "<br>B: $",
          BG_Median
        )
      )) +
        xlab("Foreground MFI") + ylab("Background MFI") +
        geom_jitter() +
        theme_light()



    } else if (log_mfi == FALSE) {
      p <- ggplot(bg_plot , aes(
        FBG_Median,
        BG_Median,
        text = paste(
          "Antigen: ",
          antigen,
          "<br>FG: $",
          FBG_Median,
          "<br>B: $",
          BG_Median
        )
      )) +
        xlab("Foreground MFI") + ylab("Background MFI") +
        geom_jitter() +
        theme_light()
    } else {
      p <- ggplot(bg_plot , aes(
        FBG_Median,
        BG_Median,
        text = paste(
          "Antigen: ",
          antigen,
          "<br>FG: $",
          FBG_Median,
          "<br>B: $",
          BG_Median
        )
      )) +
        xlab("Foreground MFI") + ylab("Background MFI") +
        geom_jitter() +
        theme_light()
    }
    return(p)
  }


#' Background correction
#' @title bg_correct
#' @param iden A character indicating the name of the object to be 
#' used under Data1
#' @param Data1 A data frame with sample identifiers merged with micro 
#' array data.
#' @param genepix_vars A list of specific definitions of the experiment design.
#'  See \code{\link{array_vars}}.
#' @param method a description of the background correction to be used. 
#'  Possible values are \code{"none","subtract_local",
#' "subtract_global","movingmin_bg","minimum_half","edwards" or "normexp"}. 
#' The default is \code{"subtract_local"}.
#' @details  The function implements background correction methods developed 
#' by \code{\link[limma]{backgroundCorrect}}. But the
#' \code{minimum_half and movingmin_bg} uses the block of the protein array as
#' the grid. If method="movingmin_bg" the minimum
#' background value within a  block is subtracted.
#' If method="minimum_half" then any intensity which is negative after 
#' background subtraction is reset to be equal to half the 
#' minimum positive value in
#' a block.  If method="movingmin_value" then any intensity which is negative 
#' after background subtraction is reset to the minimum positive value
#' in a block. For \code{edwards} we implement a similar algorithm with 
#' \code{limma::backgroundCorrect(method="edwards")} and for \code{'normexp'}
#' we use  the saddle-point approximation to maximum likelihood, 
#' \code{\link[limma]{backgroundCorrect}} for more details.
#' @description  A generic function to perform background correction.
#' @return A data frame with background corrected data
#' @export
#' @import  dplyr limma
#' @importFrom rlang sym
#' @examples
#' ## Not Run
#' genepix_vars <- array_vars(
#'   channel = '635',
#'   chip_path = system.file('extdata', 'array_data/machine1/',
#'   package='protGear'),
#'   totsamples = 21,
#'   blockspersample = 2,
#'   mig_prefix = '_first',
#'   machine = 1,
#'   date_process = '0520')
#' raw_df <- readr::read_csv(system.file('extdata','Data1_bg_sample.csv',
#'  package='protGear'))
#' bg_correct(iden='iden', Data1 = raw_df, genepix_vars = genepix_vars,
#'  method='subtract_local')
bg_correct <-
  function(iden, Data1, genepix_vars, method = "subtract_local") {
    #--------------------------------------------------------------------------
    if (paste0(genepix_vars$BG) %ni% names(Data1)) {
      genepix_vars$BG <-
        names(Data1)[names(Data1) %like% paste0(genepix_vars$BG, '$')]
      genepix_vars$BG <- rlang::sym(genepix_vars$BG)
    }

    if (paste0(genepix_vars$FG) %ni% names(Data1)) {
      genepix_vars$FG <-
        names(Data1)[names(Data1) %like% paste0(genepix_vars$FG, '$')]
      genepix_vars$FG <- rlang::sym(genepix_vars$FG)
    }


    ##save the MFI values without formatting the background
    data1_full_bg <- Data1 %>%
      dplyr::select(sampleID,
                    antigen = Name,
                    FMedian = !!genepix_vars$FG) %>%
      mutate(replicate = 1:n()) %>%
      ## removing Land mark and Buffer
      filter(
        !grepl(
          '^[Ll][Aa][Nn][Dd][Mm][Aa][Rr][Kk]|^[bB][Uu][Ff][Ff][Ee][Rr]',
          antigen
        )
      )
    # %>%     spread(antigen, F635Median)
    # combine Name and replicate
    ## save in the background data folder files with higher background values
    file_ident <- paste0(iden, "_raw_MFI_BG", ".csv")

    #create_dir(path = system.file("processe_data/raw_MFI_BG/"))
    #write_csv(data1_full_bg ,system.file("processed_data/raw_MFI_BG/", 
    #'file_ident', package="protGear"))
    #-----------------------------------------------------------------------
    if (method == "none" | method == "") {
      #------------------------------------------------------------------------
      ##MFI values without subtracting the background
      Data1 <- Data1 %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG ,
          BGMedian = !!genepix_vars$BG,
          Block,
          Column,
          Row
        ) %>%
        mutate(FMedianBG_correct = FMedian) %>%
        mutate(replicate = 1:n()) 
      #------------------------------------------------------------------------

    } else if (method == "subtract_local") {
      ## this approach subtracts the local background estimated by 
      #the Array Jet Machine
      #-------------------------------------------------------------------------
      ##save the MFI values with subtracting the background
      Data1 <- Data1 %>%
        dplyr::mutate(FMedianBG_correct = (!!genepix_vars$FG) - 
                        (!!genepix_vars$BG)) %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          FMedianBG_correct,
          Block,
          Column,
          Row
        ) %>%
        dplyr::mutate(replicate = 1:n())
      #------------------------------------------------------------------------

    } else if (method == "subtract_global") {
      ## this approach subracts the median of the backgrounds in a slide
      #------------------------------------------------------------------------
      ##save the MFI values with subtracting the background
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG - global_BGMedian) %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          FMedianBG_correct,
          Block,
          Column,
          Row
        ) %>%
        mutate(replicate = 1:n())
      #------------------------------------------------------------------------
    } else if (method == "movingmin_bg") {
      ## this is subtracted
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG-!!genepix_vars$BG) %>%
      ## this is generated while reading the array files using 
        #read_array_files function
        mutate(FMedianBG_correct = !!genepix_vars$FG - minimum_BGMedian) %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          FMedianBG_correct,
          Block,
          Column,
          Row
        ) %>%
        mutate(replicate = 1:n())

    } else if (method == "minimum_half") {
      ## this approach ensures all the MFI values are positive
      ## if the MFI <0 after subtraction the MFI is set to the half of the 
      #minimum corrected intensities
      #------------------------------------------------------------------------
      ##save the MFI values with subtracting the background
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG-!!genepix_vars$BG) %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          FMedianBG_correct,
          Block,
          Column,
          Row
        )  %>%
        group_by(Block) %>%
        mutate(FMedianBG_correct = ifelse(
          FMedianBG_correct < 0.1,
          (minpositive(FMedianBG_correct) /
             2),
          FMedianBG_correct
        )) %>%
        group_by(sampleID, antigen) %>%
        mutate(replicate = 1:n())
      #------------------------------------------------------------------------
    } else if (method == "minimum_value") {
      ## this approach ensures all the MFI values are positive
      ## if the MFI <0 after subtraction the MFI is set to the minimum of 
      #the corrected intensities
      #-------------------------------------------------------------------------
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG-!!genepix_vars$BG) %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          FMedianBG_correct,
          Block,
          Column,
          Row
        )  %>%
        group_by(Block) %>%
        mutate(FMedianBG_correct = ifelse(FMedianBG_correct < 0.1,
                                          (minpositive(
                                            FMedianBG_correct
                                          )), FMedianBG_correct)) %>%
        group_by(sampleID, antigen) %>%
        mutate(replicate = 1:n())

      #------------------------------------------------------------------------
    } else if (method == "edwards") {
      #a log-linear interpolation method is used to adjust lower intensities as 
      #in Edwards (2003).
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG-!!genepix_vars$BG)
      one <- matrix(1, nrow(Data1), 1)
      delta.vec <- function(d, f = 0.1) {
        ##  mean(d < 1e-16, na.rm = TRUE) % of values that are negative
        ## mean(d < 1e-16, na.rm = TRUE) * (1 + f) the % of values just 
        #above the negative values
        ## gives the quartile cut off value of the threshold
        quantile(d,
                 probs = mean(d < 1e-16, na.rm = TRUE) * (1 + f),
                 na.rm = TRUE)
      }
      #delta <- one %*% apply(as.matrix(Data1[['FMedianBG_correct']]), 
      #2, delta.vec)
      ## no need to multiply with 1 since its returning the same value 
      #and we want to implement in a data frame
      delta <-
        apply(as.matrix(Data1[['FMedianBG_correct']]), 2, delta.vec)

      ## each value its given its own value accordingly
      ## this helps maintain the variation
      Data1 <- Data1 %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          FMedianBG_correct,
          BGMedian = !!genepix_vars$BG,
          Block,
          Column,
          Row
        ) %>%
        group_by(Block) %>%
        mutate(FMedianBG_correct = ifelse(FMedianBG_correct < delta,
                                          (delta * exp(
                                            1 - (BGMedian + delta) / FMedian
                                          )), FMedianBG_correct)) %>%
        group_by(sampleID, antigen) %>%
        dplyr::mutate(replicate = 1:n())
    } else if (method == "normexp") {
      ##a convolution of normal and exponential distributions is fitted to the 
      #foreground intensities using
      #the background intensities as a covariate, and the expected signal given
      #the observed foreground becomes
      #the corrected intensity. This results in a smooth monotonic 
      #transformation of the background subtracted
      #intensities such that all the corrected intensities are positive.
      ##Both norm exp are implemented in Limma for DNA micro array data
      Data1 <- Data1 %>%
        mutate(FMedianBG_correct = !!genepix_vars$FG-!!genepix_vars$BG)
      E <- as.matrix(Data1[['FMedianBG_correct']])
      rownames(E) <- rownames(Data1)

      ## here we use Here "saddle" gives the saddle-point approximation to 
      #maximum likelihood from
      # Ritchie et al (2007) and improved by Silver et al (2009) 
      #--> check limma for details
      ## can we use offset--> updates
      bg_correct <-
        limma::backgroundCorrect.matrix(
          E = E,
          method = "auto",
          offset = 0,
          printer = NULL,
          normexp.method = "saddle",
          verbose = TRUE
        )
      bg_correct <- data.frame(FMedianBG_correct = bg_correct)

      ## select the important variables and join with the background 
      #corrected data
      Data1 <- Data1 %>%
        dplyr::select(
          sampleID,
          sample_array_ID,
          antigen = Name,
          FMedian = !!genepix_vars$FG,
          BGMedian = !!genepix_vars$BG,
          Block,
          Column,
          Row
        ) %>%
        bind_cols(bg_correct) %>%
        group_by(sampleID, antigen) %>%
        dplyr::mutate(replicate = 1:n())
    }
    #Data1 <- Data1 %>% rename(F635MedianB635=F635.Median...B635)

    return(Data1)
  }




#' Merge sample ID with the array data
#' @param iden A character indicating the name of the object to be used under
#' data_files.
#' @param data_files A list of data objects with names utilised by iden.
#' @param genepix_vars A list of specific definitions of the experiment design.
#'  See \code{\link{array_vars}}.
#' @param method A description of the background correction to be used. 
#' See \code{\link{bg_correct}}.
#' @description  A generic function that merges the protein data with the
#'  sample identifiers or sample names. If the file
#' does not have sample identifiers the function generates it automatically.
#' @return a data frame merged with corresponding sample ID's.
#' The sample ID are specified in the sample ID files
#' @export
#' @import dplyr
#' @importFrom purrr set_names
#' @examples
#' ## Not run:
#' ### Define the genepix_vars
#' genepix_vars <- array_vars(
#'   channel = "635",
#'   chip_path = system.file("extdata", "array_data/machine1/",
#'    package="protGear"),
#'   totsamples = 21,
#'   blockspersample = 2,
#'   mig_prefix = "_first",
#'   machine = 1,
#'   ## optional
#'   date_process = "0520"
#' )
#'
#' ## the path where the micro-array data is located
#' data_path <- paste0(genepix_vars$chip_path)
#' filenames <- list.files(genepix_vars$chip_path,
#'                         pattern = "*.txt$|*.gpr$", full.names = FALSE
#' )
#' ## create a list of all the files
#' data_files <- purrr::map(
#'  .x = filenames,
#'   .f = read_array_files,
#'   data_path = data_path,
#'   genepix_vars = genepix_vars
#' )
#' data_files <- purrr::set_names(data_files, 
#' purrr::map(filenames, name_of_files))
#' ## merge the lab data with samples and perform bg correction
#' merge_sampleID(iden = "KK2-06", data_files = data_files,
#'                genepix_vars =genepix_vars,method = "subtract_global" )
#' ## End(Not run)
merge_sampleID <- function(iden, data_files, genepix_vars, method)
{
  ## read in the sample ID files
  ## this can be pulled from a mysql table
  if (file.exists(file.path(genepix_vars$sampleID_path,
                            paste0(iden , ".csv")))) {
    arraynames <-
      read.csv(
        file.path(genepix_vars$sampleID_path, paste0(iden , ".csv")) ,
        header = TRUE ,
        stringsAsFactors = FALSE ,
        colClasses = "character"
      )
  } else{
    warning(iden,
            ".csv Not found in the sampleID files",
            genepix_vars$sampleID_path)
    arraynames <-
      data.frame(
        v1 = (1:genepix_vars$totsamples) ,
        v2 = paste0("SID_gen", 1:genepix_vars$totsamples),
        barcode = iden
      )
  }

  ## replicate to the number of blocks
  ## make sure the block is arranged before merging with the data file
  arraynames <- arraynames %>%
    dplyr::select(v1, v2) %>%
    arrange(as.numeric(v1))

  ## capture errors for same sample ID in a slide
  if (length(unique(arraynames$v2)) < genepix_vars$totsamples) {
    sink("log_replicates.txt" , append = TRUE)
    warning("Most likely there is a repeated sample name for " , iden)
    sink()
    arraynames <- arraynames %>%
      group_by(v2) %>%
      mutate(index = 1:n()) %>%
      mutate(v2 = ifelse(index > 1, paste0(v2, "_", index), v2)) %>%
      select(-index)
  }


  ## get the data from the loop
  ## extract the specific data from the data files
  data <- data_files[[iden]]

  ## pick the spots per block from the data file
  spotsperblock <- table(data$Block)[[1]]
  sampleID <-
    rep(arraynames$v2, each = spotsperblock * genepix_vars$blockspersample)
  sample_array_ID <-
    rep(arraynames$v1, each = spotsperblock * genepix_vars$blockspersample)


  Data1 <- data %>%
    # assign respective sample number to each row
    mutate(
      sample = rep(
        1:genepix_vars$totsamples,
        each = spotsperblock * genepix_vars$blockspersample
      ),
      # Bring in the sampleIDs..192 each sample
      sampleID = sampleID,
      sample_array_ID = sample_array_ID,
      # Abit of formating the Antigen names and concentration
      Name = gsub(':', '', Name),
      Name = gsub('\n', '', Name),
      Name = gsub(' ', '', Name)
    )  %>%
    ##remove uneccessary concs
    ## filter(!grepl('Landmark|Buffer|IgG', Name))   %>%
    # group by iden and antigen name
    group_by(sampleID, Name)

  ## DO the background correction
  ## specify the
  Data1 <- bg_correct(iden, Data1 , genepix_vars, method = method)
  Data1 <- Data1 %>%  mutate(iden = iden)
  return(Data1)
}



#' Read a gpr file to visualize
#'
#' @param infile a .gpr file to be used to visualize the expression intensities
#'  of the slide spots
#'
#' @return a data frame to visualize the background or foreground values
#' @export
#' @importFrom  data.table fread
#' @examples
#' ## Not run:
#' read_array_visualize(infile = system.file("extdata",
#' "/array_data/machine1/KK2-06.txt", package="protGear"))
#' ## End(Not run)
read_array_visualize <- function(infile) {
  x <- grep('Block.*Column|Column.*Block', readLines(infile))
  # d_f <- read.table(inFile$datapath,skip=x-1, header = T)
  d_f <- data.table::fread(infile, skip = x - 1, header = TRUE)
  return(d_f)
}

#' Visualize the slide mimicking the original scan image.
#'
#' @param infile a .gpr file to be used to visualize the expression 
#' intensities of the slide spots
#' @param MFI_var the MFI variable to plot, can be either the
#'  background or foreground value
#' @param d_f  a data frame with array data
#' @param interactive a logical to specify whether an interactive 
#' graph is returned or not
#'
#' @import htmltools ggplot2
#' @importFrom plotly ggplotly
#' @return A ggplot of slide foreground values
#' @export
#'
#' @examples
#' ## Not run:
#' visualize_slide(
#' infile = system.file("extdata", "/array_data/machine1/KK2-06.txt",
#'  package="protGear"),
#' MFI_var = "B635 Median"
#' )
#' ## End(Not run)
visualize_slide <-
  function(infile,
           MFI_var,
           interactive = FALSE,
           d_f = NA) {
    ## d_f only used for the shiny app
    if (is.na(d_f)) {
      d_f <- read_array_visualize(infile)

      d_f <- d_f %>%
        group_by(Block) %>%
        mutate(
          meanX = mean(X),
          meanY = mean(Y),
          maxY = max(Y),
          maxX = max(X),
          minY = min(Y),
          minX = min(X)
        )
    }
    ## define mid points to put the block labels
    MFI_var_sys <- rlang::sym(MFI_var)
    mid <- median(log(d_f[[MFI_var]]))
    labels <- sprintf(
      "<b>%s</b><br> MFI= %s ",
      d_f$Name,
      formatC(d_f$`F635 Median`, format = "d", big.mark = ",")
    ) %>%
      lapply(htmltools::HTML)

    point_size <- 0.5
    if (interactive == FALSE)
      point_size <- 1
    ## plot the visual slide
    p <- ggplot(d_f, aes(x = X, y = -Y, text = labels)) +
      #geom_rect(aes(xmin = minX, xmax = maxX, ymin = -minY, ymax = -maxY),
      #color = "black",alpha=0.0001,fill="blue") +
      geom_point(size = point_size, 
                 aes_string(colour = sprintf("log(`%s`)", MFI_var))) +
      theme_void() +
      theme(legend.position = "none")  +
      scale_color_gradient2(
        midpoint = mid,
        low = "blue",
        mid = "white",
        high = "red",
        space = "Lab"
      ) +
      geom_text(aes(
        x = meanX,
        y = -meanY,
        label = paste("Block", Block)
      ),
      color = "black",
      size = 2)

    if (interactive == FALSE) {
      return(p)
    } else if (interactive == TRUE) {
      p <- ggplotly(p, tooltip = 'text')
      return(p)
    }
  }


#' Visualize the slide mimicking the original scan image using a 2d plot.
#'
#' @param infile - a .gpr file to be used to visualize the expression 
#' intensities of the slide spots
#' @param MFI_var the MFI variable to plot, can be either the 
#' background or foreground value
#' @param d_f a data frame with array data
#'
#' @return A 2d plot of either the background or foreground values
#' @export
#' @import ggplot2
#' @examples
#' ## Not run:
#' visualize_slide_2d(
#' infile = system.file("extdata", "/array_data/machine1/KK2-06.txt", 
#' package="protGear"),
#' MFI_var = "B635 Median"
#' )
#' ## End(Not run)
visualize_slide_2d <- function(infile, MFI_var , d_f = NA) {
  if (is.na(d_f)) {
    d_f <- read_array_visualize(infile)

    d_f <- d_f %>%
      group_by(Block) %>%
      mutate(
        meanX = mean(X),
        meanY = mean(Y),
        maxY = max(Y),
        maxX = max(X),
        minY = min(Y),
        minX = min(X)
      )
  }


  mid <- median(log(d_f[[MFI_var]]))
  ggplot(data = d_f,
         aes_string(
           x = 'X',
           y = sprintf("-%s", 'Y'),
           #-Y,
           z =  sprintf("log(`%s`)", MFI_var)
         )) + #log(`F635 Median`)
    ## we have 24 vs 8 per block
    #stat_summary_2d(fun = median ,binwidth = c(40,120)) +
    stat_summary_2d(fun = median) +
    scale_fill_gradient2(
      midpoint = mid,
      low = "blue",
      mid = "white",
      high = "red",
      space = "Lab"
    ) +
    theme_void() +
    theme(legend.position = "none") +
    geom_text(aes(
      x = meanX,
      y = -meanY,
      label = paste("Block", Block)
    ),
    color = "black",
    size = 4)
}

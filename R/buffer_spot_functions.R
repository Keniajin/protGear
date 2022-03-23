#' Extract buffer spots of data
#'
#' @param Data1 An object of the class data frame
#' @param buffer_spot A character string containing the name of the buffer
#' spots.
#' @import dplyr
#' @importFrom dplyr select  filter
#' @description A function to extract the buffer spots data. A buffer spot only
#'  has the solution for
#'  proprietary ingredients for stabilizing protein and minimizing evaporation.
#' @return A data frame of the buffer control spots
#' @export
#'
#' @examples
#' bg_correct_df <- readr::read_csv(system.file("extdata", "Data1_sample.csv",
#' package="protGear"))
#' buffer_spots(Data1 = bg_correct_df)
buffer_spots <- function(Data1 , buffer_spot = "buffer") {
 Data2_buffer <- Data1 %>%
    # within each Name count sampleID. We had grouped this earlier
    # n() - gives number of observations in the current group
    mutate(replicate = 1:n()) %>%
    # Select only relevant variables
    dplyr::select(sampleID,
                  antigen,
                  replicate,
                  FMedianBG_correct,
                  Block,
                  Column,
                  Row)
  
  if (buffer_spot == 'buffer') {
    Data2_buffer <- Data2_buffer %>%
      filter(grepl('^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))
  } else {
    Data2_buffer <- Data2_buffer %>%
      filter(grepl(paste0('^', buffer_spot), tolower(antigen)))
  }
  
  
  Data2_buffer <- Data2_buffer %>%
    # combine Name and replicate
    unite(antigen, antigen, replicate)
  return(Data2_buffer)
}


#'  Plot the buffer values
#'
#' @param buffer_names A character string containing the name of the variable
#' with buffer spots. Default set to 'antigen'.
#' @param buffer_mfi A character string containing the name of the variable with
#' MFI value.Assuming background correction is done already.
#' Default to 'FMedianBG_correct'
#' @param slide_id  A character string containing the name of the slide/array
#' identifier variable.
#' @param df A data frame to be used to plot
#' @import dplyr ggplot2
#' @importFrom dplyr select  filter
#' @importFrom gtools mixedsort
#' @importFrom ggplot2 ggplot
#' @return plot of buffer spots
#' @export
#'
#' @examples
#'buffers <- readr::read_csv(system.file("extdata", "buffers_sample2.csv",
#'package="protGear"))
#' plot_buffer(df=buffers,buffer_names = "sampleID")
plot_buffer <- function(df = buffers,
                        buffer_names = "antigen",
                        buffer_mfi = "FMedianBG_correct",
                        slide_id = ".id") {
  x <- buffer_names
  y <- buffer_mfi
  df[[buffer_names]] <- factor(df[[buffer_names]] ,
                               levels = mixedsort(unique(
                                 as.character(df[[buffer_names]]))))
  
  
  p <- ggplot(data = df, aes_string(x = x, y = y)) +
    geom_jitter(aes_string(x = x, y = y, color = slide_id)) +
    geom_boxplot(aes_string(x = x, y = y), alpha = 0.2) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

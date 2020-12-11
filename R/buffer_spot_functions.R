

#' Extract buffer spor data
#'
#' @param Data1 An object of the class data frame
#' @description A function to extract the buffer spots data. A buffer spot only has the solution for
#'  proprietary ingredients for stabilizing protein and minimizing evaporation.
#' @return
#' @export
#'
#' @examples
buffer_spots <- function(Data1) {
  Data2_buffer <- Data1 %>%
    # within each Name count sampleID. We had grouped this earlier
    # n() - gives number of observations in the current group
    mutate(replicate = 1:n()) %>%
    # Select only relevant variables
    dplyr:::select(sampleID, antigen, replicate,FMedianBG_correct,
                   Block, Column, Row) %>%
    filter(grepl('^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   %>%
    # combine Name and replicate
    unite(antigen,antigen,replicate)
}


#'  Plot the buffer values
#'
#' @param buffer_names A character string containing the name of the buffer spot identifier variable. Default set to 'antigen'.
#' @param buffer_mfi A character string containing the name of the variable with MFI value.Assuming background correction is done already.
#' Default to 'FMedianBG_correct'
#' @param slide_id  A character string containing the name of the slide/array identifier variable.
#' @param df A data frame to be used to plot
#'
#' @return
#' @export
#'
#' @examples
plot_buffer <- function(df=buffers, buffer_names="antigen", buffer_mfi="FMedianBG_correct",slide_id=".id"){
  x=buffer_names
  y=buffer_mfi
  df[[buffer_names]] <- factor( df[[buffer_names]] , levels=mixedsort( unique(as.character( df[[buffer_names]] ))))


  ggplot(data = df,aes_string(x=x, y=y)) +
    geom_jitter(aes_string(x=x, y=y, color=slide_id))+
    geom_boxplot(aes_string(x=x, y=y), alpha = 0.2) +
    #  ggrepel::geom_text_repel(data=filter(buffers, F635MedianB635>5000),aes(label=.id), size=3)+
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


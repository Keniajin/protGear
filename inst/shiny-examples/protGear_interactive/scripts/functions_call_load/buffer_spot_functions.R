
## buffer_data_sets
## this can then be passed to other functions to extract the other details
## this can be passed to extract the replicates_data
###___________________________________________________
#' Title
#'
#' @param Data1 
#'
#' @return
#' @export
#'
#' @examples
buffer_spots <- function(Data1) {
  ##Select the buffer data since it has more than 3 repetions per sample and merge it way down
  ## the buffer takes the following position
  # block 1 of the sample - Column 6 ,7 and 8 rows 22,23,24
  # block 2 of the sample - column 1,2,3,4,5,6 rows 23,23,24
  # we utlise column 8 and 1 buffer spots
  Data2_buffer <- Data1 %>% 
    # within each Name count sampleID. We had grouped this earlier
    # n() - gives number of observations in the current group
    mutate(replicate = 1:n()) %>%
      #ungroup() %>%
    # Select only relevant variables
    dplyr:::select(sampleID, antigen, replicate,FMedianBG_correct,
                   Block, Column, Row) %>%
    filter(grepl('^[bB][Uu][Ff][Ff][Ee][Rr]', antigen))   %>% 
  ## removing the Buffer close to land mark
  #%>% filter(Column==8 | Column==1) %>% 
    # combine Name and replicate
    unite(antigen,antigen,replicate)
}        


#' Title
#'
#' @param df 
#' @param x 
#' @param y 
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


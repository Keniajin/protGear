##________________________________________________________________________###
###creating a pipe operator to negate the %in% operator
#https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html http://stackoverflow.com/questions/24941080/meaning-of-symbol-in-r
"%ni%" <- Negate("%in%")


minpositive <- function(x) min(x[x > 0], na.rm = T)



#' List the array structure variables
#'
#' @param channel A character indicating the channel that the data was scanned at. It is mostly included in the MFI variable names.
#' @param totsamples A numeric value indicating teh number of samples on a slide.
#' @param blockspersample A numeric value indicating the numer of blocks in a mini-array. The \code{".gal"} file can help in getting this
#' @param chip_path A character indicating the path of the folder  location with the array data.
#' @param sampleID_path A character indicating the path of the folder location with the sample identifiers matching the array structure.
#' @param mig_prefix Optional: A character indicating the identifier of an MIG dilution file
#' @param machine Optional:A character indicating the machine used to process the data in the folder
#' @param FG  Optional:A character indicating the name of the foreground variable name. if not specified its created as \code{paste0("F",channel,".Median")}
#' @param BG Optional:A character indicating the name of the background variable name.  if not specified its created as \code{paste0("B",channel,".Median")}
#' @param FBG Optional:A character indicating the name of the foreground - background variable name.  if not specified its created as \code{paste0("F",channel,".Median...B",channel)}
#' @param date_process Optional:A character indicating the date when the samples were processed.
#'
#' @description A generic function returning a list with the data structure.
#' @return
#' @export
#'
#' @examples
array_vars <- function(channel="635",
                       totsamples ,
                       FG="",
                       BG="",
                       FBG="",
                       blockspersample,
                       chip_path = "data/array_data",
                       sampleID_path ="data/array_sampleID/",
                       mig_prefix = "_first",
                       machine ="",
                       date_process = "" ){

  ####List the directories with the CHIP data###############
  paths <- list.dirs(path = chip_path, recursive =  T)
  ## remove the parent directory
  ## the folders with the chip data with the different batches is left
  paths <- paths[!grepl(paste0( chip_path,"$") , paths)]
  if(FG==""){
    FG= rlang::sym(paste0("F",channel,".Median"))
  }else{
    FG= rlang::sym(FG)
  }
  if(BG==""){
    BG= rlang::sym(paste0("B",channel,".Median"))
  }else{
    BG= rlang::sym(BG)
    }
  if(FBG==""){
    FBG= rlang::sym(paste0("F",channel,".Median...B",channel))
  }else{
    FBG= rlang::sym(FBG)
  }

  genepix_vars <- list(FG= FG,#rlang::sym(paste0("F",channel,".Median")),
                       BG= BG,#rlang::sym(paste0("B",channel,".Median")) ,
                       FBG= FBG,#rlang::sym(paste0("F",channel,".Median...B",channel)),
                       paths=paths,
                       chip_path=chip_path,
                       sampleID_path=sampleID_path,
                       mig_prefix=mig_prefix,
                       machine=machine,
                       date_process=date_process,
                       totsamples=totsamples,
                       blockspersample=blockspersample,
                       mp=machine,
                       dp=date_process)
  return(genepix_vars)
}





#' Title Create directory function
#'
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
create_dir <- function(path){
  if(!file.exists(paste0(path))) {
    dir.create(paste0(path))
  }else warning(paste0("The folder",path," already exists"))
}


###___________________________________________________

#' Object names of a list
#'
#' @param i - a list filenames with .txt or .gpr extension
#'
#' @return
#' @export
#' @description A generic function returning a vector with the names of files in the same directory
#' @examples
name_of_files <- function(i) {
  name <- gsub("\\.txt*|\\.gpr*", "", i, perl = TRUE)
  name <- gsub(" repeat", "", name, perl = TRUE)
  name <- gsub(" ", "_", name, perl = TRUE)
}


#___________________________________________________
# Function to be called in case of replicated error

#'         \\\_Start_Function_For Error\\\         #
#'
#' @description A generic function to write into the log file with a replicate check error
#' @param iden An id for the file with replicates error
error_replicates <- function(iden) {
  sink("errors/error_replicates.txt" , append = T)
  print(paste0("The replicates per antigen per sample are more than expected for ", iden))
  sink()
}
#'         \\\_End_Function_\\\         #
#___________________________________________________




###
#' Check existing sample ID names
#' @param genepix_vars A list of specific definitions of the experiment design. See \code{\link{array_vars}}.
#' @description  A generic function to check if the file(s) witht the MFI values have a corresponding sample ID file. Sample ID file is
#' a file with the identifiers for the samples in array file.
#' @return
#' @export
#'
#' @examples
check_sampleID_files <- function(genepix_vars){
  ## copy all sample ID with missing CSV file
  ##
  sid_files <- gsub(".csv", "",list.files(genepix_vars$sampleID_path))
  ## check if all the chip files have an existing sampleID file
  sid_check <- gsub(".txt|.gpr", "",list.files(genepix_vars$chip_path , recursive = T,
                                               pattern="*.txt|*.gpr", full.names=F))
  ## convert all the file names to caps to avoid merge errrors due to case
  sid_check <- toupper(sub(".*/(.*)", "\\1", sid_check))

  ## missing sampleID for any given file
  miss_id_file <- sid_check[sid_check  %ni%  toupper(sid_files)]


  ## create a folder to collect the errrors
  if(dir.exists("errors")==F){
    dir.create("errors")
  }
  if(length(miss_id_file)>0){
    write.table(miss_id_file,"errors/missing_IDfile.txt")
  }

  if(length(miss_id_file)==0){
    warning("All array files have a corresponding sampleID file")
    return(0)
  }else{
    return(miss_id_file)
  }

}

##________________________________________________________________________###
###creating a pipe operator to negate the %in% operator
#https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html http://stackoverflow.com/questions/24941080/meaning-of-symbol-in-r
"%ni%" <- Negate("%in%")

minpositive <- function(x) min(x[x > 0], na.rm = T)

## cretae the variables in the 

array_vars <- function(channel="635",
                       ### added only on shiny
                       FG,
                       BG, 
                       totsamples ,
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
  
  genepix_vars <- list(FG=rlang::sym(paste0(FG)),
                       BG=rlang::sym(paste0(BG)),
                       #FB= rlang::sym(paste0("F",channel,".Median")),
                       #BG= rlang::sym(paste0("B",channel,".Median")) ,
                   #FBG= rlang::sym(paste0("F",channel,".Median...B",channel)),
                   FBG=rlang::sym(paste0(FG,'...',gsub(".Median","",BG))),
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




###___________________________________________________
## create directory function
#' Title
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
#' Title - This function gives the names of the files 
#'
#' @param i 
#'
#' @return
#' @export
#'
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
#'         
error_replicates <- function(iden) { 
  sink("errors/error_replicates.txt" , append = T)
  print(paste0("The replicates per antigen per sample are more than expected for ", iden))
  sink()
}
#'         \\\_End_Function_\\\         #
#___________________________________________________




### check 
### check 
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
  miss_id_file <- sid_check[sid_check %ni%  toupper(sid_files)]
  
  
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

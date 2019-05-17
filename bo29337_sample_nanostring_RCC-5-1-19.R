
# function to extract SampleIDs from /nanostring/rawdata .ZIP folders containing .RCC files
# and extract the SampleIDs from the .RCC file name
# Variable needed to execute the function is the studypath, for example
# studypath <- "/gne/data/obdroot/PDL1mab/go29294"
#####################################################################################
bo29337_sample_nanostring_RCC <- function(studypath){
  report <- NULL
  report <- as_tibble(report)
  nano_raw <- paste(studypath, "/nanostring/rawdata", sep="")
  ZIPfolders <- nano_raw %>% dir(full.names=T, recursive=T) %>% 
    str_subset(".ZIP|.zip") %>% str_ignore("erroneous")
  
  if(length(ZIPfolders)==0){
    print("No nanostring .ZIP folders found")
    stop("No nanostring .ZIP folders found")
  }
  
  for(i in seq_along(ZIPfolders)){ 
    RCCfiles <- ZIPfolders[i] %>% unzip(list=T) %>% select(Name) %>% pull()
    if(RCCfiles %>% 
       grepl("[a-zA-Z]{2}-[0-9]{7}-[a-zA-Z]{1}[0-9]{2}|[0-9]{8}[A-Z]{1}[0-9]{4}[A-Z]{1}", .) %>%
       all()==T){
      SampleID <- RCCfiles %>% lapply(str_extract, 
                                      "[a-zA-Z]{2}-[0-9]{7}-[a-zA-Z]{1}[0-9]{2}|[0-9]{8}[A-Z]{1}[0-9]{4}[A-Z]{1}") %>% 
        unlist(use.names=F) %>% toupper()
      File_Name <- RCCfiles %>%
        lapply(str_subset, "[a-zA-Z]{2}-[0-9]{7}-[a-zA-Z]{1}[0-9]{2}|[0-9]{8}[A-Z]{1}[0-9]{4}[A-Z]{1}") %>% 
        unlist(use.names=F)
      File_Path <- ZIPfolders[i]
      combine <- cbind(File_Path, File_Name, SampleID)
      report <- rbind2(report, combine) %>% distinct()
    } else {
      # if the above condition fails - add the file to the sample_missed_files
      # report and move to the next iteration 
      file <- ZIPfolders[i]
      samp <- "sample_nanostring_RCC"
      sample_missed_files(file, studypath, samp)
      cat("Zip folder ", ZIPfolders[i], "\n could not be processed - see sample_missed_files report\n")
      next
    }
  }
  study <- basename(studypath)
  if (!file.exists(paste0(getwd(), "/", study, "_sample_to_file.csv"))){
    write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=T)
  } else {
    write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=F)
  }
  print("Report on .RCC files generated in your home directory")
}
#####################################################################################

# Cong changed line 22, 25 and 28 "[0-9]{6}" to "[0-9]{7}" to be in line with naming convention of study bo29337 nanostring RCC files.


#' sample_ihc
#' 
#' sample_ihc function reads .csv files and image files 
#' from the ihc/ folders and maps SampleIDs to 
#' file names and file paths
#' 
#' @description sample_ihc function requires 
#' dsi_sample_mapping_table and will open the window asking the user
#' to choose dsi_sample_mapping_table appropriate for present study
#' 
#' @param studypath for example
#' studypath <- "/gne/data/obdroot/PDL1mab/go29294"
#' @param dsi_map - imported dsi_sample_mapping_table for the study
#' 
#' @export
bo29337_sample_ihc <- function(studypath, dsi_map){
  report <- NULL
  report <- as_tibble(report)
  
  ihc_dir <- paste(studypath, "/ihc/rawdata", sep="")
  ihc_files <- ihc_dir %>% dir(full.names=T, recursive=T)
  
  if(length(ihc_files)==0){
    print("No files in ihc/rawdata folder have been found")
    stop("No files in ihc/rawdata folder have been found")}
  
  Sample_IDs_dsi <- dsi_map$SAMPLE_ID %>% toupper() %>% sort() # character vector of 
  # SampleIDs read from dsi_mapping_table, coersed to upper case
  
  # use regexIDs_dsi_table function to get a string of regular expressions 
  # representing unique SampleID formats in the dsi_sample_mapping_table
  dsi_SampleIDs <- regexIDs_dsi_table(Sample_IDs_dsi)
  one_of_dsi <- paste(dsi_SampleIDs, collapse="|")
  
  # collect all ihc image files in the ihc/rawdata/image folder and
  # map SampleIDs to file names/paths, print in a report
  ihc_image_dir <- paste(studypath, "/ihc/rawdata/image", sep="")
  ihc_image_files <- ihc_image_dir %>% dir(full.names=T, recursive=T) %>% 
    str_ignore(".csv|.CSV") %>% str_ignore("thumbs|Thumbs")
  
  if(length(ihc_image_files)==0){
    print("No ihc/rawdata image files found")}
  # isolate the file and the folder name from the whole file path
  # to avoid extracting wrong parts of the filename as SampleID
  ihc_image_file_folder <- paste(ihc_image_files %>% toupper() %>% dirname(.) %>%
                                   basename(.), ihc_image_files %>% toupper() %>% basename(.),sep="_")
  
  SampleID <- ihc_image_file_folder  %>% basename (.) %>% str_extract(one_of_dsi)
  File_Path <- ihc_image_files
  File_Name <- basename(File_Path)
  
  combine <- cbind(File_Path, File_Name, SampleID)
  report <- rbind2(report, combine) %>% distinct()
  #####
  # collect all .xlsx files in the ihc/rawdata folder into ihc_xlsx_files vector
  ihc_xlsx_raw <- paste(studypath, "/ihc/rawdata", sep="")
  ihc_xlsx_files <- ihc_xlsx_raw %>% dir(full.names=T, recursive=T) %>% 
    str_ignore("image") %>% str_subset(".xlsx|.XLSX")
  
  # if no .xlsx files found - stop the script and print out the error message
  if(length(ihc_xlsx_files)==0){
    study <- basename(studypath)
    if (!file.exists(paste0(getwd(), "/", study, "_sample_to_file.csv"))){
      write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=T)
    } else {
      write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=F)
    }
    print("Report on ihc files in /ihc/rawdata was generated in your home directory")
    stop("No ihc .xlsx files found")}
  
  # column names for SampleIDs in the .xlsx files in ihc/rawdata folders
  Sampleid_colnames <- c("scene_name", "Slide name","TMA Core Position", "Patient ID", 
                         "HGX Sample ID", "Accession ID", "Block ID", "Visit Code", "% Marker Area",
                         "Marker Area (µm²)", "Hematoxylin Area (µm²)", "No Stain Area (µm²)",
                         "# Marker Areas", "Comments") 
  # add column names if needed
  
  for(i in 1:length(ihc_xlsx_files)){
    tryCatch({xlsx_file_i <- ihc_xlsx_files[i] %>% read_excel(col_names=T) %>%
      as_tibble()},
      error=function(e) {cat("Bad file pattern:\n", excel_files[i], "\n")}
    )
    
    if(any(colnames(xlsx_file_i) %in% Sampleid_colnames)){
      # if the above condition is true - extract SampleIDs and File names
      # and paths into the report table
      SampleID <- xlsx_file_i %>%   
        select("HGX Sample ID") %>% unlist(use.names=F) %>% toupper() %>%
        str_extract(one_of_dsi)
      File_Path <- ihc_xlsx_files[i]
      File_Name <- basename(ihc_xlsx_files[i]) 
      
      combine <- cbind(File_Path, File_Name, SampleID)
      report <- rbind2(report, combine) %>% distinct() %>% na.omit()
    } else {
      # if above condition in not true - add the file to the sample_missed_files
      # report and move to the next iteration
      file <- ihc_xlsx_files[i]
      samp <- "sample_ihc"
      sample_missed_files(file, studypath, samp)
      cat("File ", ihc_xlsx_files[i], "\n could not be processed - see sample_missed_files report")
      next
    }
  }
  # print sample to file mapping report or append to the existing one
  study <- basename(studypath)
  if (!file.exists(paste0(getwd(), "/", study, "_sample_to_file.csv"))){
    write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=T)
  } else {
    write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=F)
  }
  print("Report on ihc files in /ihc/rawdata was generated in your home directory")
}

#######################################
# Cong modified the funtion by changing everything about csv to xlsx, because the IHC data for study bo29337 are in format of Excel files.




#' sample_fluidigm
#' 
#' sample_fluidigm function reads excel and .csv files from the fluidigm/ folders
#' and maps SampleIDs to file names and file paths
#' 
#'  @param studypath for example
#' studypath <- "/gne/data/obdroot/PDL1mab/go29294"
#' @export
bo29337_sample_fluidigm <- function(studypath){
  
  report <- NULL
  report <- as_tibble(report)
  
  # Collect all fluidigm rawdata files
  fluidigm_raw <- paste(studypath, "/fluidigm/rawdata", sep="")
  fluidigm_files <- fluidigm_raw %>% dir(full.names=T, recursive=T)
  
  # Subset all .csv and .xlsx fluidigm files (excluding .tif files):
  fluidigm_files_excel <- fluidigm_files %>% str_subset(".csv|.xlsx|.xls")
  
  # Map .xlsx files
  fluidigm_files_xlsx <- fluidigm_files_excel %>% str_subset(".xlsx")
  
  
  Sampleid_colnames <- c("Specimen_Name", "FMI SAMPLE ID", "FMID", "Specimen Name", 
                         "Specimen", "FMI", "specimenName", "SpecimenName", "specimen_name",
                         "specimen name", "Sample", "SAMPLE", "sampleID", "Specimen ID", "Name", "SMPID")
  
  for (i in seq_along(fluidigm_files_xlsx)){
    tryCatch({xlsx_file <- read_excel(fluidigm_files_xlsx[i], col_names=T, skip=3)},
             error=function(e) {cat("Bad file pattern:\n", fluidigm_files_xlsx[i], "\n")}
    )
    #check if column name of the 1st column of the text file is contained
    # in the Sampleid_colnames and the format of SampleIDs in the first
    # column consists of three letters followed by six numbers
    if(any(colnames(xlsx_file) %in% Sampleid_colnames)){
      # if the above condition is true - extract SampleIDs and File names
      # and paths into the report table
      SampleID <- xlsx_file %>% 
        select(one_of(Sampleid_colnames)) %>% pull() %>% toupper()
      File_Path <- fluidigm_files_xlsx[i]
      File_Name <- basename(File_Path)
      
      combine <- cbind(File_Path, File_Name, SampleID)
      report <- rbind2(report, combine) %>% distinct()
    } else {
      # if above condition in not true - add the file to the sample_missed_files
      # report and move to the next iteration
      file <- fluidigm_files_xlsx[i]
      samp <- "sample_fluidigm"
      sample_missed_files(file, studypath, samp)
      cat("File ", fluidigm_files_xlsx[i], "\n could not be processed - see sample_missed_files report")
      next
    }
  }
  
  if(nrow(report)!=0){
    # print sample to file mapping report or append to the existing one
    study <- basename(studypath)
    if (!file.exists(paste0(getwd(), "/", study, "_sample_to_file.csv"))){
      write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=T)
    } else {
      write_csv(report, paste0(getwd(), "/", study, "_sample_to_file.csv"), append=T, col_names=F)
    }
    print("Report on fluidigm xlsx files in /fluidigm/rawdata was generated in your home directory")
  } 
  
  # Map .csv files
  fluidigm_files_csv <- fluidigm_files_excel %>% str_subset(".csv")
  
  for (i in seq_along(fluidigm_files_csv)){
    tryCatch({csv_file <- read_csv(fluidigm_files_csv[i], col_names=T, skip=11)},
             error=function(e) {cat("Bad file pattern:\n", fluidigm_files_csv[i], "\n")}
    )
    #check if column name of the csv file is contained
    # in the Sampleid_colnames and the format of SampleIDs in the first
    # column consists of three letters followed by six numbers
    if(any(colnames(csv_file) %in% Sampleid_colnames)){
      # if the above condition is true - extract SampleIDs and File names
      # and paths into the report table
      SampleID <- csv_file[,2] %>% pull() %>% toupper()
      File_Path <- fluidigm_files_csv[i]
      File_Name <- basename(File_Path)
      
      combine <- cbind(File_Path, File_Name, SampleID)
      report <- rbind2(report, combine) %>% distinct()
    } else {
      # if above condition in not true - add the file to the sample_missed_files
      # report and move to the next iteration
      file <- fluidigm_files_xlsx[i]
      samp <- "sample_fluidigm"
      sample_missed_files(file, studypath, samp)
      cat("File ", fluidigm_files_csv[i], "\n could not be processed - see sample_missed_files report")
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
  print("Report on fluidigm csv files in /fluidigm/rawdata was generated in your home directory")
  
  Report_File_Path <- report$File_Path %>% unique()
  
  total_files <- fluidigm_files_excel
  
  difference <- setdiff(total_files, Report_File_Path) %>% 
    str_ignore("summary|Summary|erroneous|contents|Contents|Analysis|analysis|standard|annotated|Thumbs|thumbs|readme|manifest|Manifest|sas7bdat")
  
  backup_options <- options()
  options(max.print=999999)
  
  currentDate <- Sys.Date() %>% str_replace_all("-", "_")
  study <- basename(studypath)
  file.name <- paste(study, "_sample_to_file_report_", currentDate, ".txt", sep="")
  sink(file.name, append=T, split=T)
  cat("\n\n", study, "_sample to file mapping report\n\n")
  print(as.data.frame(Sys.info()))
  cat("\n########################################\n\n")
  # print the difference between all files in the folders and mapped files
  # and precentage of files mapped
  if(length(difference)==0){
    print("All files were included")} else{
      percent <- round((length(Report_File_Path)/length(total_files))*100, digits=1)
      cat(percent, "% of files were mapped in ", study, "fluidigm rawdata folders\n\n")
      cat("The following files were excluded from the sample to file mapping report for study ", study, ":\n\n")
      cat("The list below excludes files that do not contain SampleIDs, such as files containing\n")
      cat("summary, contents, analysis, standard, annotated, thumbs, readme - in the file or folder name\n\n")
      print(difference)}
  cat("###########################################")
  
  while (sink.number()>0) sink()
  options(backup_options)
}

################################

# Cong modified original function by changing argument skip =10 tp skip =3 (line 30) to make it useful for study bo29337.




  
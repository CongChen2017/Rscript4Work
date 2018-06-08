#set work path to source file folder
setwd("Desktop")

#clean enviroment
rm(list=ls())

#read data file
rawdata <- read.csv("NewCounts.Jan2017.csv", stringsAsFactors = FALSE)

#keep ATII cell data
colnames <- colnames(rawdata)
keep <- grepl("ATII", colnames)
keep[1] <- TRUE
ATIIdata <- rawdata[,keep]

#format data table
rownames(ATIIdata) <- ATIIdata$Symbol #add symbol as row names
ATIIdata$Symbol <- NULL #remove Symbol column
colnames(ATIIdata) <- NULL #remove column names

#input group info
group <- c("ATII.Ctrl.O", "ATII.SPChomo.O")

#generate output files for Prism
for(name in rownames(ATIIdata)) {  #for each gene
  gene <- ATIIdata[name,]    #generate data frame with only one gene
  gene.1 <- gene[1:3]    #generate data frame for 1st group
  gene.1[4] <- "NA"         #add a column to make consistent with groups with 4 samples
  colnames(gene.1) <- NULL  #remove column names
  gene.2 <- gene[4:7]       #same to the last group
  colnames(gene.2) <- NULL
  
  #generate a new data frame with transposed data from each group
  PrismData <- data.frame(t(gene.1), t(gene.2))
  #rename column names as group names
  colnames(PrismData) <- group
  #genearte output files for prism
  write.csv(PrismData, file= paste("~/Desktop/Results/","Prism.Jan2017.ATII.",name,".csv", sep=""), 
            row.names = FALSE)
  
}


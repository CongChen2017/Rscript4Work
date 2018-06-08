#set work path to source file folder
setwd("Desktop")

#clean enviroment
rm(list=ls())

#read data file
rawdata <- read.csv("NewCounts.Sep2016.csv", stringsAsFactors = FALSE)

#keep ATII cell data
colnames <- colnames(rawdata)
keep <- grepl("ATII", colnames)
keep[1] <- TRUE
ATIIdata <- rawdata[,keep]

#remove LysM data
colnames <- colnames(ATIIdata)
keep <- !grepl("LysM", colnames)
ATIIdata <- ATIIdata[,keep]

#format data table
rownames(ATIIdata) <- ATIIdata$Symbol #add symbol as row names
ATIIdata$Symbol <- NULL #remove Symbol column
colnames(ATIIdata) <- NULL #remove column names

#input group info
group <- c("ATII.Ctrl.Y", "ATII.SPChomo.Y", "ATII.SPChetz.Y", "ATII.Ctrl.M",
           "ATII.SPChomo.M", "ATII.SPChetz.M", "ATII.Ctrl.O", "ATII.SPChetz.O")

#generate output files for Prism
for(name in rownames(ATIIdata)) {  #for each gene
  gene <- ATIIdata[name,]    #generate data frame with only one gene
  gene.1 <- gene[1:3]    #generate data frame for 1st group
  gene.1[4] <- "NA"         #add a column to make consistent with groups with 4 samples
  colnames(gene.1) <- NULL  #remove column names
  gene.2 <- gene[4:6]       #same to the last group
  gene.2[4] <- "NA"
  colnames(gene.2) <- NULL
  gene.3 <- gene[7:9]
  gene.3[4] <- "NA"
  colnames(gene.3) <- NULL
  gene.4 <- gene[10:13]
  colnames(gene.4) <- NULL
  gene.5 <- gene[14:17]
  colnames(gene.5) <- NULL
  gene.6 <- gene[18:21]
  colnames(gene.6) <- NULL
  gene.7 <- gene[22:24]
  gene.7[4] <- "NA"
  colnames(gene.7) <- NULL
  gene.8 <- gene[25:28]
  colnames(gene.8) <- NULL
  #generate a new data frame with transposed data from each group
  PrismData <- data.frame(t(gene.1), t(gene.2), t(gene.3), t(gene.4),
                          t(gene.5), t(gene.6), t(gene.7), t(gene.8))
  #rename column names as group names
  colnames(PrismData) <- group
  #change 0 values to NA
  for(x in 1:8) {
    if(PrismData[4,x] == 0)
    PrismData[4,x] <- NA
  }
  #genearte output files for prism
  write.csv(PrismData, file= paste("~/Desktop/Results/","Prism.Sep2016.ATII.",name,".csv", sep=""), 
            row.names = FALSE)
  
}


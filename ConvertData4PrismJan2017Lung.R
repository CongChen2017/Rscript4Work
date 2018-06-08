#set work path to source file folder
setwd("Desktop")

#clean enviroment
rm(list=ls())

#read data file
rawdata <- read.csv("NewCounts.Jan2017.csv", stringsAsFactors = FALSE)

#keep Lung data
colnames <- colnames(rawdata)
keep <- grepl("Lung", colnames)
keep[1] <- TRUE
Lungdata <- rawdata[,keep]

#remove LysM data
colnames <- colnames(Lungdata)
keep <- !grepl("LysM", colnames)
Lungdata <- Lungdata[,keep]

#format data table
rownames(Lungdata) <- Lungdata$Symbol #add symbol as row names
Lungdata$Symbol <- NULL #remove Symbol column
colnames(Lungdata) <- NULL #remove column names

#input group info
group <- c("Lung.Ctrl.M", "Lung.SPChomo.M", "Lung.SPChetz.M")

#generate output files for Prism
for(name in rownames(Lungdata)) {  #for each gene
  gene <- Lungdata[name,]    #generate data frame with only one gene
  gene.1 <- gene[1:6]    #generate data frame for 1st group
  colnames(gene.1) <- NULL  #remove column names
  gene.2 <- gene[7:10]       
  gene.2[5] <- "NA"
  gene.2[6] <- "NA"
  colnames(gene.2) <- NULL
  gene.3 <- gene[11:14]
  gene.3[5] <- "NA"
  gene.3[6] <- "NA"
  colnames(gene.3) <- NULL
  #generate a new data frame with transposed data from each group
  PrismData <- data.frame(t(gene.1), t(gene.2), t(gene.3))
  #rename column names as group names
  colnames(PrismData) <- group
  #genearte output files for prism
  write.csv(PrismData, file= paste("~/Desktop/Results/","Prism.Jan2017.Lung.",name,".csv", sep=""), 
            row.names = FALSE)
  
}


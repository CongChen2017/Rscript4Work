#set work path to source file folder
setwd("Desktop")

#clean enviroment
rm(list=ls())

#read data file
rawdata <- read.csv("NewCounts.Sep2016.csv", stringsAsFactors = FALSE)

#keep AM cell data
colnames <- colnames(rawdata)
keep <- grepl("AM", colnames)
keep[1] <- TRUE
AMdata <- rawdata[,keep]

#remove LysM data
colnames <- colnames(AMdata)
keep <- !grepl("LysM", colnames)
AMdata <- AMdata[,keep]

#format data table
rownames(AMdata) <- AMdata$Symbol #add symbol as row names
AMdata$Symbol <- NULL #remove Symbol column
colnames(AMdata) <- NULL #remove column names

#input group info
group <- c("AM.Ctrl.M", "AM.SPChomo.M", "AM.SPChetz.M")

#generate output files for Prism
for(name in rownames(AMdata)) {  #for each gene
  gene <- AMdata[name,]    #generate data frame with only one gene
  gene.1 <- gene[1:4]    #generate data frame for 1st group
  colnames(gene.1) <- NULL  #remove column names
  gene.2 <- gene[5:8]       #same to the last group
  colnames(gene.2) <- NULL
  gene.3 <- gene[9:12]
  colnames(gene.3) <- NULL
  #generate a new data frame with transposed data from each group
  PrismData <- data.frame(t(gene.1), t(gene.2), t(gene.3))
  #rename column names as group names
  colnames(PrismData) <- group
  #genearte output files for prism
  write.csv(PrismData, file= paste("~/Desktop/Results/","Prism.Sep2016.AM.",name,".csv", sep=""), 
            row.names = FALSE)
  
}


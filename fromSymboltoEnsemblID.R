rm(list=ls()) #clean environment
library(org.Mm.eg.db) #load library
setwd("Desktop") #set working diretory
#read raw gene list
genelist <- read.csv("gene_list.csv", stringsAsFactors = FALSE)
head(genelist)
#convert gene symbol to lowrcases
genelist$Symbol <- tolower(genelist$Symbol)
head(genelist)
#generate a function to convert first letter to uppercase
firstup <- function(x) {
  substr(x,1,1) <- toupper(substr(x,1,1))
  x
}
#run the new function and change all gene symbols' first letter to uppercase
for (x in 1:117) {
  genelist[x,1] <- firstup(genelist[x,1])
}
head(genelist)
#convert gene symbols to Ensembl IDs
genelist$Ensembl <- mapIds(org.Mm.eg.db, genelist$Symbol,
                           keytype="SYMBOL", column="ENSEMBL")
head(genelist)
#delete Symbol column
genelist$Symbol <- NULL
head(genelist)
#get rid of NAs from gene list
genelist <- na.omit(genelist)
#change column name to GeneID
colnames(genelist) <- "GeneID"
#generate output file for genelist
write.csv(genelist, file="EnsemblID.csv", row.names = F)



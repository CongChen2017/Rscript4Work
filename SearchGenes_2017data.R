#load library
library(org.Mm.eg.db)

#set work path to source file folder
setwd("Desktop")

#clean enviroment
rm(list=ls())

#read files, gene list first and then norm counts
genelist <- read.csv("GeneID.csv", stringsAsFactors = FALSE)
NormCounts <- read.csv("htseq.normCounts.Jan2017.csv", stringsAsFactors = FALSE)

#check both files a little bit
head(genelist)
head(NormCounts)

#check how many genes from gene list can be found from norm count data table
overlap <- intersect(genelist$GeneID, NormCounts$X)

#Add a column of keep to the norm counts, with all 0s
NormCounts$keep <- 0

#using loop adding 1 at the end of the genes identified
x <- 1 #initial row ID
y <- 1 # initial gene list ID
repeat {
  repeat {
  if (NormCounts[x,1] == overlap[y]) {  #compare Ensembl ID from counts with gene list
    NormCounts[x,"keep"] <- 1 # if matches, then change 0 to 1 for keep column
  }
  y <- y+1
  if (y == length(overlap)+1) {
    break  #compare another gene until all genes screened
  }
  }
    x <- x+1 #move to another row (gene)
    y <- 1 #go back to first gene in Overlap gene list
    if (x == nrow(NormCounts)+1) {
      break  #end the loop when reach the end of the count table
    }
}

# generate new data table with only target genes and delete keep column
keep <- NormCounts$keep == 1
NewCounts <- NormCounts[keep,]
NewCounts$keep <- NULL

#Convert Ensembl IDs to gene Symbols & delete Ensembl IDs
NewCounts$Symbol <- mapIds(org.Mm.eg.db, NewCounts$X,
                           keytype="ENSEMBL", column="SYMBOL")
NewCounts <- NewCounts[,c(ncol(NewCounts), 1:(ncol(NewCounts)-1))]
NewCounts$X <- NULL

#order the genes based on Symbol alphabetical order
NewCounts <- NewCounts[order(NewCounts$Symbol),]

#generate output file
write.csv(NewCounts, file="NewCounts.Jan2017.csv", row.names = FALSE)



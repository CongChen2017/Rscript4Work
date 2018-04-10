# the purpose of this script is to combine two gene list and pick unique ones to form a new gene list
# after that, use the new gene list extract data from a huge data set to run analysis.

# clean enviroment
rm(list=ls())

# read in file#1 (CtrlvsSPChomo in this case)
homogenes <- read.csv("testho.csv", stringsAsFactors = FALSE)
# read in file#2 (CtrlvsSPChetz in this case)
hetzgenes <- read.csv("test.csv", stringsAsFactors = FALSE)

# get gene lists from two files (first column X)
homogenelist <- homogenes$X
hetzgenelist <- hetzgenes$X

# append two lists into one, then use unique() function to get unique gene Ensmbl IDs
genelist <- append(homogenelist,hetzgenelist)
genelist <- unique(genelist)

# read in data file we will work with, and following steps for data reformat
rawdata <- read.csv("htseq.normCounts.Sep2016.csv", stringsAsFactors = FALSE)

# step 1: keep ATII cell data
colnames <- colnames(rawdata)
keep <- grepl("ATII", colnames)
keep[1] <- TRUE
ATIIdata <- rawdata[,keep]

# step 2: remove LysM data
colnames <- colnames(ATIIdata)
keep <- !grepl("LysM", colnames)
ATIIdata <- ATIIdata[,keep]

# step 3: keep ATII.M cell data
colnames <- colnames(ATIIdata)
keep <- grepl(".M.0", colnames)
keep[1] <- TRUE
seqdata <- ATIIdata[,keep]

# step 4: Format the data
#countdata <- seqdata[,-1] #get rid of first column (gene IDs)
#rownames(countdata) <- seqdata[,1] #use gene IDs as row names
#head(countdata)

# compare genelist with gene info from working file
overlap <- intersect(genelist, seqdata$X)

# filter working file and only keep overlapped genes
keep <- seqdata$X %in% overlap
NewCounts <- seqdata[keep,]

# write a new file for future analysis (need to convert to Excel before Heatmap/clustering)
write.csv(NewCounts, file="New.trimmed.ATII.M.csv", row.names = FALSE)

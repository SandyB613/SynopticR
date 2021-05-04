### ***** START OF SCRIPT FOR TIMING ***** ###

# Start clock to time procedure

ptm <- proc.time ()

# *** Part 1: set up system and read in synoptic report text file ***

#load libraries
library(psych)    # basic stats methods
library(stringr)  # string handling methods including strsplit
library(here)     #added this in May 2021, uses relative path to specify files

#if you open the Rproj instead of a script, the working directory will be the
# directory where Rproj is located

#this is original method using setwed, changed in May 2021
#set working directory and check it
#this was old line for windows
#this is new line for mac
#setwd("/Users/sandy/Documents/R/SynText")
#setwd("Z:/Documents/R/SynText")


#read in synoptic report text and check first few lines
#this is old code before using here package
#syn <- readLines("pnsyn-old.txt")
#syn <- readLines("pnsyn.txt")

#using here you give the folder(s) relative to the folder in which the Rproj
# file is located which here package identifies for you
syn <- readLines(here("data", "pnsyn.txt"))
head(syn)

# *** Part 2: extract standard case information and build basic data frame ***

#Use grep to collect the accession numbers and demographic information
#  return all lines that contain strings in the form "Snn-n" where n is single number
#  this are the lines that contain accession numbers
#  not case specific (ignore.case = TRUE)
#  returns value of the line (value = TRUE)
#  put the list of lines containing the accession numbers in ANlines
#Note - another approach is grepl (grep-logical) which returns TRUE/FALSE
#  depending if string of interest is found

ANlines <- grep("s([0-9]{2})-([0-9])", syn, ignore.case = TRUE, value = TRUE)

#now split up the continuous lines of text based on presence of a blank space
# using strsplit(ANlines, " ")
#apply that string split to the entire ANlines variable using sapply
#put only the first element in variable AN using this: "[, 1)
#AN is now a list of all the accession numbers in the file

AN <- sapply(strsplit(ANlines, " "), "[", 1)

#now get the accession date which is in the same line as the accession number
#this returns a list of all accession dates - fourth element returned by strsplit
#note that these are not really recognized as dates, just character strings
ADate <- as.Date(sapply(strsplit(ANlines, " "), "[", 4))


#now go for gender and age
#need to do this in two steps:
#  first grep includes search element lines with "gender" string
#  second grep just pulls out data lines and excludes search criteria lines
#    because only those lines also have "Age:" string
#use sapply to create list of all genders and put in Gender (all male in this case)
#use sapply to create list of all ages and put in Age
#gender and age are 2nd and 4th data elements in the split strings of text
#want these to be factors and numeric class types respectively,
#  otherwise would be character

AllGendLines <- grep("gender", syn, ignore.case = TRUE, value = TRUE)
GendAgeLines <- grep("Age:", AllGendLines, ignore.case = FALSE, value = TRUE)
Gender <- factor(sapply(strsplit(GendAgeLines, " "), "[", 2))
Age <- as.numeric (sapply(strsplit(GendAgeLines, " "), "[", 4))

#get signout date (as a string, not true date)
#need two greps, first includes selection criteria lines
#second grep excludes them with invert=TRUE
AllSDateLines <- grep("Sign-Out Date:", syn, ignore.case = FALSE, value = TRUE)
SDateLines <- grep(" To ", AllSDateLines, ignore.case = FALSE, value = TRUE, invert = TRUE)
SDate <- as.Date(sapply(strsplit(SDateLines, " "), "[", 3))

#get pathologist name as factor, need to join three separate strings
# due to lack of consistency in pathologist name syntax
PathLines <- grep("Primary Pathologist:", syn, ignore.case = FALSE, value = TRUE)
Path3 <- sapply(strsplit(PathLines, " "), "[", 3)
Path4 <- sapply(strsplit(PathLines, " "), "[", 4)
Path5 <- sapply(strsplit(PathLines, " "), "[", 5)
Path <- factor(paste(Path3, Path4, Path5, sep=" "))

#now create a dataframe from these lists containing basic case information
SynGen <- data.frame(AN, ADate, SDate, Gender, Age, Path, stringsAsFactors=FALSE)
head(SynGen)

#now insert line numbers for accession numbers from the original test file into the
# data frame - this will simplify the loops to load the other data
# as we can just search between the accession number lines
k <- 1 #k will be counter for the dataframe
for (i in 1:length(syn)) {  #i will be counter for the text file
	#looping down entire text file
	if (grepl("S([0-9]{2})-([0-9])", syn[i])) {
		SynGen[k, "ANline"] <- i
		k <- k+1 #move to next row of data frame
		}
		i <- i+1 #move down a line in text file
	}


# *** Part 3: extract synoptic report data elements and load into data frame ***
#
# The available data may vary by tumour type, synoptic report version and use of optional report elements

####### remove this section when done development ##########
#just testing R code here, need to remove this section
#to add new columns to dataframe these are some options

#This would create a new column called "hist" and set the value of
# ALL rows in that column to be "adenocarcinoma"
#SynGen$hist <- "adenocarcinoma"

#This would create a new column called "Hist" and set the value of
# the first row to be "adenocarcinoma" while the rest of the rows
# remain as "<NA>"
#SynGen[1,"Hist"] <- "adenocarcinoma"

#can do the same thing using a variable for column name
#x <- "Hist"
#SynGen[1, x] <- "adenocarcinoma"

#This changes the value of the second row from <NA> to "adenocarcinoma"
#SynGen[2,"Hist"] <- "adenocarcinoma"
###### end of section to be removed ##############

#The definitions of the report elements for the particular synoptic report are contained
# in a simple text file called "SynData.txt"
#
#this is info needed to parse in synoptic report data
#  varNM is variable name
#  pat is string pattern to use to reconize line with data in the larger synoptic report text file
#  datPos is data position: number of text element that contains desired data
#  datClass is data class (not sure if this is needed really)
#  as.is = TRUE prevents conversion of character data to factors

#read the data definition text file

#this is old method before using here package
#SynData <- read.table("SynData.txt", header = T, sep = "$", as.is = TRUE)

#using here package
SynData <- read.table(here("data", "SynData.txt"), header = T, sep = "$", as.is = TRUE)
head(SynData)

#start in the main data framge at row 1
maindf.row <- 1 #maindf.row is row counter for main data frame

#this starts the outer loop through the main dataframe
for (maindf.row in 1:NROW(SynGen)) {	#go through the entire main dataframe row by row
	#specify what lines in text file to search based on acc number lines
	#start one line below each line that contains an accession number so add 1
	#end one line above each line that contains an accession number so subtract one
	startline <- SynGen$ANline[maindf.row]+1
	endline <- ifelse(maindf.row < NROW(SynGen), SynGen$ANline[maindf.row+1]-1, length(syn))

	#start at the first row of the variable dataframe and go through it all
	vardf.row <- 1  #vardf.row is row counter for variable data frame

	#this starts the inner loop through the variable/data dataframe
	while(vardf.row <= NROW(SynData)) {
		#creating varible names to simplify code
		var <- SynData[vardf.row, 1]
		pat <- SynData[vardf.row, 2]
		pos <- SynData[vardf.row, 3]
		varClass <- SynData[vardf.row, 4]

		#find the text lines that contain each variables string pattern and store in SynLine
		SynLine <- grep(pat, syn[startline:endline], ignore.case = TRUE, value = TRUE)
		#split the text line up into individual strings that are separated by spaces
		# and store those lists of words in splitLine
		splitLine <- strsplit(SynLine, " ")
		#there are two possibilities
		#   1. the string pattern is not found in which case the length of splitLine
		#	will be zero and the value of "NA" is assigned to that piece of data
		#   2. the string pattern is found in which case the length of splitLine is not
		#      zero and the list element at the appropriate position is put in the main
		#      dataframe
		SynGen[maindf.row, var] <- ifelse(length(splitLine)==0, NA, splitLine[[1]][pos])
		#move to the next row in the variable/data dataframe
		vardf.row <- vardf.row+1
	}	#this ends the inner loop through the variable/data dataframe

	#move to the next row in the main dataframe
	maindf.row <- maindf.row+1
}	#this ends the outer loop through the main dataframe


#Now make sure all data classes are correct
# First all the basic case data (same for all synoptic templates)
SynGen$AN <- factor(SynGen$AN)
SynGen$ADate <- as.Date(SynGen$ADate)
SynGen$SDate <- as.Date(SynGen$SDate)
SynGen$Gender <- factor(SynGen$Gender)
SynGen$Age <- as.numeric(SynGen$Age)
SynGen$Path <- factor(SynGen$Path)

#Now change the classes of the columns in the synoptic data dataframe
# based on the data class value in the variable dataframe
#start at the first row of the variable dataframe and go through it all
#note that we are running through the ROWS of the data df but the
# COLUMNS of the synoptic df
#there are 7 elements that we've already fixed the classes for so we
#are starting at column 8 (counter + 7) for the synoptic data elements

	vardf.row <- 1  #vardf.row is row counter for variable data frame

	#this starts the inner loop through the variable/data dataframe
	while(vardf.row <= NROW(SynData)) {
	if (SynData[vardf.row,4] == "fac") SynGen[,vardf.row+7] <- factor(SynGen[,vardf.row+7])
	if (SynData[vardf.row,4] == "num") SynGen[,vardf.row+7] <- as.numeric(SynGen[,vardf.row+7])

	#move to the next row in the variable/data dataframe
	vardf.row <- vardf.row+1
}	#this ends the inner loop through the variable/data dataframe

proc.time() - ptm

### ***** END OF SCRIPT FOR TIMING ***** ###


#create csv file from dataframe for use in Excel etc.
#this is old version before using here
#write.csv(pros.data, file = "pros.txt")
#this is new version using here
write.csv(pros.data, here("output", "pros.txt"))

#show structure of SynGen
str(SynGen)

#look at summary statistics of SynGen
summary(SynGen)

Path.ggs.table <- with(SynGen, table(Path, ggs))
prop.table(Path.ggs.table, 1)

Path.gg1.table <- with(SynGen, table(Path, gg1))
chisq.test(Path.gg1.table)
prop.table(Path.gg1.table, 1)

Path.gg2.table <- with(SynGen, table(Path, gg2))
chisq.test(Path.gg2.table)
prop.table(Path.gg2.table, 1)


edit(SynGen)

#the table function will return counts
Path.PNI.table <- with(SynGen, table(Path, pni))
#the prop function will return proportions
#  and needs to operate on a table
#  the 1 parameter in prop means calculate proportions on rows, not columns
prop.table(Path.PNI.table, 1)

Path.EPE.table <- with(SynGen, table(Path, epe))
prop.table(Path.EPE.table, 1)

chisq.test(Path.EPE.table)

# this uses Pearson's Chi-squared test of independence to test if EPE rate
#  is independent of pathologist
# this gives a p of .43 so the EPE result is independent of pathologist
# the test throws a warning due to small values of some table entries
# combine the 3 lower EPE rates and compare to the one higher rate

tbl <- Path.EPE.table
ab <- "Alexander H. Boag,"
cd <- "Christopher Davidson, M.D."
db <- "David M. Berman,"
mm <- "Marosh Manduch, M.D."
ctbl = rbind(tbl[cd,], tbl[ab,] + tbl[db,] + tbl[mm,])

chisq.test(ctbl)
#this now gives a p of .17 so EPE is still independent of pathologist based
# on grouping of all pathologists except CD vs CD alone
# no longer throws an error

#now compare AB vs CD alone
xtbl = rbind(tbl[cd,], tbl[ab,])
chisq.test(xtbl)
head(SynGen)
prop.table(Path.EPE.table, 1)
describe(SynGen$ggs)
describeBy(SynGen$gg2, SynGen$epe)
describeBy(SynGen$posCores, SynGen$epe)
describeBy(SynGen$ggs, SynGen$Path)
summary(Path.EPE.table)

plot(SynGen$proCa ~ SynGen$ggs)
plot(SynGen$proCa ~ SynGen$gg1)
cor(SynGen$proCa, SynGen$ggs)

#bringing in short dataframe for R paper

psyn.df <- read.csv(file="pshort.csv")
head(psyn.df)
edit(psyn.df)

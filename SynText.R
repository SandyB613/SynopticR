#load libraries
library(psych)    # basic stats methods
library(stringr)  # string handling methods lime strsplit

#set working directory and check it
setwd("Z:/Documents/R/SynText")
getwd()

#should look at replacing the setwd with here package

#read in prostate needle synoptic report text and check first few lines
syn <- readLines("pnsyn.txt")
head(syn)

#Use grep as follows
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
ADate <- sapply(strsplit(ANlines, " "), "[", 4)


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

#putting in name just to check consistency
NameLines <- grep("Patient:", syn, ignore.case = FALSE, value = TRUE)
Name <- sapply(strsplit(NameLines, " "), "[", 2)

#get signout date
#need two greps, first includes selection criteria lines
#second grep excludes them with invert=TRUE 
AllSDateLines <- grep("Sign-Out Date:", syn, ignore.case = FALSE, value = TRUE)
SDateLines <- grep(" To ", AllSDateLines, ignore.case = FALSE, value = TRUE, invert = TRUE)
SDate <- sapply(strsplit(SDateLines, " "), "[", 3)

#get histologic type
HistLines <- grep("Histologic Type:", syn, ignore.case = FALSE, value = TRUE)
Hist <- sapply(strsplit(HistLines, " "), "[", 3)

#get first Gleason grade pattern (need \\ escape sequence for bracket)
GG1Lines <- grep("Primary \\(Predom", syn, ignore.case = FALSE, value = TRUE)
GG1 <- as.numeric(sapply(strsplit(GG1Lines, " "), "[", 5))

#get second Gleason grade pattern
GG2Lines <- grep("Secondary", syn, ignore.case = FALSE, value = TRUE)
GG2 <- as.numeric(sapply(strsplit(GG2Lines, " "), "[", 6))

#get total Gleason score
GScoreLines <- grep("Total Gleason Score:", syn, ignore.case = FALSE, value = TRUE)
GScore <- as.numeric(sapply(strsplit(GScoreLines, " "), "[", 4))

#get number of positive cores
PCoresLines <- grep("Number of Cores Positive:", syn, ignore.case = FALSE, value = TRUE)
PCores <- as.numeric(sapply(strsplit(PCoresLines, " "), "[", 5))

#get per cent of tissue involved by cancer
ProCaLines <- grep("Proportion", syn, ignore.case = FALSE, value = TRUE)
ProCa <- as.numeric(sapply(strsplit(ProCaLines, " "), "[", 9))

#get total number of cores
TCoresLines <- grep("Total Number of Cores", syn, ignore.case = FALSE, value = TRUE)
TCores <- as.numeric(sapply(strsplit(TCoresLines, " "), "[", 5))

#get periprostatic fat invasion/EPE status as a factor
EPELines <- grep("Periprostatic", syn, ignore.case = FALSE, value = TRUE)
EPE <- factor(sapply(strsplit(EPELines, " "), "[", 4))

#get seminal vesicle invasion status as a factor
SVILines <- grep("Seminal", syn, ignore.case = FALSE, value = TRUE)
SVI <- factor(sapply(strsplit(SVILines, " "), "[", 4))

#get lymph-vascular invasion status as a factor
LVILines <- grep("Lymph", syn, ignore.case = FALSE, value = TRUE)
LVI <- factor(sapply(strsplit(LVILines, " "), "[", 3))

#get perineural invasion status as a factor
#note - this value is missing from some reports (6 cases) for an unknown reason
# which causes a problem as results don't all line up with this method
PNILines <- grep("Perineural", syn, ignore.case = FALSE, value = TRUE)
PNI <- factor(sapply(strsplit(PNILines, " "), "[", 3))
length(PNI)

#get pathologist name as factor, need to join three separate strings
# due to lack of consistency in pathologist name syntax
PathLines <- grep("Primary Pathologist:", syn, ignore.case = FALSE, value = TRUE)
Path3 <- sapply(strsplit(PathLines, " "), "[", 3)
Path4 <- sapply(strsplit(PathLines, " "), "[", 4)
Path5 <- sapply(strsplit(PathLines, " "), "[", 5)
Path <- factor(paste(Path3, Path4, Path5, sep=" "))

#now create a preliminary dataframe from these lists to make sure it all works
syn.data <- data.frame(AN, ADate, Gender, Age, Name, SDate, Hist, GG1, GG2, GScore, ProCa, PCores, TCores, PNI, EPE, SVI, LVI, Path)
head(syn.data)

#now create an anomymous dataframe
pros.data <- data.frame(AN, ADate, Gender, Age, SDate, Hist, GG1, GG2, GScore, ProCa, PCores, TCores, PNI, EPE, SVI, LVI, Path)
head(pros.data)

#create csv file from dataframe for use in Excel etc.
write.csv(pros.data, file = "pros.txt")

str(syn.data)
summary(syn.data)

#the table function will return counts
Path.PNI.table <- with(syn.data, table(Path, PNI))
#the prop function will return proportions
#  and needs to operate on a table
#  the 1 parameter in prop means calculate proportions on rows, not columns
prop.table(Path.PNI.table, 1)

Path.EPE.table <- with(syn.data, table(Path, EPE))

chisq.test(Path.EPE.table) 

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

prop.table(Path.EPE.table, 1)
describe(syn.data$GScore)
describeBy(syn.data$GG2, EPE)
describeBy(syn.data$PCores, EPE)
describeBy(syn.data$GScore, Path)
summary(Path.EPE.table)

plot(syn.data$GScore ~ syn.data$ProCa)
plot(syn.data$GG1 ~ syn.data$ProCa)
cor(syn.data$ProCa, syn.data$GScore)

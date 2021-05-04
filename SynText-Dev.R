#not sure if I need psych library but anyway...

library(psych)
setwd("Z:/Documents/R/SynText")
getwd()

#read in prostate needle synoptic report text
syn <- readLines("pnsyn.txt")
class(syn)
#check out the first few lines
head(syn)

#check how many lines in syn
length(syn)

#this will show content of 11th row
syn[11]
syn[10] #This is content of 10th row

library(stringr)

#display a sub string from 9th to 13th character of 11th row
str_sub(syn[11],9,13)

#split the text from the 11th row into separate elements based
# on separating blank space
strsplit(syn[11], " ")

#split 11th row again and put into variable x then display 4th element of the list
x <- strsplit(syn[11], " ")
x[[1]][4]
#now display 1st element of the list in x
x[[1]][1]
class(x[[1]][1])
#this returns character so is as expected

#now try grepl function to test for presence of a string in a value
grepl("gender", x[[1]][1])
#this returns FALSE, try again with proper case

grepl("Gender", x[[1]][1])
#this returns TRUE

#now try setting argument to ignore case, this returns TRUE
grepl("gender", x[[1]][1], ignore.case = TRUE)

#now try using variables, this works and returns TRUE
a <- x[[1]][1]
b <- "gender"
grepl(b,a, ignore.case = TRUE)

#now turning attention to the 10th row to look at detecting new records
x <- strsplit(syn[10], " ")
AccNo <- x[[1]][1]
#This returns "S11-17760"

#Want to confirm the returned string is of the form "Snn-n" where n is single number
grepl("s([0-9]{2})-([0-9])", AccNo, ignore.case = TRUE)
#this returns TRUE and seems to work when other character strings are substituted

#try using grep
grep("s([0-9]{2})-([0-9])", AccNo, ignore.case = TRUE, value = TRUE)
grep("s([0-9]{2})-([0-9])", syn, ignore.case = TRUE, value = TRUE)
aclines <- grep("s([0-9]{2})-([0-9])", syn, ignore.case = TRUE, value = TRUE)
strsplit(aclines, " ")

#this returns a list of all accession numbers - first element returned by strsplit
ANlist <- sapply(strsplit(aclines, " "), "[", 1) 

#this returns a list of all accession dates - fourth element returned by strsplit
ADateList <- sapply(strsplit(aclines, " "), "[", 4)

#now go for gender and age
#need to do this in two steps:
#  first grep includes search element lines with gender
#  second grep just pulls out data lines and excludes search criteria
AllGender <- grep("gender", syn, ignore.case = TRUE, value = TRUE)
GenderAge <- grep("Age:", AllGender, ignore.case = FALSE, value = TRUE)
GendList <- sapply(strsplit(GenderAge, " "), "[", 2) 
AgeList <- sapply(strsplit(GenderAge, " "), "[", 4)

#putting in name just to check consistency
Names <- grep("Patient:", syn, ignore.case = FALSE, value = TRUE)
Nmlist <- sapply(strsplit(Names, " "), "[", 2)

#now create a preliminary dataframe from these lists
syn.data <- data.frame(ANlist, ADateList, GendList,AgeList, Nmlist)
head(syn.data)

#this next phase will be for a more generalized approach 
#will read in search items etc from a text file and go from there
#read in table containing info to parse in synoptic report data
#  varNM is variable name
#  pat is pattern to use to reconize line with data
#  datPos is data position: number of text element that contains desired data
#  datClass is data class (not sure if this is needed really)
#  as.is = TRUE prevents conversion of character data to factors
help(read.table)
SynData <- read.table("SynData.txt", header = T, sep = "$", as.is = TRUE)
xx <- SynData[2,1]
class(xx)
xx
NROW(SynData)
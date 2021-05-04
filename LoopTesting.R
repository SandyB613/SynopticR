#This works as expected
k <- 1
for (i in 1:100) {
	cat("i=", i, "k=", k, fill = TRUE)
	#cat creates a character string from elements and prints to terminal
	#fill = TRUE creates line break
	k <- k + 1
	}
	
#similar using while, works fine
k <- 1
while (k <= 100) {
	cat("k=", k, fill = TRUE)
	k <- k + 1
	}

#again using repeat, if and break
k <- 1
repeat {
	cat("k=", k, fill = TRUE)
	k <- k + 1
	if(k > 100) break()
	}

#again using for and if-else
for (i in 1:100) 
{
	if (i < 51) {
	cat("i is less than 50, i=", i, fill = TRUE)
	} else
	cat("i is greater than 50, i=", i, fill = TRUE)
}

#using elseif function
#if the first arguement is true then the second argument is return otherwise
# the third argument is returned
# e.g. y <- ifelse(j<5, 1, 0) would return 1 when j<5 and 0 when j not < 5

x <- c("brown", "black", "red-yellow", "maroon", "mauve")
n <- length(x)
for (i in 1:n) {
	w <- ifelse(grepl("b", x[i]), "The word starts with b", "The word does not start with b")
	cat(x[i], ".", w, fill = TRUE)
	}



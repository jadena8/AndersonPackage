#This function is to take a multiple sequence alignment (MSAs) and determine if they are the same size. 
#The sequences need to be the same size in order to generate a square matrix for calculation.
#
#Input: msa <- a multiple sequence alignment read in from a file
#Output: A PASS or FAIL to see if the sequences passed or failed the unit test
#           if failed: the code will also print out the sequence number that failed the unit test
#
#same_size Function: Check to see if all sequences in the file are the same length
#

#Function that checks to see if the length of each sequence is the same
same_size <- function(msa){
check <- TRUE
counter <- 1
  #Loop that checks each sequences from position 2 on
  #Position 2 is chosen because I check the sequence at i with the sequence at i-1 (so position 2 with position 1)
  for(i in 2:length(msa)) {
    if(nchar(msa[i-1]) == nchar(msa[i])) {
      check <- TRUE
      counter <- counter + 1
    }
    else {
      check <- FALSE
      cat(counter)
      #If at any point our sequences aren't the same size, this breaks the loop and keeps check = FALSE
      break
    }
  }
#If our checking variable is true, then our sequences are all the same size, if not then they are not
if (check == TRUE){
  cat("PASS")
}
else{
  cat("FAIL")
}
return(check)
} 




#This function takes in a msa to check whether or not the sequences have large gaps.
#If the sequences do have large gaps, then they will fail the unit test
#
#Input: msa <- a multiple sequence alignment read in from a file or pasted, MUST NOT BE IN TRADITIONAL FORMATS, sequences only
#***DAN: It would not hurt to have a more precise description of the format of msa
#Output: A PASS or FAIL to see if the sequences passed or failed the unit test
#           if failed: the code will also print out the sequence number that failed the unit test
#
#is_protein function: counts the number of gaps in the msa vector, if greater than 10 for any sequence fails unit test
#

is_protein <- function(msa) {
#This function counts how many gaps are in a sequence and checks if they're at a tolerable level
  
  #Counter tells you which sequence you're at, if the code breaks, count will print to the screen the location of the sequence in the vector that is too gappy
  counter <- 1
  #The PASS/FAIL variable
  checker <- TRUE
  #Counts the number of gaps in a sequence
  gap_counter <- 0
  
  #Loop through each sequence in the alignment
  for(i in 1:length(msa)){
    #split the sequence into a new vector to count number of gaps
    split_sequence <- strsplit(msa[i], "")[[1]]
    
    #Loop over that new sequence vector
    for(j in 1:nchar(msa[i])){
      
      #Count the number of gaps in the new sequence
      if(split_sequence[[j]] == '-') {
        gap_counter <- gap_counter + 1
      }
      
      #If the sequence fails to gap threshold, then break out of the loops and print the position of the sequence in the vector that is too gappy
      if(gap_counter > 10){
        checker <- FALSE
        cat(counter, "\n")
        break
      }
      if(checker == FALSE){
        break
      }
    }
    
    #Increment counter and reset gap counter
    counter <- counter + 1
    gap_counter <- 0
  }

#Prints the pass or fail message to the console
if(checker == TRUE){
  cat("PASS")
}
else{
  cat("FAIL")
}
}



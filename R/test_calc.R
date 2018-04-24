#This function takes in a msa to check whether or not the sequences are in a FASTA format.
#If the sequences are in FASTA format, they will pass, if not they will fail the unit test
#
#Input: msa <- a multiple sequence alignment read in from a file
#Output: A PASS or FAIL to see if the sequences passed or failed the unit test
#           if failed: the code will also print out the sequence number that failed the unit test
#
#test_calc function: check to see if the sequences are in FASTA format since I customized the alignment. Will check for > on the first line and for sequences on the next.
#

#File path that is to be opened and checked to see if in FASTA format
test_calc_format <- function(f_to_open){
  openFile <- file(f_to_open, open = "r")
  #Counter variables to see if the number of sequences = the number of "titles"
  count_carrot <- 0
  count_seq <- 0
  
  #Loop through the open file, multiple openFile by 2 to account for both sequences and titles
  for(i in 1:(2*openFile)){
    #Read in line and split the string to check certain positions
    line <- readLines(con = openFile, n = 1)
    line <- strsplit(line, "")[[1]]
    
    #Check to see if we're on a title line
    if(line[1] == ">") {
      count_carrot <- count_carrot + 1
    }
    #If not, we're on a sequence line
    else{
      count_seq <- count_seq + 1
    }
    if(i == 2*openFile){
      break
    }
  }
  #closes the file
  close(openFile)
  
  #If the number of sequences is equal to the number of titles, we're in FASTA format
  if(count_seq == count_carrot){
    print("PASS")
  }#If not, we're not in FASTA format
  if(count_seq != count_carrot){
    print("FAIL")
  }
}

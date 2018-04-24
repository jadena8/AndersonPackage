#Identifying strand locations with topology files to grab strands from beta barrels
#A topology file contains a string of letters that denotes 1 of 4 specific things about a residue
#I=inner side of the membrane, O=outter side of the membrane, p=pore facing, L=lipid facing
#an alternating patter of LpLp or pLpL...etc. denotes a beta strand

strand_ident <- function(topo_file) {
  #Open the topology file to be for strands segmentation
  openFile <- file(topo_file, open = "r")

  #initialize specific counter variables and a vector to store strands
  count_strand <- 0
  store_strand_place <- 1
  strands <- 0

  #begin looping through lines in the file
  for(i in openFile) {
    #Read in line i, split that into a character list, and unlist it into a character vector
    line <- readLines(con = openFile)
    line <- strsplit(line,"")
    line <- unlist(line)

    #for each item in that character vector
    for(j in 1:length(line)){

      #If pos j is in a strand
      if(line[j] == "p" | line[j] == "L"){
        #and we haven't started counting a strand
        if(count_strand == 0){
          #put the starting position j into the first position of the strands vector
          strands[store_strand_place] <- j - 1
          #increment the strands vectors counter
          store_strand_place <- store_strand_place + 1
        }
        #and then increment the strand counter
        count_strand <- count_strand + 1
      }

      #We are no longer in a strand if we start with I or O
      if(line[j] == "I" | line[j] == "O") {
        #If I have counted some strand
        if(count_strand != 0){
          #store the end position of that strand and reset the strand counter
          strands[store_strand_place] <- j - 1
          store_strand_place <- store_strand_place + 1
          count_strand <- 0
        }
      }
    }
  }
}


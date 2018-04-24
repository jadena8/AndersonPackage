#Identifying strand locations with topology files to grab strands from beta barrels
#A topology file contains a string of letters that denotes 1 of 4 specific things about a residue
#I=inner side of the membrane, O=outter side of the membrane, p=pore facing, L=lipid facing
#an alternating patter of LpLp or pLpL...etc. denotes a beta strand


topo_file <- "query.top"
openFile <- file(topo_file, open = "r")

test <- c("OOOOLpLpLpLpIIII", "OOOOOOOIIIIII", "OOLpLpLIIIpLpLpLOOO")

count_strand <- 0
store_strand_place <- 1
strands <- 0

for(i in openFile) {
  line <- readLines(con = openFile)
  line <- strsplit(line,"")
  line <- unlist(line)

  for(j in 1:length(line)){

    if(line[j] == "p" | line[j] == "L"){

      if(count_strand == 0){
        strands[store_strand_place] <- j - 1
        store_strand_place <- store_strand_place + 1
      }

      count_strand <- count_strand + 1
    }

    if(line[j] == "I" | line[j] == "O") {

      if(count_strand != 0){

        strands[store_strand_place] <- j - 1
        store_strand_place <- store_strand_place + 1
        count_strand <- 0
      }
    }
  }
}








#for(i in openFile){
 # line <- readLines(con = openFile)
  #line <- strsplit(line, "")
  #line <- unlist(line)
  #for(j in 1:length(line)){
    #if(line[j] == "p" | line[j] == "L"){

     # if(count_strand == 0){
      #  strands[store_strand_place] <- j
       # count_strand <- count_strand + 1

    #  }
    #}

  #  if(line[j] == "I" | line[j] == "O") {

  #  }

 # }
#}

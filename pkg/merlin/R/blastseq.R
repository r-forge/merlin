blastseq <-
function(sq,nb=10,database="nr",program="blastn",oot=60,waittime=5,verbose=FALSE){
  if(class(sq)!="DNAbin"){
    stop("Input sequences must be a DNAbin class")
  }
  numseq <- length(sq)
  hitdf <- data.frame(seqname=character(),gi=integer(),evalue=numeric(),score=integer())
  # main loop
  for(i in 1:numseq){
    seqname <- names(sq)[i]
    tm <- 0
    if(verbose){
      print(paste("Processing ",seqname,": ",i," of ",numseq,sep=""))
    }
    seqstr <- lapply(as.character(myseq[i]), paste, collapse = "")
    blastinquery <- paste("http://blast.ncbi.nlm.nih.gov/Blast.cgi?QUERY=", seqstr, "&DATABASE=", database, "&HITLIST_SIZE=", nb, "&FILTER=L&EXPECT=10&PROGRAM=", program, "&CLIENT=web&SERVICE=plain&NCBI_GI=on&PAGE=Nucleotides&CMD=Put", sep = "")
    blastin <- scan(blastinquery, what = "raw")
    RID <- blastin[rev(grep("RID", blastin))[1] + 2]
    if(verbose){
      print(paste("Received RID",RID))
    }
    blastoutquery <- paste("http://blast.ncbi.nlm.nih.gov/Blast.cgi?RID=", RID, "&SHOW_OVERVIEW=no&FORMAT_TYPE=XML&ALIGNMENTS=0&NCBI_GI=yes&CMD=Get",sep = "")
    blastout <- scan(blastoutquery, what = "", sep = "\n", quiet = TRUE)
    while (blastout[1] != "<?xml version=\"1.0\"?>" & tm <= oot) {
      Sys.sleep(waittime)
      blastout <- scan(blastoutquery, what = "", sep = "\n", quiet = TRUE)
      tm <- tm + waittime
    }
    if(blastout[1]!="<?xml version=\"1.0\"?>"){
      print(paste("Failed to obtain BLAST output"))
      blastout <- NA
      hitdf <- rbind(hitdf,data.frame(seqname=seqname,gi=NA,evalue=NA,score=NA))
    }
    else{
      print(paste("Received BLAST output for RID",RID))
      # Process XML
      myxmlstr <- paste(blastout,collapse="\n")
      myxml <- xmlTreeParse(myxmlstr,asText=TRUE)
      xmltop <- xmlRoot(myxml)
      #def <- unlist(lapply(getNodeSet(xmltop,"//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_def"),xmlValue))
      id <- unlist(lapply(getNodeSet(xmltop,"//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_id"),xmlValue))
      numid <- length(id)
      evalue <- as.double(unlist(lapply(getNodeSet(xmltop,"//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_hsps/Hsp/Hsp_evalue"),xmlValue)))
      score <- as.integer(unlist(lapply(getNodeSet(xmltop,"//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_hsps/Hsp/Hsp_score"),xmlValue)))
      gi <- as.integer(unlist(lapply(strsplit(id,"|",fixed=TRUE),"[[",2)))
      hitdf <- rbind(hitdf,data.frame(seqname=rep(seqname,numid),gi=gi,evalue=evalue,score=score))
    }
  }
  hitdf
}

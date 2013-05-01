read.ncbifasta <-
function(fn,type="fasta"){
  myseq <- read.dna(fn,format="fasta",as.matrix=FALSE)
  seqnames <- names(myseq)
  x <- scan(what=list(character(),integer(),character(),character(),character()),sep="|",quote="\"",text=paste(seqnames,collapse="\n"),quiet=TRUE)
  gi <- x[[2]]
  accversion <- x[[4]]
  accession <- unlist(lapply(strsplit(x[[4]],".",fixed=TRUE),"[[",1))
  #print(paste(x[[4]],accession))
  description <- x[[5]]
  if(type=="table"){
    return(data.frame(gi,accession,accversion,description))
  }
  else if(type=="fasta"){
    newseq <- myseq
    names(newseq) <- accession
    return(newseq)
  }
  else{
    stop("Unrecognized option")
  }
}

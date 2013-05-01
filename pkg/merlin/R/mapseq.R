mapseq <-
function(ch=NULL,ref=NULL,sm=nucleotideSubstitutionMatrix(match = 1, mismatch = 0, baseOnly = FALSE, type="DNA"),gapOpening=0,gapExtension=-1,aslist=FALSE){
  #patt <- gsub("-","",c2s(toupper(ch)),fixed=TRUE)
  patt <- DNAStringSet(gsub("-","",lapply(lapply(myseqs,toupper),c2s),fixed=TRUE))
  subj <- DNAString(gsub("-","",c2s(toupper(ref)),fixed=TRUE))
  al <- pairwiseAlignment(pattern=patt,subject=subj,type="local",substitutionMatrix=sm,gapOpening=gapOpening,gapExtension=gapExtension)
  if(aslist){
    al2 <- lapply(as.character(al),s2c)
    al3 <- lapply(al2,as.SeqFastadna)
    numseq <- length(al3)
    for(i in 1:numseq){
      attr(al3[[i]],"name") <- attr(ch[[i]],"name")
      attr(al3[[i]],"Annot") <- attr(ch[[i]],"Annot")
    }
    return(al3)
  }
  else{
    return(al)
  }
}

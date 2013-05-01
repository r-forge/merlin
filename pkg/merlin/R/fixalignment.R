fixalignment <-
function(s,f){
  numseq <- length(s)
  newseq <- s
  for(i in 1:numseq){
    thisseq <- s[[i]]
    if(f[i]==0){
      newthisseq <- thisseq
    }
    if(f[i]==1){
      newthisseq <- as.SeqFastadna(c("-","-",thisseq),name=attr(thisseq,"name"),Annot=attr(thisseq,"Annot"))
    }
    if(f[i]==2){
      newthisseq <- as.SeqFastadna(c("-",thisseq),name=attr(thisseq,"name"),Annot=attr(thisseq,"Annot"))
    }
    newseq[[i]] <- newthisseq
  }
  newseq
}

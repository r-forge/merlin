degapalignment <-
function(al){
  numseq <- dim(al)[[1]]
  numsites <- dim(al)[[2]]
  numgaps <- rep(0,numsites)
  for(i in 1:numsites){
    thissite <- al[,i]
    numgaps[i] <- sum(as.character(thissite)=="-")
  }
  newal <- al[,numgaps<numseq]
  newal
}

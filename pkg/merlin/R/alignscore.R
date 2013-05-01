alignscore <-
function(ch=NULL,ref=NULL,sens="F",numcode=1,type="local"){
  s <- c2s(seqinr::translate(ref,sens="F",numcode=numcode))
  # Calculate translations
  if(sens=="F"|sens=="R"){
    f0 <- c2s(seqinr::translate(ch,sens=sens,numcode=numcode))
    f2 <- c2s(seqinr::translate(c("-",ch),sens=sens,numcode=numcode))
    f1 <- c2s(seqinr::translate(c("-","-",ch),sens=sens,numcode=numcode))
  }
  else if(sens=="B"){
    f0 <- c2s(seqinr::translate(ch,sens="F",numcode=numcode))
    f2 <- c2s(seqinr::translate(c("-",ch),sens="F",numcode=numcode))
    f1 <- c2s(seqinr::translate(c("-","-",ch),sens="F",numcode=numcode))
    r0 <- c2s(seqinr::translate(ch,sens="R",numcode=numcode))
    r2 <- c2s(seqinr::translate(c("-",ch),sens="R",numcode=numcode))
    r1 <- c2s(seqinr::translate(c("-","-",ch),sens="R",numcode=numcode))
  }
  af0 <- pairwiseAlignment(pattern=f0,subject=s,type=type)
  af1 <- pairwiseAlignment(pattern=f1,subject=s,type=type)
  af2 <- pairwiseAlignment(pattern=f2,subject=s,type=type)
  if(sens=="B"){
    ar0 <- pairwiseAlignment(pattern=r0,subject=s,type=type)
    ar1 <- pairwiseAlignment(pattern=r1,subject=s,type=type)
    ar2 <- pairwiseAlignment(pattern=r2,subject=s,type=type)
    return(c(score(af0),score(af1),score(af2),score(ar0),score(ar1),score(ar2)))
  }
  else{
    return(c(score(af0),score(af1),score(af2)))
  }
}

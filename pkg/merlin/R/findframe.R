findframe <-
function(ch,ref,sens="F",numcode=1,type="local"){
  stopvec <- countstops(ch,sens,numcode)
  scorevec <- alignscore(ch,ref,sens,numcode,type)
  numframes <- length(stopvec)
  minstop <- seq(1,numframes)[stopvec==min(stopvec)]
  minscore <- seq(1,numframes)[scorevec==max(scorevec)]
  if(length(minscore)==1){
    thisstop <- stopvec[minscore]
    if(thisstop==0|thisstop==min(stopvec)){
      if(sens=="B"){
        framevec <- c(0,1,2,0,1,2)
        sensvec <- c("F","F","F","B","B","B")
      }
      else{
        framevec <- c(0,1,2)
        sensvec <- rep(sens,numframes)
      }
      return(c(as.integer(framevec[minscore]),sensvec[minscore],as.integer(stopvec[minscore]),as.double(scorevec[minscore])))
    }
    else{return(NA)}
  }
  else{
    return(NA)
  }
}

null.to.other <-
function(x,y=NA){
  if(is.null(x)){
    return(y)
  }
  else{
    return(x)
  }
}

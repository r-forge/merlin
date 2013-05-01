write.tabdelimited <-
function(x,fn,col.names=TRUE,row.names=FALSE,quote=FALSE){
  write.table(x,file=fn,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
}

gbc.to.fasta <-
function(tbl,fn){
  numseq <- dim(tbl)[[1]]
  output <- c()
  for(i in 1:numseq){
    output <- c(output,paste(">",tbl$accession[i],sep=""))
    output <- c(output,tbl$seq[i])
  }
  write(fn,paste(output,collapse="\n"))
}

\name{blastseq}
\alias{blastseq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
blastseq(sq, nb = 10, database = "nr", program = "blastn", oot = 60, waittime = 5, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sq}{
%%     ~~Describe \code{sq} here~~
}
  \item{nb}{
%%     ~~Describe \code{nb} here~~
}
  \item{database}{
%%     ~~Describe \code{database} here~~
}
  \item{program}{
%%     ~~Describe \code{program} here~~
}
  \item{oot}{
%%     ~~Describe \code{oot} here~~
}
  \item{waittime}{
%%     ~~Describe \code{waittime} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sq, nb = 10, database = "nr", program = "blastn", oot = 60, 
    waittime = 5, verbose = FALSE) 
{
    if (class(sq) != "DNAbin") {
        stop("Input sequences must be a DNAbin class")
    }
    numseq <- length(sq)
    hitdf <- data.frame(seqname = character(), gi = integer(), 
        evalue = numeric(), score = integer())
    for (i in 1:numseq) {
        seqname <- names(sq)[i]
        tm <- 0
        if (verbose) {
            print(paste("Processing ", seqname, ": ", i, " of ", 
                numseq, sep = ""))
        }
        seqstr <- lapply(as.character(myseq[i]), paste, collapse = "")
        blastinquery <- paste("http://blast.ncbi.nlm.nih.gov/Blast.cgi?QUERY=", 
            seqstr, "&DATABASE=", database, "&HITLIST_SIZE=", 
            nb, "&FILTER=L&EXPECT=10&PROGRAM=", program, "&CLIENT=web&SERVICE=plain&NCBI_GI=on&PAGE=Nucleotides&CMD=Put", 
            sep = "")
        blastin <- scan(blastinquery, what = "raw")
        RID <- blastin[rev(grep("RID", blastin))[1] + 2]
        if (verbose) {
            print(paste("Received RID", RID))
        }
        blastoutquery <- paste("http://blast.ncbi.nlm.nih.gov/Blast.cgi?RID=", 
            RID, "&SHOW_OVERVIEW=no&FORMAT_TYPE=XML&ALIGNMENTS=0&NCBI_GI=yes&CMD=Get", 
            sep = "")
        blastout <- scan(blastoutquery, what = "", sep = "\n", 
            quiet = TRUE)
        while (blastout[1] != "<?xml version=\"1.0\"?>" & tm <= 
            oot) {
            Sys.sleep(waittime)
            blastout <- scan(blastoutquery, what = "", sep = "\n", 
                quiet = TRUE)
            tm <- tm + waittime
        }
        if (blastout[1] != "<?xml version=\"1.0\"?>") {
            print(paste("Failed to obtain BLAST output"))
            blastout <- NA
            hitdf <- rbind(hitdf, data.frame(seqname = seqname, 
                gi = NA, evalue = NA, score = NA))
        }
        else {
            print(paste("Received BLAST output for RID", RID))
            myxmlstr <- paste(blastout, collapse = "\n")
            myxml <- xmlTreeParse(myxmlstr, asText = TRUE)
            xmltop <- xmlRoot(myxml)
            id <- unlist(lapply(getNodeSet(xmltop, "//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_id"), 
                xmlValue))
            numid <- length(id)
            evalue <- as.double(unlist(lapply(getNodeSet(xmltop, 
                "//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_hsps/Hsp/Hsp_evalue"), 
                xmlValue)))
            score <- as.integer(unlist(lapply(getNodeSet(xmltop, 
                "//BlastOutput/BlastOutput_iterations/Iteration/Iteration_hits/Hit/Hit_hsps/Hsp/Hsp_score"), 
                xmlValue)))
            gi <- as.integer(unlist(lapply(strsplit(id, "|", 
                fixed = TRUE), "[[", 2)))
            hitdf <- rbind(hitdf, data.frame(seqname = rep(seqname, 
                numid), gi = gi, evalue = evalue, score = score))
        }
    }
    hitdf
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

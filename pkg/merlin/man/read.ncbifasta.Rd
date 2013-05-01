\name{read.ncbifasta}
\alias{read.ncbifasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.ncbifasta(fn, type = "fasta")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{
%%     ~~Describe \code{fn} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (fn, type = "fasta") 
{
    myseq <- read.dna(fn, format = "fasta", as.matrix = FALSE)
    seqnames <- names(myseq)
    x <- scan(what = list(character(), integer(), character(), 
        character(), character()), sep = "|", quote = "\"", text = paste(seqnames, 
        collapse = "\n"), quiet = TRUE)
    gi <- x[[2]]
    accversion <- x[[4]]
    accession <- unlist(lapply(strsplit(x[[4]], ".", fixed = TRUE), 
        "[[", 1))
    description <- x[[5]]
    if (type == "table") {
        return(data.frame(gi, accession, accversion, description))
    }
    else if (type == "fasta") {
        newseq <- myseq
        names(newseq) <- accession
        return(newseq)
    }
    else {
        stop("Unrecognized option")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

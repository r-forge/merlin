\name{gbc.to.fasta}
\alias{gbc.to.fasta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gbc.to.fasta(tbl, fn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tbl}{
%%     ~~Describe \code{tbl} here~~
}
  \item{fn}{
%%     ~~Describe \code{fn} here~~
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
function (tbl, fn) 
{
    numseq <- dim(tbl)[[1]]
    output <- c()
    for (i in 1:numseq) {
        output <- c(output, paste(">", tbl$accession[i], sep = ""))
        output <- c(output, tbl$seq[i])
    }
    write(fn, paste(output, collapse = "\n"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

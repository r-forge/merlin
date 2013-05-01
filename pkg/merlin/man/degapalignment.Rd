\name{degapalignment}
\alias{degapalignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
degapalignment(al)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{al}{
%%     ~~Describe \code{al} here~~
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
function (al) 
{
    numseq <- dim(al)[[1]]
    numsites <- dim(al)[[2]]
    numgaps <- rep(0, numsites)
    for (i in 1:numsites) {
        thissite <- al[, i]
        numgaps[i] <- sum(as.character(thissite) == "-")
    }
    newal <- al[, numgaps < numseq]
    newal
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

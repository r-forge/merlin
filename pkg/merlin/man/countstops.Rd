\name{countstops}
\alias{countstops}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
countstops(ch, sens = "F", numcode = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch}{
%%     ~~Describe \code{ch} here~~
}
  \item{sens}{
%%     ~~Describe \code{sens} here~~
}
  \item{numcode}{
%%     ~~Describe \code{numcode} here~~
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
function (ch, sens = "F", numcode = 1) 
{
    if (sens == "F" | sens == "R") {
        f0 <- seqinr::translate(ch, sens = sens, numcode = numcode)
        f2 <- seqinr::translate(c("-", ch), sens = sens, numcode = numcode)
        f1 <- seqinr::translate(c("-", "-", ch), sens = sens, 
            numcode = numcode)
    }
    else if (sens == "B") {
        f0 <- seqinr::translate(ch, sens = "F", numcode = numcode)
        f2 <- seqinr::translate(c("-", ch), sens = "F", numcode = numcode)
        f1 <- seqinr::translate(c("-", "-", ch), sens = "F", 
            numcode = numcode)
        r0 <- seqinr::translate(ch, sens = "R", numcode = numcode)
        r2 <- seqinr::translate(c("-", ch), sens = "R", numcode = numcode)
        r1 <- seqinr::translate(c("-", "-", ch), sens = "R", 
            numcode = numcode)
    }
    f0stops <- sum(f0 == "*")
    f1stops <- sum(f1 == "*")
    f2stops <- sum(f2 == "*")
    if (sens == "B") {
        r0stops <- sum(r0 == "*")
        r1stops <- sum(r1 == "*")
        r2stops <- sum(r2 == "*")
        return(c(f0stops, f1stops, f2stops, r0stops, r1stops, 
            r2stops))
    }
    else {
        return(c(f0stops, f1stops, f2stops))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

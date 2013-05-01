\name{findframe}
\alias{findframe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
findframe(ch, ref, sens = "F", numcode = 1, type = "local")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch}{
%%     ~~Describe \code{ch} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{sens}{
%%     ~~Describe \code{sens} here~~
}
  \item{numcode}{
%%     ~~Describe \code{numcode} here~~
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
function (ch, ref, sens = "F", numcode = 1, type = "local") 
{
    stopvec <- countstops(ch, sens, numcode)
    scorevec <- alignscore(ch, ref, sens, numcode, type)
    numframes <- length(stopvec)
    minstop <- seq(1, numframes)[stopvec == min(stopvec)]
    minscore <- seq(1, numframes)[scorevec == max(scorevec)]
    if (length(minscore) == 1) {
        thisstop <- stopvec[minscore]
        if (thisstop == 0 | thisstop == min(stopvec)) {
            if (sens == "B") {
                framevec <- c(0, 1, 2, 0, 1, 2)
                sensvec <- c("F", "F", "F", "B", "B", "B")
            }
            else {
                framevec <- c(0, 1, 2)
                sensvec <- rep(sens, numframes)
            }
            return(c(as.integer(framevec[minscore]), sensvec[minscore], 
                as.integer(stopvec[minscore]), as.double(scorevec[minscore])))
        }
        else {
            return(NA)
        }
    }
    else {
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

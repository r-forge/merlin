\name{alignscore}
\alias{alignscore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
alignscore(ch = NULL, ref = NULL, sens = "F", numcode = 1, type = "local")
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
function (ch = NULL, ref = NULL, sens = "F", numcode = 1, type = "local") 
{
    s <- c2s(seqinr::translate(ref, sens = "F", numcode = numcode))
    if (sens == "F" | sens == "R") {
        f0 <- c2s(seqinr::translate(ch, sens = sens, numcode = numcode))
        f2 <- c2s(seqinr::translate(c("-", ch), sens = sens, 
            numcode = numcode))
        f1 <- c2s(seqinr::translate(c("-", "-", ch), sens = sens, 
            numcode = numcode))
    }
    else if (sens == "B") {
        f0 <- c2s(seqinr::translate(ch, sens = "F", numcode = numcode))
        f2 <- c2s(seqinr::translate(c("-", ch), sens = "F", numcode = numcode))
        f1 <- c2s(seqinr::translate(c("-", "-", ch), sens = "F", 
            numcode = numcode))
        r0 <- c2s(seqinr::translate(ch, sens = "R", numcode = numcode))
        r2 <- c2s(seqinr::translate(c("-", ch), sens = "R", numcode = numcode))
        r1 <- c2s(seqinr::translate(c("-", "-", ch), sens = "R", 
            numcode = numcode))
    }
    af0 <- pairwiseAlignment(pattern = f0, subject = s, type = type)
    af1 <- pairwiseAlignment(pattern = f1, subject = s, type = type)
    af2 <- pairwiseAlignment(pattern = f2, subject = s, type = type)
    if (sens == "B") {
        ar0 <- pairwiseAlignment(pattern = r0, subject = s, type = type)
        ar1 <- pairwiseAlignment(pattern = r1, subject = s, type = type)
        ar2 <- pairwiseAlignment(pattern = r2, subject = s, type = type)
        return(c(score(af0), score(af1), score(af2), score(ar0), 
            score(ar1), score(ar2)))
    }
    else {
        return(c(score(af0), score(af1), score(af2)))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

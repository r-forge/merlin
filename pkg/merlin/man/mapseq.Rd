\name{mapseq}
\alias{mapseq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapseq(ch = NULL, ref = NULL, sm = nucleotideSubstitutionMatrix(match = 1, mismatch = 0, baseOnly = FALSE, type = "DNA"), gapOpening = 0, gapExtension = -1, aslist = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ch}{
%%     ~~Describe \code{ch} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
  \item{sm}{
%%     ~~Describe \code{sm} here~~
}
  \item{gapOpening}{
%%     ~~Describe \code{gapOpening} here~~
}
  \item{gapExtension}{
%%     ~~Describe \code{gapExtension} here~~
}
  \item{aslist}{
%%     ~~Describe \code{aslist} here~~
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
function (ch = NULL, ref = NULL, sm = nucleotideSubstitutionMatrix(match = 1, 
    mismatch = 0, baseOnly = FALSE, type = "DNA"), gapOpening = 0, 
    gapExtension = -1, aslist = FALSE) 
{
    patt <- DNAStringSet(gsub("-", "", lapply(lapply(myseqs, 
        toupper), c2s), fixed = TRUE))
    subj <- DNAString(gsub("-", "", c2s(toupper(ref)), fixed = TRUE))
    al <- pairwiseAlignment(pattern = patt, subject = subj, type = "local", 
        substitutionMatrix = sm, gapOpening = gapOpening, gapExtension = gapExtension)
    if (aslist) {
        al2 <- lapply(as.character(al), s2c)
        al3 <- lapply(al2, as.SeqFastadna)
        numseq <- length(al3)
        for (i in 1:numseq) {
            attr(al3[[i]], "name") <- attr(ch[[i]], "name")
            attr(al3[[i]], "Annot") <- attr(ch[[i]], "Annot")
        }
        return(al3)
    }
    else {
        return(al)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

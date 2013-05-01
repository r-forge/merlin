\name{read.gbc}
\alias{read.gbc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.gbc(fn, include.sequence = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fn}{
%%     ~~Describe \code{fn} here~~
}
  \item{include.sequence}{
%%     ~~Describe \code{include.sequence} here~~
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
function (fn, include.sequence = FALSE) 
{
    myxml <- xmlTreeParse(fn)
    xmltop <- xmlRoot(myxml)
    numseq <- length(xmltop)
    locus <- rep("", numseq)
    accession <- rep("", numseq)
    seqlen <- rep(0, numseq)
    isolate <- rep(NA, numseq)
    isolationsource <- rep(NA, numseq)
    country <- rep(NA, numseq)
    collectiondate <- rep(NA, numseq)
    organism <- rep(NA, numseq)
    strain <- rep(NA, numseq)
    host <- rep(NA, numseq)
    note <- rep("", numseq)
    if (include.sequence) {
        seq <- rep("", numseq)
    }
    for (i in 1:numseq) {
        print(paste("Processing", i, "of", numseq))
        locus[i] <- unlist(lapply(getNodeSet(xmltop[[i]], "//INSDSeq/INSDSeq_locus"), 
            xmlValue))[1]
        accession[i] <- unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_primary-accession"), xmlValue))[1]
        seqlen[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_length"), xmlValue))[1], NA)
        isolate[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='isolate']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        isolationsource[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='isolation_source']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        country[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='country']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        collectiondate[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='collection_date']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        organism[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='organism']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        strain[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='strain']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        host[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='host']/INSDQualifier_value"), 
            xmlValue))[1], NA)
        note[i] <- null.to.other(unlist(lapply(getNodeSet(xmltop[[i]], 
            "//INSDSeq/INSDSeq_feature-table/INSDFeature/INSDFeature_quals/INSDQualifier[INSDQualifier_name='note']/INSDQualifier_value"), 
            xmlValue))[1], "")
        if (include.sequence) {
            seq[i] <- unlist(lapply(getNodeSet(xmltop[[i]], "//INSDSeq/INSDSeq_sequence"), 
                xmlValue))[1]
        }
    }
    if (include.sequence) {
        return(data.frame(locus, accession, seqlen, isolate, 
            isolationsource, country, collectiondate, organism, 
            strain, host, note, seq))
    }
    else {
        return(data.frame(locus, accession, seqlen, isolate, 
            isolationsource, country, collectiondate, organism, 
            strain, hostnote))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

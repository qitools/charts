\name{ishikawa}
\alias{ishikawa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ishikawa}
\description{
This function implements the cause and effect diagrams of package \href{https://public.opencpu.org/ocpu/cran/qcc/}{qcc} in a OpenCPU app.
}
\usage{
ishikawa(content)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{content}{
One or more lists of string values. Each list will become a branch of the diagram.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
This function returns a plot.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Robert Badgett \email{rbadgett@kumc.edu}
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
function (content) 
{
    KUBlue = "#0022B4"
    SkyBlue = "#6DC6E7"
    par(col.axis = "black", col.lab = KUBlue, col.main = KUBlue, 
        col.sub = KUBlue, bg = SkyBlue, xaxt = "s", yaxt = "s", 
        mai = c(0.956, 0.769, 0.769, 0.394), pin = c(14, 14))
    cause.and.effect(cause = tttt, title = "", effect = "Nonconformity", 
        cex = c(1, 1, 0.9), font = c(2, 3, 2))
    mtext("Cause and effect diagram", side = 3, line = 2, col = KUBlue, 
        font = 2, cex = 1.3)
    mtext("(Ishikawa or fish-bone diagram)", side = 3, line = 1, 
        col = KUBlue, font = 2, cex = 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

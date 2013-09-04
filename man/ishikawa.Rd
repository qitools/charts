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
content =
"
Provider=c('Distracted','Unaware','Inaccessible \n  knowledge','Nonparsable documentation','Not motivated','Not done: reason unknown'),

Patient=c('No visit during time period','Multiple complaints','TTM: early stage','HBM factor', 'No resource: _____'),

System=c('Acute vs continuity visit', 'Specialty vs primary care', 'Personnel absent','Supplies unavailable','Bean counting error'),

Personnel=c('Distracted','Inaccessible knowledge','Nonparsable documentation','Not done: reason unknown','Not motivated','Not empowered')
"

## The function is currently defined as
ishikawa <-
function(content) {
temp <- paste('list(',gsub('\n', '', fixed = TRUE, content),')')
causes<-eval(parse(file = "", n = NULL, text = temp))
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue,bg=SkyBlue,xaxt="s",yaxt="s",mai=c(.956,.769,.769,.394),pin=c(14,14))
cause.and.effect(cause=causes,title="",effect="Nonconformity",cex = c(1, 1, 0.9), font = c(2, 3, 2))
mtext("Cause and effect diagram",        side=3,line=1,col=KUBlue,font=2, cex=1.3)
mtext("(Ishikawa or fish-bone diagram)", side=3,line=0,col=KUBlue,font=2, cex=1)
display_logo(x=0.8,y=-1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

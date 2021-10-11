\name{PLOT}
\alias{PLOT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PLOT
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PLOT(items = items, zoom = "IN", rows = 3, cols = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{items}{
%%     ~~Describe \code{items} here~~
}
  \item{zoom}{
%%     ~~Describe \code{zoom} here~~
}
  \item{rows}{
%%     ~~Describe \code{rows} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
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
function (items = items, zoom = "IN", rows = 3, cols = 5) 
{
    par(mfrow = c(rows, cols), mar = c(2.5, 2.5, 2, 0.5))
    for (i in c(1:length(items))) {
        y = items[i]
        main = names(y)
        lapply(y, zoom, main)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

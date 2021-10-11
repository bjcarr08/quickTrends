\name{IN}
\alias{IN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IN
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IN(y, main)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
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
function (y, main) 
{
    x = c(1:length(y))
    lims = ifelse(max(y) > 75, 100, ifelse(max(y) > 50, 70, ifelse(max(y) > 
        25, 50, ifelse(max(y) > 10, 20, ifelse(max(y) > 5, 10, 
        5)))))
    my_loess <- loess(y^(1/2) ~ x, span = 0.7, degree = 2)
    out <- predict(my_loess)^2
    MSE = Metrics::mse(y, out)
    infl <- c(FALSE, diff(diff(out) > 0) != 0)
    yMaxInfl <- out[ifelse(sum(x[infl]^2) == 0, 1, max(x[infl])):length(out)]
    fit <- lm(yMaxInfl ~ c(1:length(yMaxInfl)))
    slope <- fit$coefficients[[2]]
    lineCol = ifelse(mean(yMaxInfl) < 2, "black", ifelse(slope > 
        0.05, "#FF7F00", ifelse(slope < (-0.05), "blue", "black")))
    boxLTY = ifelse(mean(yMaxInfl) < 1, 3, 1)
    boxCOL = ifelse(mean(yMaxInfl) < 1, "white", 1)
    PCH = ifelse(MSE > 75, 1, 16)
    CEX = ifelse(MSE > 75, 1.5, 1)
    plot(y ~ x, ylim = c(0, lims), xlab = "", ylab = "", main = main, 
        cex = CEX, cex.main = 0.9, pch = PCH, col = lineCol)
    lines(out, col = lineCol)
    points(x[infl], out[infl], pch = 17, cex = 2)
    box(col = boxCOL, lty = boxLTY)
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

\name{orderdIM}
\alias{orderdIM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ordered clusters in a matrix network representation 2
}
\description{
This function reorder the groups in an image matrix in such a way that the blocks with a higher average weigh are placed on the left hand side of the matrix.
}
\usage{
orderdIM(net, res)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{Original network of class \code{matrix}.}
  \item{res}{An object of type \code{list}, containing partition \code{clu} (it can be obtained by using \code{clu(res)} where \code{res} is an object produced by the function \code{oprRandParC} from \code{blockmodeling} package) and image matrix \code{IM} (it can be obtained by using \code{IM(res)} where \code{res} is an object produced by the function \code{oprRandParC} from \code{blockmodeling} package).
   }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns reordered image matrix of class \code{array}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Marjan Cugmas
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[myPhD]{orderedClu}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

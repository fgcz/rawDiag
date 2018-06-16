\name{benchmark}
\alias{benchmark}
\alias{b.Linux}
\alias{b.Apple}
\alias{X.Linux}
\docType{data}
\title{
benchmark 
}
\description{
a data set for benchmarking the throuput of \code{read.raw} and \code{as.rawDiag.mzR} functions.
}
\usage{data("benchmark")}
\format{
  Three data frames.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
the benchmark was performed on the following public available data
\href{http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD006932}{proteomeXchange PXD006932}.
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(benchmark)
## maybe str(benchmark) ; plot(benchmark) ...
}
\keyword{datasets}

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
    \item{\code{b.Linux}}{performed  \code{\link{benchmark_raw}} on a Linux server}
    \item{\code{b.Apple}}{on a apple laptop}
    \item{\code{X.Linux}}{performed  \code{rawDiag:::benchmark_mzR} on a Linux server}
  }
}
\details{
As benchmark data, we downloaded the raw files described in
pmid29183128 on the filesystem availbale through
\href{http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD006932}{proteomeXchange PXD006932}.
For the benchmark we limited the input to 128 files,
corresponding to two times the available number of processor cores of
the Linux system. 
For comparison to the open file standards using the mzR package \code{mzR},
we converted the raw files into mzML files using  version ProteoWizard 3.0.11252
64-bit on Windows 10.
}
\source{
the benchmark was performed on the following public available data
\href{http://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD006932}{proteomeXchange PXD006932}.
}
\seealso{\code{\link{benchmark_raw}}}
\references{
\itemize{
\item{\url{http://planetorbitrap.com/rawfilereader#.WyThDK3QPmE}}
}
}
\examples{
data(benchmark)
length(unique(b.Linux$nrow))
round(sum(unique(b.Linux$file.size)) / 1024^3)
format(sum(unique(b.Linux$nrow)), scientific=FALSE, big.mark = "'")
rawDiag:::.technote_benchmark_figure_1()
rawDiag:::.technote_benchmark_figure_2()

}
\keyword{datasets}
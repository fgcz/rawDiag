\name{WU163763}
\alias{WU163763}
\docType{data}
\title{
MassIVE MSV000082389 / bfabric WU163763
}
\description{
Dataset for demonstration of rawDiag package functions. Contains 9 raw files recorded on a Thermo Fisher Scientific Q Exactive HF-X at the FGCZ, ETHZ/UZH. Each injection contained 1 ug of tryptic HeLa digest (Pierce) and was analyzed in shotgun mode. 04, 05, 07: tech. replicates (LC-MS method a) 08, 09, 11: tech. replicates (LC-MS method b) 12, 13, 16: tech. replicates (LC-MS method c) Details can be found in the corresponding publication.

}
\usage{data("WU163763")}
\format{
  A data frame with 1166726 observations on the following 21 variables.
  \describe{
    \item{\code{filename}}{a character vector}
    \item{\code{scanNumber}}{a numeric vector}
    \item{\code{StartTime}}{a numeric vector}
    \item{\code{BasePeakMass}}{a numeric vector}
    \item{\code{BasePeakIntensity}}{a numeric vector}
    \item{\code{TIC}}{a numeric vector}
    \item{\code{ScanType}}{a character vector}
    \item{\code{CycleNumber}}{a numeric vector}
    \item{\code{MSOrder}}{a character vector}
    \item{\code{MassAnalyzer}}{a character vector}
    \item{\code{PrecursorMass}}{a numeric vector}
    \item{\code{AGC}}{a character vector}
    \item{\code{ChargeState}}{a numeric vector}
    \item{\code{IonInjectionTimems}}{a numeric vector}
    \item{\code{FTResolution}}{a numeric vector}
    \item{\code{LMCorrection}}{a numeric vector}
    \item{\code{PrescanMode}}{a numeric vector}
    \item{\code{MasterScanNumber}}{a numeric vector}
    \item{\code{ElapsedScanTimesec}}{a numeric vector}
    \item{\code{AGCMode}}{a logical vector}
    \item{\code{transient}}{a numeric vector}
  }
}
\source{
\itemize{
  \item \href{ftp://massive.ucsd.edu/MSV000082389}{}
  \item at the Functional Genomics Center Zurich
  \itemize{
  \item \href{https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=163763}{bfabric workunit 163763}
  \item \url{http://fgcz-ms.uzh.ch/p2692/Proteomics/QEXACTIVEHFX_1/tobiasko_20180220_scanSpeed/}
  }
}}
\references{
\url{https://doi.org/10.1101/304485}
}
\examples{
data(WU163763)
## maybe str(WU163763) ; plot(WU163763) ...
}
\author{Tobias Kockmann, 2018}
\keyword{datasets}

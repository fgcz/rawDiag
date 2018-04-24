# rawDiag <img src="inst/shiny/demo/www/octopussy.png" align="right" width="100px" />

An R package for Diagnostic Plots for Mass Spectrometry Data

## Requirements  
for Windows/Linux/MacOSX platforms

Due to license reason, we can not distribute Thermo Fisher Scientific software with the *rawDiag* package.


The *New RawFileReader from Thermo Fisher Scientific* (see http://planetorbitrap.com/rawfilereader)
have to be downloaded and installed to use the R function `read.raw`.

for Linux (Debian) system run the following code snippet:
```
apt-get update \
  && apt-get install mono-complete vim less unzip r-base -y \
  && cd /tmp/ \
  && unzip /tmp/ThermoRawFileReader_linux.4.0.22.nupkg \
  && gacutil -i lib/ThermoFisher.CommonCore.BackgroundSubtraction.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.Data.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.RawFileReader.dll \
  && echo $?
```

## Installation

please note: due to the data size (>=40MB) download can take a while
```{r}
# install.packages("devtools")
devtools::install_github("protViz/rawDiag")
```


## Usage

"Hello; World!" example on the R command line

```{r}
library(rawDiag)
load(file.path(path.package(package = "rawDiag"),
                 file.path("extdata", "PXD006932_Exp3A_smp.RData")))
                 
PlotPrecursorHeatmap(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
PlotMassDistribution(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
```

an interactive shiny example

```{r}
# install.packages("shiny")
# install.packages("DT")
library(shiny)
rawDiag_shiny <- system.file("shiny", "demo", package = "rawDiag")
shiny::runApp(rawDiag_shiny, display.mode = "normal")
```


## See Also

- screen recording (3:02 minutes, size 47MB, no audio track): http://fgcz-ms.uzh.ch/~cpanse/PAPERS/pr-2018-001736.mov
- shiny demo on our compute server: http://fgcz-ms-shiny.uzh.ch:8080/rawDiag-demo/

## References

C. Trachsel, C. Panse, T. Kockmann, J. Grossmann, W. E. Wolski, and R. Schlapbach. *rawDiag - An R package supporting rational LC-MS method optimization for bottom-up proteomics.*, 2018. (manuscript submitted to Journal of Proteome Research; **pr-2018-001736**).

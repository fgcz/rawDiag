# rawDiag <img src="https://user-images.githubusercontent.com/12233339/39515832-84b561ea-4dfb-11e8-9411-276bc6fb71d6.png" align="right" width="100px" />

an R package supporting rational LC-MS method optimization for bottom-up proteomics

"Its raw, fast and colourfull!"

## 1. System Requirements  
for Windows/Linux/MacOSX platforms

### R 
- R (>3.4.0)
- install https://CRAN.R-project.org/package=devtools

### The New Raw File Reader 

Due to license reason, we currently can not distribute Thermo Fisher Scientific software with the *rawDiag* package (we hope that this will change soon).
The *New RawFileReader from Thermo Fisher Scientific* (see http://planetorbitrap.com/rawfilereader)
has to be downloaded and installed to use the R function `read.raw`.

on Linux (Debian) system run the following code snippet once you have downloaded the libraries:
```{sh}
apt-get update \
  && apt-get install mono-complete vim less unzip r-base -y \
  && cd /tmp/ \
  && unzip /tmp/ThermoRawFileReader_linux.4.0.22.nupkg \
  && gacutil -i lib/ThermoFisher.CommonCore.BackgroundSubtraction.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.Data.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.RawFileReader.dll \
  && echo $?
```

## 2. Installation guide

please note: due to the data size (>=40MB) download can take a while
```{r}
# install.packages("devtools")
library("devtools")
devtools::install_github("protViz/rawDiag")
```

## 3. Usage

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

rawDiag - an R package supporting rational LC-MS method optimization for bottom-up proteomics
Christian Trachsel, Christian Panse, Tobias Kockmann, Witold Eryk Wolski, Jonas Grossmann, Ralph Schlapbach
bioRxiv 304485; doi: https://doi.org/10.1101/304485
(manuscript submitted to Journal of Proteome Research; **pr-2018-001736**).

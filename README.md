# rawDiag <img src="inst/shiny/demo/www/octopussy.png" align="right" width="100px" />

An R package for Diagnostic Plots for Mass Spectrometry Data


## Installation

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

Shiny example

```{r}
# install.packages("shiny")
rawDiag_shiny <- system.file("shiny", "demo", package = "rawDiag")
shiny::runApp(rawDiag_shiny, display.mode = "normal")
```


## Links
- screen recording (3:02 minutes, size 47MB, no audio track): http://fgcz-ms.uzh.ch/~cpanse/PAPERS/pr-2018-001736.mov
- shiny demo on our compute server: http://fgcz-ms-shiny.uzh.ch:8080/rawDiag-demo/

## References

C.Trachsel, C.Panse, T.Kockmann, J.Grossmann, W.Wolski, and R. Schlapbach. rawDiag - **An R package supporting rational LC-MS method optimization for bottom-up proteomics.** JPR, 2018. under review Journal of Proteome Research (**pr-2018-001736**).

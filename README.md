# rawDiag <img src="inst/shiny/demo/www/octopussy.png" align="right" width="100px" />

An R package for Diagnostic Plots for Mass Spectrometry Data


## Installation

```{r}
# install.packages("devtools")
devtools::install_github("protViz/rawDiag")
```


## Usage

```{r}
library(rawDiag)
load(file.path(path.package(package = "rawDiag"),
                 file.path("extdata", "PXD006932_Exp3A_smp.RData")))
                 
PlotPrecursorHeatmap(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
PlotMassDistribution(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
```

## References



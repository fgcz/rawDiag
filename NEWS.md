# News for Package 'rawDiag'

## Changes in version 0.99.23 (2024-02-15)

* pass `R CMD check` on bioconductor with `Status: OK`

## Changes in version 0.99.19 (2024-02-14)

* NAMESPACE
  * renamed `read.raw` to `readRaw`
* Documentation
  * add an Introduction and Installation section in vignette
  * removed commented code that isn't run
* Code
  * removed paste cmd in message
  * use the BiocParallel 
* refactor rawDiag shiny application, e.g.,  `buildRawDiagShinyApp`
returns a `shiny::shinyApp` object

## Changes in version 0.99.9 (2024-01-05)

* Add NEWS.md file

## Changes in version 0.99.1 (2023-12-06)

* refacor existing code https://github.com/fgcz/rawDiag
  * use mono assemplies provides through Bioconductor [rawrr](https://bioconductor.org/packages/rawrr/) package
* camel case for public plot functions, e.g., rename
`PlotCycleLoad` to `plotCycleLoad`
* submit to bioconductor [#3251](https://github.com/Bioconductor/Contributions/issues/3251)

## Changes in version 0.0.41 (2021-03-21)
(as demonstrated in ABRF R4Core session.)

* removed data used in JPR Techical Note.

## Changes in version 0.0.2 (2018-04-28)

* doi [10.1021/acs.jproteome.8b00173](https://pubs.acs.org/doi/10.1021/acs.jproteome.8b00173) version.

## Changes in version 0.0.1 (2018-03-13)

* initial commit [4627b798](https://github.com/fgcz/rawDiag/commit/4627b798151cac71e156094fcf5e5d99af037693).

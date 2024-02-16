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

- Add NEWS.md file

## Changes in version 0.99.1 (2023-12-06)

- refacor existing code https://github.com/fgcz/rawDiag

- camel case for public plot functions, e.g., rename
`PlotCycleLoad` to `plotCycleLoad`

- submit to bioconductor [#3251](https://github.com/Bioconductor/Contributions/issues/3251)

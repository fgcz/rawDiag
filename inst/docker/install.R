#R

# Christian Panse <cp@fgcz.ethz.ch>
# Functional Genomics Center Zurich 2018



pkgs <- c( 'devtools',
  'dplyr',
  'ggplot2',
  'hexbin',
  'magrittr',
  'parallel',
  'protViz',
  'rmarkdown',
  'RSQLite',
  'scales',
  'shiny',
  'testthat',
  'tidyr',
  'tidyverse')

install.packages(pkgs, repo="https://stat.ethz.ch/CRAN/")

R.version.string; Sys.info()[c('sysname', 'version')]

# library(devtools); devtools::install_github('protViz/rawDiag')

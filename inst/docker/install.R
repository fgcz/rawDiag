# ADD install.R /tmp
# RUN R --no-save < /tmp/install.R
#R
install.packages(c('shiny', 'rmarkdown', 'tidyr', 'devtools'),
  repo="https://stat.ethz.ch/CRAN/")


library(devtools); devtools::install_github('protViz/rawDiag')

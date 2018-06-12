FROM bioconductor/devel_proteomics2

LABEL version="1.0"
LABEL description="This docker image is for the shiny demo only."
MAINTAINER Christian Panse <cp@fgcz.ethz.ch>

RUN apt-get install libxml2 -y
ADD install.R /tmp
RUN R --no-save < /tmp/install.R && echo $?

# docker run -it -p 8787:8787 cpanse/recmap R -e "library(shiny); recmap_shiny <- system.file('shiny-examples', package = 'recmap'); shiny::runApp(recmap_shiny, display.mode = 'normal', port=8787, host='0.0.0.0')"

FROM bioconductor/devel_proteomics2

LABEL version="1.1"
LABEL description="This file illustrates \
how to compile CS code using the \
ThermoRawFileReader library and use it \
as commandline programm."

MAINTAINER Christian Panse <Christian.Panse@gmail.com>

# download from http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
# ADD ThermoRawFileReader/ThermoRawFileReader_linux.4.0.22.nupkg /tmp/

ADD install.R /tmp
RUN apt-get update \
  && apt-get install mono-complete vim less unzip r-base curl libxml2 -y
RUN R --no-save < /tmp/install.R

RUN cd /tmp/ \
  && pwd \
  && curl -LO https://github.com/fgcz/rawDiag/archive/master.zip \
  && unzip master.zip \
  && mv rawDiag-master rawDiag \
  && curl http://fgcz-ms.uzh.ch/~cpanse/rawDiag_0.0.10.tar.gz  \
    | tar xvfz - --wildcards --no-anchored '*.dll'
#  && gacutil -i rawDiag/exec/ThermoFisher.CommonCore.BackgroundSubtraction.dll \
#  && gacutil -i rawDiag/exec/ThermoFisher.CommonCore.Data.dll \
#  && gacutil -i rawDiag/exec/ThermoFisher.CommonCore.RawFileReader.dll

#ADD fgcz_raw.cs /tmp/
RUN cd /tmp \
  && mcs /out:rawDiag/exec/fgcz_raw.exe \
  rawDiag/inst/docker/fgcz_raw.cs /r:rawDiag/exec/ThermoFisher.CommonCore.Data.dll \
  /r:rawDiag/exec/ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:rawDiag/exec/ThermoFisher.CommonCore.RawFileReader.dll /target:exe 

RUN cd /tmp \
  && R CMD build rawDiag \
  && R CMD check rawDiag_*tar.gz \
  && R CMD INSTALL rawDiag_*tar.gz; \
  echo $?


FROM bioconductor/devel_proteomics2

LABEL version="1.0"
LABEL description="This file illustrates \
how to compile CS code using the \
ThermoRawFileReadera library and use it \
as commandline programm."

MAINTAINER Christian Panse <Christian.Panse@gmail.com>

# download from http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
ADD ThermoRawFileReader/ThermoRawFileReader_linux.4.0.22.nupkg /tmp/

RUN apt-get update \
  && apt-get install mono-complete vim less unzip r-base curl -y \
  && cd /tmp/ \
  && unzip /tmp/ThermoRawFileReader_linux.4.0.22.nupkg \
  && gacutil -i lib/ThermoFisher.CommonCore.BackgroundSubtraction.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.Data.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.RawFileReader.dll \
  && echo $?

ADD fgcz_raw.cs /tmp/
RUN cd /tmp \
  && mcs /out:/usr/local/bin/fgcz_raw.exe \
  fgcz_raw.cs /r:lib/ThermoFisher.CommonCore.Data.dll \
  /r:lib/ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:lib/ThermoFisher.CommonCore.RawFileReader.dll /target:exe \
  && echo $?


RUN apt-get install libxml2 -y
ADD install.R /tmp
RUN R --no-save < /tmp/install.R && echo $?

FROM debian:stable 

MAINTAINER Christian Panse <Christian.Panse@gmail.com>

# http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
ADD ThermoRawFileReader_linux.4.0.22.nupkg /tmp/

RUN apt-get update \
  && apt-get install mono-complete vim less unzip r-base -y \
  && cd /tmp/ \
  && unzip /tmp/ThermoRawFileReader_linux.4.0.22.nupkg \
  && gacutil -i lib/ThermoFisher.CommonCore.BackgroundSubtraction.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.Data.dll \
  && gacutil -i lib/ThermoFisher.CommonCore.RawFileReader.dll \
  && echo $?

ADD Program.cs /tmp/
RUN cd /tmp \
  && mcs /out:/usr/local/bin/fgcz_raw.exe \
  Program.cs /r:lib/ThermoFisher.CommonCore.Data.dll \
  /r:lib/ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:lib/ThermoFisher.CommonCore.RawFileReader.dll /target:exe \
  && echo $?

ADD install.R /tmp
RUN R --no-save < /tmp/install.R

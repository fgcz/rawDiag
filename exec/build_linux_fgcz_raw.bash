#!/bin/bash

# build script Christian Panse <cp@fgcz.ethz.ch> 2018,2019

set -x

mcs /target:library ../src/fgcz_raw.cs  /out:fgcz_raw.dll  \
  /r:ThermoFisher.CommonCore.Data.dll \
  /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:ThermoFisher.CommonCore.RawFileReader.dll \
  /optimize \
  /platform:anycpu

## apt-get install mono-mcs
mcs /out:fgcz_raw.exe \
  ../src/fgcz_raw.cs /r:ThermoFisher.CommonCore.Data.dll \
  /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:ThermoFisher.CommonCore.RawFileReader.dll /target:exe \
  /optimize \
  /platform:anycpu

exit $?

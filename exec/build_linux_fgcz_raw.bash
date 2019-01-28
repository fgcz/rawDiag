#!/bin/bash

## apt-get install mono-mcs
mcs /out:fgcz_raw.exe \
  ../inst/docker/fgcz_raw.cs /r:ThermoFisher.CommonCore.Data.dll \
  /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll \
  /r:ThermoFisher.CommonCore.RawFileReader.dll /target:exe \
  /optimize \
  /platform:anycpu

exit $?

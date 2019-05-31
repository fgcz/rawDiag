CSC=mcs
CSCFLAGS=-lib:../exec /r:ThermoFisher.CommonCore.Data.dll /r:ThermoFisher.CommonCore.MassPrecisionEstimator.dll /r:ThermoFisher.CommonCore.RawFileReader.dll /optimize /platform:anycpu
#CSCFLAGS=/r:../exec/ThermoFisher.CommonCore.Data.dll /r:../exec/ThermoFisher.CommonCore.MassPrecisionEstimator.dll /r:../exec/ThermoFisher.CommonCore.RawFileReader.dll /optimize /platform:anycpu

dll:
	$(CSC) /target:library rawDiag.cs  /out:../exec/rawDiag.dll  $(CSCFLAGS)

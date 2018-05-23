# A recipe on howto compile and run a commandline program using *RawFileReader from Thermo Fisher Scientific*


## Install

- login; on your Linux Docker server

- Get *The New RawFileReader from Thermo Fisher Scientific* from http://planetorbitrap.com/rawfilereader

## Build Docker image

```bash 
docker build -t $USER/fgcz-raw:v1  .
```

## Testing


copy some sample data in the following we use http://central.proteomexchange.org/cgi/GetDataset?ID=PXD006932

```bash
docker run -v /scratch/$USER/PXD006932/Exp3A/:/scratch/$USER/PXD006932/Exp3A/ \
  -a stdin \
  -a stdout \
  -i -t \
  $USER/fgcz-raw:v1 mono /usr/local/bin/fgcz_raw.exe /scratch/$USER/PXD006932/Exp3A/20161213_NGHF_DBJ_SA_Exp3A_HeLa_1ug_7min_7500_03.raw info
```

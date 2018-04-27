# A recipe on howto compile and run a commandline program using *RawFileReader from Thermo Fisher Scientific*


## Install

- login; on your Linux Docker server

- Get *RawFileReader from Thermo Fisher Scientific*

- http://planetorbitrap.com/rawfilereader

- Build Docker image

```bash 
docker build -t cpanse/fgcz-raw:v1  .
```

- Run the commandline program


## Testing


copy some sample data in the following we use http://central.proteomexchange.org/cgi/GetDataset?ID=PXD006932

```bash
docker run -v /scratch/cpanse/PXD006932/Exp3A/:/scratch/cpanse/PXD006932/Exp3A/ -a stdin -a stdout -i -t cpanse/fgcz-raw:v1 mono /usr/local/bin/fgcz_raw.exe
```

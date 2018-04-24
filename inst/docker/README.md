# A recipe on howto compile and run a commandline program using *RawFileReader from Thermo Fisher Scientific*

## Get *RawFileReader from Thermo Fisher Scientific*

- http://planetorbitrap.com/rawfilereader

## Build Docker image

```bash 
docker build -t cpanse/fgcz-raw:v1  .
```

## docker run -v /scratch/:/scratch/ -a stdin -a stdout -i -t

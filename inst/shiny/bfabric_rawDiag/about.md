# About **rawDiag** 


an R package supporting rational LC-MS method optimization for bottom-up proteomics on multiple OS platforms


main features:
 
1. **multiplatform** and **fast** reading through using [The New RawFileReader from Thermo Fisher Scientific](http://planetorbitrap.com/rawfilereader).
2. uses latest visualization generation through using Rs [ggplot2](https://cran.r-project.org/package=ggplot2) package.
3. provides an R interface to your instrument raw data.
4. ships with an example [shiny application](http://fgcz-ms-shiny.uzh.ch:8080/bfabric_rawDiag/).


## 1. System Requirements  

a Windows/Linux/MacOSX x64 platform 


### 1.1 .NET Framework and R

- https://www.mono-project.com/ (>4.0.22) for (Linux and MacOSX)
- .NET Framework 4.5.1 or higher (Windows)
- R (>3.4.0); please do not use R (3.5.0) on Windows! There is an `system2` issue.
- install https://CRAN.R-project.org/package=devtools
- if you want support for [Open File Standards](http://www.psidev.info/) install the [mzR](http://bioconductor.org/packages/mzR/) package. 
- U.S. language setting on windows [see issue 33](https://github.com/fgcz/rawDiag/issues/33)

### 1.2. The New RawFileReader .Net assembly from Thermo Fisher Scientific

**If your installation does not work with the below-mentioned instructions, do not hesitate to request a ready to run R package from the authors via [Email, SUBJECT `request rawDiag package`](mailto:cp@fgcz.ethz.ch?SUBJECT=request%20current%20rawDiag%20package).**


Due to [licensing reasons](https://github.com/fgcz/rawDiag/blob/master/inst/docker/ThermoRawFileReader/RawFileReaderLicense.doc),
we currently not allowed to distribute Thermo Fisher Scientific software with the *rawDiag*
package (we hope that this will change soon).
The [New RawFileReader from Thermo Fisher Scientific](http://planetorbitrap.com/rawfilereader)
has to be downloaded and installed separately in order to be able to directly read Thermo
raw-files (by using the R function `read.raw`).

To install [the New RawFileReader .Net assembly](http://planetorbitrap.com/rawfilereader) follow the installation instructions provided by Thermo Fisher Scientific.



## 2. Instructions for use

read the vignettes.

```{r}
browseVignettes('rawDiag')
```

the documentation of the function is available through the R man pages.

## 3. Useful links
  
- [github.com/fgcz/rawDiag](https://github.com/fgcz/rawDiag)
- [reading XIC and Scan information](http://fgcz-ms.uzh.ch/~cpanse/rawDiagXICdemo.pdf)
- http://planetorbitrap.com/rawfilereader
- [screen recording (3:02 minutes, size 47MB, no audio track)](http://fgcz-ms.uzh.ch/~cpanse/PAPERS/pr-2018-001736.mov)
- [shiny demo on our compute server](http://fgcz-ms-shiny.uzh.ch:8080/rawDiag-demo/)
- [ASMS 2018 poster as PDF(1.8M, md5=dab9388c1a465d931e9d2345119a2827)](http://fgcz-ms.uzh.ch/~cpanse/ASMS2018_ID291250.pdf)

## 4. To cite rawDiag in publications, please use

Christian Trachsel, Christian Panse, Tobias Kockmann, Witold Eryk Wolski, Jonas Grossmann, Ralph Schlapbach
*rawDiag - an R package supporting rational LC-MS method optimization for bottom-up proteomics*
, JPR 17(8), 2018, [DOI:10.1021/acs.jproteome.8b00173](https://doi.org/10.1101/304485).



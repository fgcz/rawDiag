# rawDiag <img src="https://user-images.githubusercontent.com/12233339/39515832-84b561ea-4dfb-11e8-9411-276bc6fb71d6.png" align="right" width="100px" />

an R package supporting rational LC-MS method optimization for bottom-up proteomics

"Its raw, fast and colorful!" [(WEW)](https://github.com/wolski)


**If your installation does not work with the below-mentioned instructions, do not hesitate to request a ready to run R package from the authors via Email, subject `request rawDiag package`.**


## 1. System Requirements  
for Windows/Linux/MacOSX platforms with a 64 Bit architecture


### 1.1. R and mono

- https://www.mono-project.com/ (>4.0.22) for (Linux and MacOSX)
- R (>3.4.0)
- install https://CRAN.R-project.org/package=devtools
- if you want support for [Open File Standards](http://www.psidev.info/) install [mzR](http://bioconductor.org/packages/mzR/) package. 

### 1.2. The New RawFileReader .Net assembly from Thermo Fisher Scientific

Due to licensing reasons, we currently not allowed to distribute Thermo Fisher Scientific software with the *rawDiag* package (we hope that this will change soon).
The *New RawFileReader from Thermo Fisher Scientific* (see http://planetorbitrap.com/rawfilereader)
has to be downloaded and installed separately in order to be able to directly read Thermo raw-files (e.g. use the R function `read.raw`).

To install *the New RawFileReader .Net assembly* follow the installation instructions provided by Thermo Fisher Scientific.


### 1.3 Versions the software has been tested on

|plattform|plattform version|R version|
| :------- |---------------:| -------:|
|Linux     | Debian 9 | 3.5.0 |
|[dockerhub](https://hub.docker.com/r/cpanse/rawdiag) | bioconductor/devel_proteomics2| 2017-12-31 r73996 |
|Windows   | 7 | 3.4.1 |
|Windows   | 10 | 3.5.0 |
|Windows   | Server 2012 R2 x64 | 3.4.4|
|MacOSX    | 10.13.5 (17F77)|3.4.2|
|MacOSX    | 10.11.6 (15G20015)|3.4.3 |

## 2. Installation guide

### 2.1. Instructions
To ensure the proper function of this R package please check if all the requirements are fullfilled prior to using it.

#### all OS

the following code downloads and installs the R package from the Github without the required third party .dll files:

please note: due to the data size (>=40MB) download can take a while
```{r}
# install.packages("devtools")
library("devtools")
devtools::install_github("fgcz/rawDiag", build_vignettes = FALSE)
```


### 2.2. Typical install time on a "normal" desktop computer

* requirements: 1 to 30 minutes; one minute if you are a C# and R developer
* the rawDiag package through github: 10 minutes 

## 3. Demonstration

### 3.1 R commandline code snippet

"Hello; World!" example on the R command line

```{r}
library(rawDiag)
load(file.path(path.package(package = "rawDiag"),
                 file.path("extdata", "PXD006932_Exp3A_smp.RData")))
                 
PlotPrecursorHeatmap(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
PlotMassDistribution(PXD006932_Exp3A_HeLa_1ug_60min_7500_02)
```

### 3.2 An interactive shiny example

```{r}
# install.packages("shiny")
# install.packages("DT")
library(shiny)
rawDiag_shiny <- system.file('shiny', 'demo', package = 'rawDiag')
shiny::runApp(rawDiag_shiny, display.mode = 'normal')
```

### 3.3 An interactive shiny example running on your docker enviroment

source: [dockerhub](https://hub.docker.com/r/cpanse/rawdiag/)

```
docker pull cpanse/rawdiag \
&& docker run -it -p 8787:8787 cpanse/rawdiag R -e "library(shiny); \
   rawDiag_shiny <- system.file('shiny', 'demo', package = 'rawDiag'); \
   shiny::runApp(rawDiag_shiny, display.mode = 'normal', port=8787, host='0.0.0.0')"
```

connect with your web browser to http://yourdockerhostname:8787

## 4. Instructions for use

read the vignettes.

```{r}
browseVignettes('rawDiag')
```

the documentation of the function is available through the R man pages.

## 5. Useful Links
- http://planetorbitrap.com/rawfilereader
- [screen recording (3:02 minutes, size 47MB, no audio track)](http://fgcz-ms.uzh.ch/~cpanse/PAPERS/pr-2018-001736.mov)
- [shiny demo on our compute server](http://fgcz-ms-shiny.uzh.ch:8080/rawDiag-demo/)
- *rawDiag - an R package supporting rational LC-MS method optimization for bottom-up proteomics*
Christian Trachsel, Christian Panse, Tobias Kockmann, Witold Eryk Wolski, Jonas Grossmann, Ralph Schlapbach
bioRxiv 304485; doi: https://doi.org/10.1101/304485
(manuscript submitted to Journal of Proteome Research; **pr-2018-001736**).

- [MassIVE MSV000082389](https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?task=b231e78d674345798ebe50e46a9a3a93)

- [ASMS 2018 poster as PDF(1.8M, md5=dab9388c1a465d931e9d2345119a2827)](http://fgcz-ms.uzh.ch/~cpanse/ASMS2018_ID291250.pdf)

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

if (!require("bfabricShiny")){
  message("running without bfabricShiny")
}
library(protViz)
library(rawDiag)
library(parallel)
library(tidyr)
library(rmarkdown)
library(base64enc)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
# ----bfabricShinyModule---- 
  if (require("bfabricShiny")){
 bf <- callModule(bfabric, "bfabric8",
                applicationid = c(7, 160, 161, 162, 163, 176, 177, 197, 214, 232),
                 resoucepattern = 'raw$|RAW$',
                 resourcemultiple = TRUE)
  }
  
# ----Configuration----  
  filesystemRoot<- "/scratch/cpanse/"
  if(!dir.exists(filesystemRoot)){
    filesystemRoot <-  Sys.getenv('HOME')
  }
  
  filesystemDataDir = c("Downloads", 
                        "data",
                        "raw",
                        "WU163230",
                        "WU163763",
                        "PXD006932/Exp3A",
                        "PXD006932/Exp3B",
                        "PXD006932/Exp6A",
                        "PXD006932/Exp6B",
                        "PXD006932/Exp7A",
                        "PXD006932/Exp7B",
                        "PXD006932/Exp8A",
                        "PXD006932/Exp8B",
                        "PXD006932/SA"
  )
  
  filesystemDataDir <- filesystemDataDir[dir.exists(file.path(filesystemRoot, filesystemDataDir))]
  
  
  
  values <- reactiveValues(pdfcontent=NULL,
                           filesystemRoot=filesystemRoot,
                           filesystemDataDir = filesystemDataDir,
                           RDataRoot = file.path(path.package(package = "rawDiag"), "extdata"),
                           RDataData = c("WU163763"))
  

  # $filesystemDataDir)
  # values$filesystemDataDir <- values$filesystemDataDir[dir.exists(F)]
  
  output$tabs <- renderUI({
  
# ----tabsetPanel ----
    tabsetPanel(
      tabPanel("TIC/Basepeak", list(helpText("displays the total ion  chromatogram (TIC) and the base peak chromatogram."),
                                    plotOutput("tic.basepeak", height = input$graphicsheight))),
      tabPanel("Scan Frequency", list(helpText("graphs scan frequency versus RT or scan frequency marginal distribution for violin."),
                                      plotOutput("scan.frequency", height = input$graphicsheight))),
      tabPanel("Scan Time", list(helpText("plots scan time as function of RT for each MSn level. A smooth curve displays the trend."),
                                 plotOutput("scan.time", height = input$graphicsheight))),
      tabPanel("Cycle Load", list(helpText("displays duty cycle load (number of MS2 scans per duty cycle) as a function of retention time (RT) (scatter plots) or its marginal distribution (violin)."),
                                  plotOutput("cycle.load", height = input$graphicsheight))),
      tabPanel("Mass Distribution", list(helpText("displays mass distribution using color coding according to charge state (trellis) or file (violin)."), 
                                         plotOutput("mass.distribution", height = input$graphicsheight))),
      tabPanel("Lock Mass Correction", list(helpText("graphs the lock mass deviations along RT (note: this example data were acquired with lock mass correction)."), 
                                            plotOutput("lm.correction", height = input$graphicsheight))),
      tabPanel("Injection Time", list(helpText("displays injection time as a function  of RT. A smooth curve graphs the trend. The maximum is indicated by a red dashed line."), 
                                      plotOutput("injection.time", height = input$graphicsheight))),
      tabPanel("Mass Heatmap", list(helpText("draws a 2D histogram of the peak count ~ charge deconvoluted mass along RT."), 
                                    plotOutput("mass.heatmap", height = input$graphicsheight))),
      tabPanel("Precursor Heatmap", list(helpText("draws a 2D histogram of the peak count ~ charge deconvoluted mass along RT."), 
                                         plotOutput("precursor.heatmap", height = input$graphicsheight))),
      tabPanel("Cycle Time", list(helpText(" displays cycle time with respect to RT (scatter plots) or its marginal distribution (violin). A smooth curve graphs the trend. The maximum is indicated by a red dashed line."), plotOutput("cycle.time", height = input$graphicsheight))),
      tabPanel("Charge State", list(helpText("displays charge state distributions as biologist-friendly bar charts as absolute counts."), 
                                    plotOutput("charge.state", height = input$graphicsheight))),
      tabPanel("XICs", list(helpText("displays XICs of given masses."), 
                                    plotOutput("xic", height = input$graphicsheight))),
      tabPanel("XIC AUC table", DT::dataTableOutput("tableXICAUC")),
      tabPanel("Raw table", DT::dataTableOutput("table")),
      tabPanel("Raw info", DT::dataTableOutput("tableInfo")),
      #sessionInfo
      tabPanel("sessionInfo", verbatimTextOutput("sessionInfo"))
      
    )
  })
   
 
  getRawfiles <- reactive({
    message(file.path(values$filesystemRoot, input$root))
    
    f <- list.files(file.path(values$filesystemRoot,input$root))
    f[grep("raw$", f)]
    
  })
  
  
  output$rawfile <- renderUI({
    if(length(getRawfiles()) > 0){
    selectInput('rawfile', 'rawfile:', getRawfiles(), multiple = TRUE)
    }else{
      helpText("no files available")
    }
  })
  
# ----Source----  
  output$sourceFilesystem <- renderUI({
    if (input$source == 'filesystem'){
      tagList(
        selectInput('root', 'root:', values$filesystemDataDir,  multiple = FALSE),
        htmlOutput('rawfile')
        )
    } 
  })
  output$sourcePackage <- renderUI({
    if (input$source == 'package') {
      selectInput('RData', 'RData:',  values$RDataData, multiple = FALSE)
    } 
  })
  
  output$sourceBfabric <- renderUI({
    if(input$source == 'bfabric'){
      if (require("bfabricShiny")){
       bfabricInput("bfabric8")
      }
    }else if(input$source == 'package'){
        actionButton("load", "load")
    }else if(input$source == 'filesystem' & length(getRawfiles()) > 0){
      actionButton("load", "load")
    }
  })
  
  output$ReaderParameter <- renderUI({
    if(input$source  %in% c('bfabric', 'filesystem'))
    {
      cmds <- c("~/RiderProjects/fgcz-raw/bin/Debug/fgcz_raw.exe",
                "~cp/bin/fgcz_raw.exe",
                paste(path.package(package = "rawDiag"), "exec/fgcz_raw.exe", sep="/"))
      cmds <- sapply(cmds, function(x){if(file.exists(x)){x}else{NA}})
      cmds <- cmds[!is.na(cmds)]
      
      tagList(h3("RawFileReaderOptions"),
              selectInput('cmd', 'cmd:', cmds, multiple = FALSE),
              checkboxInput("usemono", "Use mono", TRUE),
              sliderInput("mccores", "Cores",
                          min = 1, max = 24,
                          value = 12))
    }
  })

  
  output$XICParameter <- renderUI({
    if(input$source  %in% c('bfabric', 'filesystem'))
    {
      peptideGroup <- c("iRT", "glyco", "msqc1")
      tagList(h3("XIC Options"),
              selectInput('XICpepitdes', 'peptides:', peptideGroup,
                          multiple = FALSE),
              selectInput('XICtol', 'tolerance in ppm:', c(10, 15, 20, 50, 100),
                          multiple = FALSE),
              checkboxInput('XICmainPeak', 'extract main peak',
                            value = FALSE, width = NULL),
              selectInput('XICnMinPeaks', 'minimal number of peaks:',
                          c(5, 10, 20,50),
                          selected = 10,
                          multiple = FALSE)
              
      )       
    }
  })
  
 
  # ----- RDataSave -------
    
  RDataSave <- observeEvent(input$RDataSave, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "saving ...")
    on.exit(progress$close())
    raw <- rawData()
    fn <- tempfile(fileext = ".RData")
    progress$set(detail = fn)
    save(raw, file = fn)
    message(fn)
    
  })
  
  queryMass <- reactive({
    df <- data.frame()
    if (input$XICpepitdes == 'glyco'){
      df <- data.frame(peptide = c('IgG1_EEQYNSTYR_G0F', 'IgG1_EEQYNSTYR_G1F',
                                   'IgG1_EEQYNSTYR_G2F', 'IgG1_EEQYNSTYR_A1F',
                                   'IgG1_EEQYNSTYR_A1F-2', 'IgG1_EEQYNSTYR_G1F+NeuAc', 'IgG1_EEQYNSTYR_A2F', 'IgG1_EEQYNSTYR_G0F+BGlcNAc', 'IgG1_EEQYNSTYR_G1F+BGlcNAc', 'IgG1_EEQYNSTYR_G2F+BGlcNAc', 'IgG1_EEQYNSTYR_A1F+BGlcNAc', 'IgG1_EEQYNSTYR_G1F+NeuAc+bGlcNAc', 'IgG1_EEQYNSTYR_G0', 'IgG1_EEQYNSTYR_G1', 'IgG1_EEQYNSTYR_G2', 'IgG1_EEQYNSTYR_A1', 'IgG1_EEQYNSTYR_G1+NeuAc', 'IgG1_EEQYNSTYR_G1+NeuAc2', 'IgG2/3_EEQFNSTFR_G0F', 'IgG2/3_EEQFNSTFR_G1F', 'IgG2/3_EEQFNSTFR_G2F', 'IgG2/3_EEQFNSTFR_A1F', 'IgG2/3_EEQFNSTFR_A1F-2', 'IgG2/3_EEQFNSTFR_G1F+NeuAc', 'IgG2/3_EEQFNSTFR_A2F', 'IgG2/3_EEQFNSTFR_G0F+BGlcNAc', 'IgG2/3_EEQFNSTFR_G1F+BGlcNAc', 'IgG2/3_EEQFNSTFR_G2F+BGlcNAc', 'IgG2/3_EEQFNSTFR_A1F+BGlcNAc', 'IgG2/3_EEQFNSTFR_G1F+NeuAc+bGlcNAc', 'IgG2/3_EEQFNSTFR_G0', 'IgG2/3_EEQFNSTFR_G1', 'IgG2/3_EEQFNSTFR_G2', 'IgG2/3_EEQFNSTFR_A1', 'IgG2/3_EEQFNSTFR_G1+NeuAc', 'IgG4_EEQFNSTYR_G0F', 'IgG4_EEQFNSTYR_G1F', 'IgG4_EEQFNSTYR_G2F', 'IgG4_EEQFNSTYR_A1F', 'IgG4_EEQFNSTYR_A1F-2', 'IgG4_EEQFNSTYR_G1F+NeuAc', 'IgG4_EEQFNSTYR_A2F', 'IgG4_EEQFNSTYR_G0F+BGlcNAc', 'IgG4_EEQFNSTYR_G1F+BGlcNAc', 'IgG4_EEQFNSTYR_G2F+BGlcNAc', 'IgG4_EEQFNSTYR_A1F+BGlcNAc', 'IgG4_EEQFNSTYR_G1F+NeuAc+bGlcNAc', 'IgG4_EEQFNSTYR_G0', 'IgG4_EEQFNSTYR_G1', 'IgG4_EEQFNSTYR_G2', 'IgG4_EEQFNSTYR_A1', 'IgG4_EEQFNSTYR_G1+NeuAc'),
                       mZ = c(878.68723,932.70483,986.72243,1083.75423,1083.75423,1029.73663,1180.78603,946.380353333333,1000.39795333333,1054.41555333333,1151.44735333333,1097.42975333333,830.001263333333,884.018863333333,938.036463333333,1035.06826333333,981.050663333333,981.050663333333,868.023953333333,922.041553333333,976.059153333333,1073.09095333333,1073.09095333333,1019.07335333333,1170.12275333333,935.717076666667,989.734676666667,1043.75227666667,1140.78407666667,1086.76647666667,819.337986666667,873.355586666667,927.373186666667,1024.40498666667,970.387386666667,873.35559,927.37319,981.39079,1078.42259,1078.42259,1024.40499,1175.45439,941.048713333333,995.066313333333,1049.08391333333,1146.11571333333,1092.09811333333,824.669623333333,878.687223333333,932.704823333333,1029.73662333333,975.719023333333), peptide=NA, z=NA)
    }else if(input$XICpepitdes == 'msqc1'){
      
      peptideSeq <- c('VLDALQAIK', 'GGPFSDSYR', 'SADFTNFDPR',
                      'AVQQPDGLAVLGIFLK',
                      'ALIVLAHSER', 'EGHLSPDIVAEQK', 'GYSIFSYATK', 'ESDTSYVSLK',
                      'FEDENFILK', 'VSFELFADK', 'GAGAFGYFEVTHDITK', 'NLSVEDAAR',
                      'FSTVAGESGSADTVR', 'TAENFR')
        
        
      df <- data.frame(peptide = peptideSeq, z = 2)
      df$mZ <- (parentIonMass(as.character(df$peptide)) + 1.008) /  df$z
      
    }else{
      # IRT
      peptideSeq <- c('LGGNEQVTR',
                      'YILAGVENSK',
                      'GTFIIDPGGVIR',
                      'GTFIIDPAAVIR',
                      'GAGSSEPVTGLDAK',
                      'TPVISGGPYEYR',
                      'VEATFGVDESNAK',
                      'TPVITGAPYEYR',
                      'DGLDAASYYAPVR',
                      'ADVTPADFSEWSK',
                      'LFLQFGAQGSPFLK')
      df <- data.frame(peptide = peptideSeq, z = 2)
      df$mZ <- (parentIonMass(as.character(df$peptide)) + 1.008) /  df$z
     
    }
    df
  })
  
  # ----- rawXICData -------
  
  extractPeak <- function(XIC, fit=TRUE, dt=0.3, minPeaks = 10, ...){
    
    intensities.max <- max(XIC$intensities)
    
    max.idx <- which(XIC$intensities == intensities.max)
    
    
    apex.idx <- which((XIC$t[max.idx] - dt) < XIC$t &  XIC$t < (XIC$t[max.idx] + dt))
    
    rv <- NA
    if(length(apex.idx) > minPeaks){
      x <- XIC$times[apex.idx]  
      y <- XIC$intensities[apex.idx]
      rv <- list(times=x, intensities=y, mass = XIC$mass)
    }
    else{
      rv <- list(times=NA, intensities=NA, mass = XIC$mass)
    }
    rv
  }
  
  rawXICData  <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("extracting XICs"))
    on.exit(progress$close())
    
    
    if (input$source == 'filesystem'){
      
      rf <- unique(file.path(values$filesystemRoot, file.path(input$root, input$rawfile)))
      
      rv <- plyr::rbind.fill(lapply(rf,
                                      function(file){ 
                                        
                                        X <- readXICs(rawfile = file, 
                                                 masses = queryMass()$mZ,
                                                 tol = input$XICtol,
                                                 mono=input$usemono
                                                ) 
                                        df <- queryMass()
                                        for (i in 1:length(X)){
                                          X[[i]]$mass <- paste(df$peptide[i], "| z =", df$z[i], '| mZ =', df$mZ[i], sep  = ' ')
                                        }
                                        
                                        if(input$XICmainPeak){
                                          X <- lapply(X, extractPeak, minPeaks=input$XICnMinPeaks)
                                        }
                                          
                                        Y <- lapply(X, function(x){
                                          if(length(x$times)>1){
                                            df <- data.frame(time = x$times, intensity = x$intensities, mass=rep(x$mass, length(x$times)));
                                            return(df)
                                          }})
                                        Y <- do.call('rbind', Y)
                                        Y$filename <- rep(basename(file), nrow(Y))
                                        as.data.frame(Y)
                                        }))
      return(rv)
    }else if(input$source == 'bfabric'){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("loading XIC data"))
      on.exit(progress$close())
      
      resources <- bf$resources()$relativepath
      rf <- resources[resources %in% input$relativepath]
      rf <- file.path("/srv/www/htdocs/", rf)
      rv <- plyr::rbind.fill(
        mclapply(rf, function(file){
          X <- readXICs(rawfile = file, 
                        masses = queryMass()$mZ,
                        tol = input$XICtol,
                        mono=input$usemono
          ) 
          df <- queryMass()
          for (i in 1:length(X)){
            X[[i]]$mass <- paste(df$peptide[i], "| z =", df$z[i], '| mZ =', df$mZ[i], sep  = ' ')
          }
          if(input$XICmainPeak){
            X <- lapply(X, extractPeak, minPeaks=input$XICnMinPeaks)
          }
          Y <- lapply(X, function(x){
            if(length(x$times)>1){
              df <- data.frame(time = x$times, intensity = x$intensities, mass=rep(x$mass, length(x$times)));
              return(df)
            }})
          Y <- do.call('rbind', Y)
          Y$filename <- rep(basename(file), nrow(Y))
          as.data.frame(Y)
        }, mc.cores = input$mccores))
      return(rv)
    }else{NULL}
    
  })
  
  
  extractMainPeak <- function(XIC, plot=FALSE, fit=FALSE, dt=0.3, nmin =10, ...){
    
    intensities.max <- max(XIC$intensities)
    max.idx <- which(XIC$intensities == intensities.max)
    apex.idx <- which((XIC$t[max.idx] - dt) < XIC$t &  XIC$t < (XIC$t[max.idx] + dt))
    
    AUC <- NA
    if(length(apex.idx) > nmin){
      x <- XIC$times[apex.idx]  
      y <- XIC$intensities[apex.idx]
      
      AUC <- sum(diff(x) * (head(y, -1) + tail(y, -1))) / 2
      
      if(plot){
        plot(XIC, ...)
        abline(v=XIC$t[max.idx], col='red')
        abline(v=c(min(x), max(x)), col='grey')
        axis(3, XIC$t[max.idx],  paste("AUC =", round(AUC,2),
                                       "n =", length(apex.idx)),
             col='red', lwd=2)
        
        plot(x, y, type='h', xlim = c(XIC$t[max.idx] -2*dt, XIC$t[max.idx] + 2*dt))
        axis(3, XIC$t[max.idx],  paste("AUC =", round(AUC,2),
                                       "n =", length(apex.idx)),
             col='red', lwd=2)
      }
      
      # fit 
      if (fit){
        peak <- data.frame(logy = log(y), x = x)
        x.mean <- mean(peak$x)
        #x.mean <-XIC$t[max.idx]
        x.sd <- sd(peak$x)
        peak$xc <- (peak$x - x.mean) 
        fit <- lm(logy ~ xc + I(xc^2), data = peak)
        xx <- with(peak, seq(min(xc) - 0.2, max(xc) + 0.2, length = 100))
        if(plot){
          abline(v = x.mean, col = rgb(0.1, 0.8, 0.1, alpha = 0.9), lwd = 1)
          lines((xx + x.mean), exp(predict(fit, data.frame(xc = xx))),
                col=rgb(0.25, 0.25, 0.25, alpha = 0.3), lwd = 5)
        }
        
      }  
    }
    
    data.frame(mass=XIC$mass, t=XIC$t[max.idx], intensity.max = XIC$intensities[max.idx], AUC=AUC)
  }
  
  rawXICAUCData  <- reactive({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("extracting main peak AUC of XICs"))
    on.exit(progress$close())
    
    if (input$source == 'filesystem'){
      
      rf <- unique(file.path(values$filesystemRoot, file.path(input$root, input$rawfile)))
      
      rv <- do.call('rbind', lapply(rf,
                                      function(file){ 
                                        
                                        X <- readXICs(rawfile = file, 
                                                      masses = queryMass()$mZ,
                                                      tol = input$XICtol,
                                                      mono=input$usemono
                                        ) 
                                        
                                        df <- queryMass()
                                        X <- do.call('rbind', lapply(X, extractMainPeak, nmin = input$XICnMinPeaks))
                                        
                                        X$peptide <- df$peptide
                                        X$filename <- basename(file)
                                        X
                                        
                                      }))
      
      return(as.data.frame(rv))
    }else if(input$source == 'bfabric'){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("loading XIC data"))
      on.exit(progress$close())
      
      resources <- bf$resources()$relativepath
      rf <- resources[resources %in% input$relativepath]
      rf <- file.path("/srv/www/htdocs/", rf)
      rv <- plyr::rbind.fill(
        mclapply(rf, function(file){
          X <- readXICs(rawfile = file, 
                        masses = queryMass()$mZ,
                        tol = input$XICtol,
                        mono=input$usemono
          ) 
          
          df <- queryMass()
          X <- do.call('rbind', lapply(X, extractMainPeak, nmin = input$XICnMinPeaks))
          
          X$peptide <- df$peptide
          X$filename <- basename(file)
          X
        }, mc.cores = input$mccores))
      return(rv)
    }else{NULL}
    
  })
  
 
  # ----- rawData -------
  rawData <- eventReactive(input$load, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("loading MS data"))
    on.exit(progress$close())
    
    if (input$source == 'filesystem'){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("loading MS data from local computer"))
      on.exit(progress$close())
      
      rf <- file.path(values$filesystemRoot, file.path(input$root, input$rawfile))
      
      rv <- plyr::rbind.fill(lapply(rf,
                                      function(file){ 
                                        read.raw(file,
                                                 mono=input$usemono, exe=input$cmd) }))
                                    #  mc.cores = 4))
      }else if(input$source == 'package'){
      rv <- NULL
      ne <- new.env()
      data("WU163763", envir=ne)
      rv <- ne[[ls(ne)]]
    }else if(input$source == 'bfabric'){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("loading MS data from bfabric storage"))
      on.exit(progress$close())
      
      resources <- bf$resources()$relativepath
      rf <- resources[resources %in% input$relativepath]
      rf <- file.path("/srv/www/htdocs/", rf)
      rv <- plyr::rbind.fill(
        mclapply(rf, function(file){
          
          read.raw(file = file,
                   mono = TRUE)
        }, mc.cores = 24))
    }else{rv <- NULL}
    
    rv
  })
  
  
PlotXIC <- function(x, method = 'trellis'){
    #x$fmass <- as.factor(x$mass)
    figure <- ggplot(x, aes_string(x = "time", y = "intensity")) +
      #geom_segment() +
      geom_line(stat='identity', size = 1, aes_string(group = "filename", colour = "filename")) +
      
      #scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      #scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "XIC plot") +
      labs(subtitle = "Plotting XIC intensity against retention time") +
      labs(x = "Retention Time [min]", y = "Intensity Counts [arb. unit]") +
      theme_light()
    
    
    if(input$XICmainPeak){
      figure <- figure + facet_wrap(~  x$mass  , scales = "free", ncol = 1) 
    }else{
      figure <- figure + facet_wrap(~  x$mass  , ncol = 1) 
    }
    return(figure)
  }
  
#---- XIC ----
  output$xic <- renderPlot({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "XICs")
    on.exit(progress$close())
    
    
    if (nrow(rawData()) > 0){
      #helpText("graphs scan frequency versus RT or scan frequency marginal distribution for violin."),
      values$gp <- PlotXICs(rawXICData(), method = input$plottype)
      values$gp
    }
  })
  
  # rawDataInfo----
  rawDataInfo <- eventReactive(input$load, {
     progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "loading info data")
    on.exit(progress$close())
 
    return(summary.rawDiag(rawData()))
  })
  
  
  # tic.basepeak ----
  output$tic.basepeak <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "tic.basepeak")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      
      
      values$gp <- PlotTicBasepeak(rawData(), method = input$plottype)
      values$gp
    }
  })
  #---- scan.frequency ----
  output$scan.frequency <- renderPlot({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "scan.frequency")
    on.exit(progress$close())

    
    if (nrow(rawData()) > 0){
      #helpText("graphs scan frequency versus RT or scan frequency marginal distribution for violin."),
      values$gp <- PlotScanFrequency(rawData(), method = input$plottype)
      values$gp
    }
  })
  #---- scan.time ----
  output$scan.time <- renderPlot({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "scan.times")
    on.exit(progress$close())
    if (nrow(rawData()) > 0){
      
      values$gp <- PlotScanTime(rawData(), method = input$plottype)
      values$gp
    }
  })
  
  #---- cycle.load ----
  output$cycle.load <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "cycle.load")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      values$gp <- PlotCycleLoad(rawData(), method = input$plottype)}
      values$gp
  })
  #---- mass.distribution ----
  output$mass.distribution <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "mass.distribution")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      
      values$gp <- PlotMassDistribution(rawData(), method = input$plottype)
      values$gp
    }
  })
  
  #---- lm.correction ----
  output$lm.correction <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "ploting", detail = "lm.correction")
    on.exit(progress$close())
    if (nrow(rawData()) > 0){
      
      values$gp <- PlotLockMassCorrection(rawData(), method = input$plottype)
      values$gp
    }
  })
  
  #---- injection.time ----
  output$injection.time <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "ploting", detail = "injection.time")
      on.exit(progress$close())
      
      values$gp <- PlotInjectionTime(rawData(), method = input$plottype)
      values$gp
    }
  })
  
  #---- mass.heatmap ----
  output$mass.heatmap <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "mass.heatmap")
      on.exit(progress$close())
      
      values$gp <- PlotMassHeatmap(rawData(), bins = input$hexbinsize, method = input$plottype)
      values$gp
      }
  })
  
  #---- precursor.heatmap ----
  output$precursor.heatmap <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "precursor.heatmap")
      on.exit(progress$close())
      
      values$gp <- PlotPrecursorHeatmap(rawData(), bins = input$hexbinsize,method = input$plottype)
      values$gp
    }
  })
  
  #---- cycle.time ----
  output$cycle.time <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "cycle.time")
      on.exit(progress$close())
      
      values$gp <- PlotCycleTime(rawData(), method = input$plottype)
      values$gp
    }
  })
  
  output$charge.state <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "charge.state")
      on.exit(progress$close())
      
      values$gp <- PlotChargeState(rawData(), method = input$plottype)
      values$gp
      
    }
  })
  
  
  #---- XIC ----
  output$xic <- renderPlot({
    if (nrow(rawXICData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "XICs")
      on.exit(progress$close())
      
      values$gp <- PlotXIC(rawXICData(), method = input$plottype)
      values$gp
    }
  })

  output$table<- DT::renderDataTable({
    rawData()
  })
  
  
  output$tableInfo <- DT::renderDataTable({
   rawDataInfo()
  })
  
  output$tableXICAUC <- DT::renderDataTable({
    rawXICAUCData()
  })
  #---- sessionInfo ----
 
  output$sessionInfo <- renderPrint({
    
    capture.output(sessionInfo())
  })
  
  #---- downloadPDF ----
  
  output$PDF <- renderUI({
    if(nrow(rawData()) > 0){
      tagList(
        #h3("XIC"),
        #actionButton('loadXICs', 'load XICs'),
        h3("PDF"),
        downloadButton('foo'))
    }
  })
  
  output$foo = downloadHandler(
    filename = paste("rawDiag.pdf", sep = ''),
    content = function(file) {
      print("YEAH PDF")
      ggsave(values$gp + labs(caption = paste("These plots were generated using the rawDiag R package version", packageVersion('rawDiag'), ". If you are using rawDiag for your work, please cite the following manuscript: C. Trachsel et al. (2018), Journal of Proteome Research doi: 10.1021/acs.jproteome.8b00173", sep = '')),
             file=file,
             dpi = 600,
             device = "pdf",
             width = 500,
             height = input$graphicsheight,
             units = 'mm', limitsize = FALSE)
    }
  )
})

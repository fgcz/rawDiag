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
  values <- reactiveValues(pdfcontent=NULL,
                           filesystemRoot="/scratch/cpanse/",
                           filesystemDataDir = c("WU163230",
                                        "WU163763",
                                        "PXD006932/Exp3A",
                                        "PXD006932/Exp3B",
                                        "PXD006932/Exp6A",
                                        "PXD006932/Exp6B",
                                        "PXD006932/Exp7A",
                                        "PXD006932/Exp7B",
                                        "PXD006932/Exp8A",
                                        "PXD006932/Exp8B",
                                        "PXD006932/SA",
                                        "p195",
                                        "cfortes_20180313_300um_WW"),
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
      tabPanel("Raw table", DT::dataTableOutput("table")),
      tabPanel("Raw info", DT::dataTableOutput("tableInfo")),
      #sessionInfo
      tabPanel("sessionInfo", verbatimTextOutput("sessionInfo"))
      
    )
  })
   
 
  getRawfiles <- reactive({
    message(file.path(values$filesystemRoot, input$root))
    
    f <- list.files(file.path("/scratch/cpanse/",input$root))
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
  
 
  # ----- rawData -------
  rawData <- eventReactive(input$load, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("loading MS data"))
    on.exit(progress$close())
    
    if (input$source == 'filesystem'){
     
      rf <- file.path(values$filesystemRoot, file.path(input$root, input$rawfile))
      
      rv <- plyr::rbind.fill(mclapply(rf,
                                      function(file){ 
                                        read.raw(file, mono=input$usemono, exe=input$cmd) },
                                      mc.cores = input$mccores))}
    else if(input$source == 'package'){
      rv <- NULL
      ne <- new.env()
      data("WU163763", envir=ne)
      rv <- ne[[ls(ne)]]
    }else if(input$source == 'bfabric'){
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = paste("loading MS data"))
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

  output$table<- DT::renderDataTable({
    rawData()
  })
  
  output$tableInfo <- DT::renderDataTable({
   rawDataInfo()
  })
  
  
  #---- sessionInfo ----
 
  output$sessionInfo <- renderPrint({
    
    capture.output(sessionInfo())
  })
  
  
  #---- downloadPDF ----
  
  output$PDF <- renderUI({
    if(nrow(rawData()) > 0){
      tagList(
        h3("PDF"),
        downloadButton('foo'))
    }
  })
  
  output$foo = downloadHandler(
    filename = paste("rawDiag.pdf", sep = ''),
    content = function(file) {
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

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rawDiag)
library(parallel)
#library(magrittr)
library(tidyr)
library(rmarkdown)
library(base64enc)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  values <- reactiveValues(pdfcontent=NULL,
                           DATAROOT="/scratch/cpanse/",
                           datadir = c("WU163230",
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
                                        "cfortes_20180313_300um_WW"))
  
  getRawfiles <- reactive({
    list.files(file.path(values$DATAROOT,input$root))
  })
  
  
  output$tabs <- renderUI({
  
#---- tabsetPanel ----
  tabsetPanel(
    tabPanel("tic.basepeak", plotOutput("tic.basepeak", height = input$graphicsheight)),
    tabPanel("scan.frequency", plotOutput("scan.frequency", height = input$graphicsheight)),
    tabPanel("scan.time", plotOutput("scan.time", height = input$graphicsheight)),
    tabPanel("cycle.load", plotOutput("cycle.load", height = input$graphicsheight)),
    tabPanel("mass.distribution", plotOutput("mass.distribution", height = input$graphicsheight)),
    tabPanel("lm.correction", plotOutput("lm.correction", height = input$graphicsheight)),
    tabPanel("injection.time", plotOutput("injection.time", height = input$graphicsheight)),
    tabPanel("mass.heatmap", plotOutput("mass.heatmap", height = input$graphicsheight)),
     tabPanel("precursor.heatmap", plotOutput("precursor.heatmap", height = input$graphicsheight)),
    tabPanel("cycle.time", plotOutput("cycle.time", height = input$graphicsheight)),
    tabPanel("charge.state", plotOutput("charge.state", height = input$graphicsheight)),
    tabPanel("raw table", DT::dataTableOutput("table")),
    tabPanel("raw info", DT::dataTableOutput("tableInfo")),
    #sessionInfo
    tabPanel("sessionInfo", verbatimTextOutput("sessionInfo"))
  )
  })
  
  
  output$source <- renderUI({
    if (input$source == 'filesystem'){
    
      cmds <- c("~/RiderProjects/fgcz-raw/bin/Debug/fgcz_raw.exe",
                "~cp/bin/fgcz_raw.exe",
                paste(path.package(package = "rawDiag"), "exec/fgcz_raw.exe", sep="/"))
      cmds <- sapply(cmds, function(x){if(file.exists(x)){x}else{NA}})
      cmds <- cmds[!is.na(cmds)]

      inputdir <- sapply(values$datadir, function(x){
        if(file.exists(file.path(values$DATAROOT, x))){x}
        else{NA}})
   
      f <- getRawfiles()
      
      list(
        selectInput('root', 'root:', inputdir, multiple = FALSE),
        selectInput('rawfile', 'rawfile:', f[grep("raw$",f )], multiple = TRUE),
        selectInput('cmd', 'cmd:', cmds, multiple = FALSE),
        checkboxInput("usemono", "Use mono", TRUE),
        sliderInput("mccores", "Cores",
                    min = 1, max = 24,
                    value = 12))
      
    } else if (input$source == 'package') {
      
      RDataFiles <- list.files(file.path(path.package(package = "rawDiag"), "extdata"))
      
      list(
        selectInput('RData', 'RData:',  RDataFiles, multiple = FALSE)
      )
    } else{
      NULL
    }
  })
  
  output$render <- renderUI({
    
    if(nrow(rawData()) > 0){
      actionButton("generatePDF", "pdf")
    }
   
  })
  
  
  output$downloadLinkButton <- renderUI({
  
    if(!is.null(values$pdfcontent)){
      downloadLink('downloadData', 'Download')
    }
    
  })
  
  #---- generateReport ----
  generateReport <- observeEvent(input$generatePDF, {
    
    rawfileQC.parameter <<- list(
      pdf = pdfFileName(),
      resourceid = -1,
      data.QC = rawData(),
      data.Info = rawDataInfo()
    )
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "render", detail = "PDF ...")
    on.exit(progress$close())
    
    message(tempdir())
    workdir <- tempdir()
    
    rmdfile2run <- file.path(workdir, "rawfileQC.Rmd")
    
    if(!dir.create(workdir)){
      stopApp(7)
    }
    
    
    if(!file.copy(file.path(path.package("rawfileQC"), "/report/rawfileQC.Rmd") , rmdfile2run)){
      stopApp(7)
    }
    
    rmarkdown::render(rmdfile2run, output_format ="pdf_document")
    
    pdffilename <- file.path(workdir, "rawfileQC.pdf")
    
    
    progress$set(message = "render", detail = "PDF base64encode ...")
    if(file.exists(pdffilename)){
      
      values$pdfcontent <- base64encode(readBin(pdffilename, "raw",
                           file.info(pdffilename)[1, "size"]), "pdf")
      message(file.info(pdffilename)[1, "size"])
    }
    
    message(tempdir())
  })
  
  
  output$pdf  = downloadHandler(
    filename = "fgcz_rawfileDiagnostic.pdf",
    content = values$pdfcontent
  )
    
  save <- observeEvent(input$save, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "saving ...")
    on.exit(progress$close())
    raw <- rawData()
    fn <- tempfile(fileext = ".RData")
    progress$set(detail = fn)
    save(raw, file = fn)
    message(fn)
    
  })
  
  pdfFileName <- reactive({tempfile(fileext = ".pdf")})
  
  rawData <- eventReactive(input$load, {
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = paste("loading MS data"))
    on.exit(progress$close())
    
    if (input$source == 'filesystem'){
      rf <- file.path(values$DATAROOT, file.path(input$root, input$rawfile))
      rv <- plyr::rbind.fill(mclapply(rf,
                                      function(file){ 
                                        read.raw(file, mono=input$usemono, exe=input$cmd) },
                                      mc.cores = input$mccores))}
    else if(input$source == 'package'){
      rv <- NULL
      fn <- file.path(file.path(path.package(package = "rawDiag"), "extdata"),
                      input$RData)
      ne <- new.env()
      load(fn, ne)
      rv <- ne[[ls(ne)]]
    }else{rv <- NULL}
    
    rv
  })
  
  # rawDataInfo----
  rawDataInfo <- eventReactive(input$load, {
     progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "loading info data")
    on.exit(progress$close())
    
    rf <- file.path(input$root, input$rawfile)
    
    df <- plyr::rbind.fill(mclapply(rf, function(x){
      
      cmd <- paste(input$cmd, x, "info") 
      
      if (input$usemono){
        cmd <- paste("mono", cmd)
      }
      
      message(paste("executing", cmd, "..."))
      
      info <- scan(pipe(cmd), what = character(), sep="\n")
      
      info <- gsub("^\ +", "", info)
      idx <- which(grepl("filename|Instrument name|Number of scans|Time range|Mass range|Number of ms2 scans|Software version|RAW file version", info))
      
      info <- info[idx]
      info <- do.call('rbind', strsplit(info, split = ": "))
      info <- as.data.frame(info)
      names(info) <- c('attribute', 'value')
       
      info$filename <- basename(x)
      info
    }, mc.cores = input$mccores))
    
    df <- (df %>% spread(attribute, value))
    return(df)
  })
  
  
  # tic.basepeak ----
  output$tic.basepeak <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "tic.basepeak")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      
      
      PlotTicBasepeak(rawData(), method = input$plottype)
      
    }
  })
  #---- scan.frequency ----
  output$scan.frequency <- renderPlot({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "scan.frequency")
    on.exit(progress$close())
    if (nrow(rawData()) > 0){
      
      PlotScanFrequency(rawData(), method = input$plottype)
    }
  })
  #---- scan.time ----
  output$scan.time <- renderPlot({
    
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "scan.times")
    on.exit(progress$close())
    if (nrow(rawData()) > 0){
      
      PlotScanTime(rawData(), method = input$plottype)
    }
  })
  
  #---- cycle.load ----
  output$cycle.load <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "cycle.load")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      PlotCycleLoad(rawData(), method = input$plottype)}
  })
  #---- mass.distribution ----
  output$mass.distribution <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "plotting", detail = "mass.distribution")
    on.exit(progress$close())
    
    if (nrow(rawData()) > 0){
      
      PlotMassDistribution(rawData(), method = input$plottype)
      
    }
  })
  
  #---- lm.correction ----
  output$lm.correction <- renderPlot({
    progress <- shiny::Progress$new(session = session, min = 0, max = 1)
    progress$set(message = "ploting", detail = "lm.correction")
    on.exit(progress$close())
    if (nrow(rawData()) > 0){
      
      PlotLockMassCorrection(rawData(), method = input$plottype)
      
    }
  })
  
  #---- injection.time ----
  output$injection.time <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "ploting", detail = "injection.time")
      on.exit(progress$close())
      
      PlotInjectionTime(rawData(), method = input$plottype)

    }
  })
  
  #---- mass.heatmap ----
  output$mass.heatmap <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "mass.heatmap")
      on.exit(progress$close())
      
      PlotMassHeatmap(rawData())
    }
  })
  
  #---- precursor.heatmap ----
  output$precursor.heatmap <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "precursor.heatmap")
      on.exit(progress$close())
      
      PlotPrecursorHeatmap(rawData())
    }
  })
  
  #---- cycle.time ----
  output$cycle.time <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "cycle.time")
      on.exit(progress$close())
      
      PlotCycleTime(rawData(), method = input$plottype)
      
    }
  })
  
  output$charge.state <- renderPlot({
    if (nrow(rawData()) > 0){
      
      progress <- shiny::Progress$new(session = session, min = 0, max = 1)
      progress$set(message = "plotting", detail = "charge.state")
      on.exit(progress$close())
      
      PlotChargeState(rawData(), method = input$plottype)
      
      
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
  
  output$downloadData <- downloadHandler(
    filename = "rawfileQC.pdf",
    content = function(file) {
      file.copy(values$pdfcontent, file)
    }
  )
  
})

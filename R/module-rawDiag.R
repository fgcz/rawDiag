#R

#' Run the rawDiag shiny application 
#'
#' @inheritParams shiny::runApp
#' @param rawDir A directory containing the input raw files,
#' default is set to the \code{$HOME/Downloads} directory.
#' @param \ldots passed to the \code{\link{runApp}} method.
#' @importFrom shiny runApp
#' @export
#' @examples
#' \dontrun{
#' rawDiag::shiny(launch.browser = TRUE, display.mode = 'normal')
#' 
#' ## embracing the command line
#' 
#' # MacOSX and Linux
#' R -q -e "rawDiag::shiny(launch.browser = TRUE)"
#' 
#' # Microsoft Windows
#' R.exe -e "rawDiag::shiny(launch.browser = TRUE)"
#' }
shiny <- function(appDir = system.file('shiny', package = 'rawDiag'), 
                  rawDir = (Sys.getenv('HOME') |>
                              file.path("Downloads")),
                  ...){
  
  rawDir |>
    file.path(rawDir |>
                list.files(recursive = TRUE,
                           pattern = "*.raw$")) -> files
  
  sapply(files, FUN=file.mtime) |> order() -> idx
  files[rev(idx)] ->> files
  
  shiny::runApp(appDir,  ...)
}

#' rawDiag shiny module UI
#' @return a shiny UI module
#' @inheritParams shiny::moduleServer
rawDiagUI <- function(id){
  ns <- NS(id)
  
  plotFunctions <- ls("package:rawDiag")[ls("package:rawDiag") |> grepl(pattern = "^plot")]
  tagList(
    fluidRow(a(img(src="https://img.shields.io/badge/JPR-10.1021%2Facs.jproteome.8b00173-brightgreen"),
               href='http://dx.doi.org/10.1021/acs.jproteome.8b00173')),
    fluidRow(
      column(width = 4,
             selectInput(ns("plotFUN"), "function", choices = plotFunctions,
                         selected = plotFunctions[1], multiple = FALSE)),
      column(width = 4,
             selectInput(ns("plotArg"), "argument", choices = c("trellis", "violin", "overlay"),
                         selected = "trellis", multiple = FALSE),
      ),
      column(width = 3,
             checkboxInput(ns('useParallel'), 'Use parallel processing (mclapply)', value = TRUE)
      )),
   
    fluidRow(plotOutput(ns("plot")))
  )
}

#' rawDiag shiny module
#'
#' @inheritParams shiny::moduleServer
#' @param vals containing rawfile
#' @return shiny module server
#' @importFrom shiny moduleServer reactive reactiveValues observeEvent renderPlot req NS tagList selectInput checkboxInput plotOutput debounce
rawDiagServer <- function(id, vals){
  moduleServer(id,
               function(input, output, session) {
                 rawfile <- reactive({ vals$rawfile }) |>
                   debounce(2000)
                 data <- reactive({
                   shiny::req(rawfile())
                   
                   progress <- shiny::Progress$new(session = session)
                   progress$set(message = "Reading Index ...")
                   on.exit(progress$close())
                   
                   if (input$useParallel & parallel::detectCores() > 1 & length(rawfile()) > 1){
                     progress$set(message = paste0("Reading ", length(rawfile()), " files ..."),
                                  detail = "using parallel::mclapply")
                     
                     parallel::mclapply(rawfile(),FUN = rawDiag::read.raw, mc.cores = parallel::detectCores()) |>
                       Reduce(f = rbind)
                   }else{
                     lapply(rawfile(), function(f){
                       rawDiag::read.raw(f,
                                         msgFUN = function(msg){
                                           progress$set(detail = paste0("Reading ", basename(f)),
                                                        message = msg)
                                         })
                     }) |>
                       Reduce(f = rbind)
                   }
                 })
                 
                 
                 observeEvent(input$plotFUN, {message(input$plotFUN)})
                 
                 
                 dynamicHeight <- reactive({
                   if (input$plotArg == "trellis"){
                     400 * length(rawfile())
                   }else{400}
                 })
                 
                 output$plot <- renderPlot({
                   shiny::req(data(), input$plotFUN, input$plotArg)
                   progress <- shiny::Progress$new(session = session)
                   progress$set(message = "plotting ...")
                   on.exit(progress$close())
                   
                   
                   do.call(what = input$plotFUN,
                           args = list(x = data(),
                                       method = input$plotArg))
                 }, height = function()dynamicHeight(), width = 800)
               }
  )
}

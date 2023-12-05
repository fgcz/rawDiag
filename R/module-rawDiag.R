#R

#stopifnot(require(rawDiagB))

#' rawDiag shiny module UI
#'
#' @param id  session id

#' @export
rawDiagUI <- function(id){
  ns <- NS(id)
  
  plotFunctions <- ls("package:rawDiagB")[ls("package:rawDiagB")|>grepl(pattern = "^plot")]
  tagList(
    selectInput(ns("plotFUN"), "rawDiag plot function", choices = plotFunctions,
                selected = plotFunctions, multiple = FALSE),
    selectInput(ns("plotArg"), "rawDiag plot argument", choices = c("trellis", "violin", "overlay"),
                selected = "trellis", multiple = FALSE),
    checkboxInput(ns('useParallel'), 'Use parallel processing (mclapply)', value = TRUE),
    plotOutput(ns("plot"))
  )
}

#' rawDiag shiny module
#'
#' @param id shiny session id
#' @param vals containing rawfile
#'
#' @export
#' @importFrom shiny moduleServer 
rawDiagServer <- function(id, vals){
  
  moduleServer(id,
               function(input, output, session) {
                 rawfile <- reactive({ vals$rawfile }) |> debounce(2000)
                 data <- reactive({
                   shiny::req(rawfile())
                   
                   progress <- shiny::Progress$new(session = session)
                   on.exit(progress$close())
                   
                   if (input$useParallel & parallel::detectCores() > 1 & length(rawfile()) > 1){
                     progress$set(message = paste0("Reading ", length(rawfile()), " files ..."),
                                  detail = "using parallel::mclapply")
                     
                     parallel::mclapply(rawfile(),FUN = rawDiagB::read.raw, mc.cores = parallel::detectCores()) |>
                       Reduce(f = rbind)
                   }else{
                     lapply(rawfile(), function(f){
                       rawDiagB::read.raw(f,
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
                   do.call(what = input$plotFUN,
                           args = list(x = data(),
                                       method = input$plotArg))
                 }, height = function()dynamicHeight(), width = 800)
               }
  )
}

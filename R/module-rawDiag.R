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
                     rawfile <- reactive({ vals$rawfile })
                     data <- reactive({
                         shiny::req(rawfile())
                         
                         parallel::mclapply(rawfile(), function(f){
                             progress <- shiny::Progress$new(session = session)
                             progress$set(message = paste0("Reading", basename(f), "..."))
                             on.exit(progress$close())
                             
                             rawDiagB::read.raw(f,
                                                msgFUN = function(msg){
                                                    progress$set(detail = msg)
                                                })
                         }) |>
                             Reduce(f = rbind)
                     })
                     
                     
                     observeEvent(input$plotFUN, {message(input$plotFUN)})
                     
                     output$plot <- renderPlot({
                         shiny::req(data(), input$plotFUN, input$plotArg)
                         do.call(what = input$plotFUN,
                                 args = list(x = data(),
                                             method = input$plotArg))
                     })
                 }
    )
}

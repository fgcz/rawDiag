#R

stopifnot(require(rawDiagB))

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
rawDiagUI <- function(id){
    ns <- NS(id)
    
    plotFunctions <- ls("package:rawDiagB")[ls("package:rawDiagB")|>grepl(pattern = "^plot")]
    tagList(
        selectInput(ns("plotFUN"), "plotFUN", choices = plotFunctions,
                    selected = plotFunctions, multiple = FALSE),
        plotOutput(ns("plot"))
    )
}

#' rawDiag shiny module
#'
#' @param id shiny session id
#' @param vals containing rawfile
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom shiny moduleServer 
rawDiagServer <- function(id, vals){
    
    moduleServer(id,
                 function(input, output, session) {
                     rawfile <- reactive({ vals$rawfile })
                     data <- reactive({
                         shiny::req(rawfile())
                         progress <- shiny::Progress$new(session = session)
                         progress$set(message = paste("Reading", vals$rawfile) )
                         on.exit(progress$close())
                         
                         rawDiagB::read.raw(rawfile())
                     })
                     
                     observeEvent(input$plotFUN, {message(input$plotFUN)})
                     
                     output$plot <- renderPlot({
                         shiny::req(data(), input$plotFUN)
                        
                         do.call(what = input$plotFUN, args = list(x = data()))
                     })
                 }
    )
}

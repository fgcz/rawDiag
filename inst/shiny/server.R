#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
stopifnot(require(rawDiag))

# Define server logic required to draw a histogram
function(input, output, session) {
    
    vals <- reactiveValues(rawfile  = NA)
    
    observeEvent(input$rawfile, {vals$rawfile = input$rawfile; message(vals$rawfile )})
    rawDiag:::rawDiagServer("test", vals)

}

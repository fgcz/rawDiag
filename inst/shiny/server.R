#R 

stopifnot(require(rawDiag))

# Define server logic required to draw a histogram
function(input, output, session) {
    
    vals <- reactiveValues(rawfile  = NA, gp = NULL, pdfFileName = NULL, plot = NULL)
    
    observeEvent(input$rawfile, {
        vals$rawfile = input$rawfile;
        message(vals$rawfile )
    })
    rawDiag:::rawDiagServer("OrbitrapFun02", vals)
    
    output$downloadPdf <- downloadHandler(
        filename = function() {
            basename(vals$pdfFilename)
        },
        content = function(file) {
            file.copy(
                from = vals$pdfFilename,
                to = file
            )
        }
    )
    
}

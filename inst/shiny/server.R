#R 

stopifnot(require(rawDiag))

# Define server logic required to draw a histogram
function(input, output, session) {
    
    vals <- reactiveValues(rawfile  = NA, gp = NULL, pdfFileName = NULL, plot = NULL, generatePDF = 0)
    
    observeEvent(input$rawfile, {
        
        vals$rawfile = input$rawfile;
        message(vals$rawfile )
    })
    
    observeEvent(input$generatePDF, {
        vals$generatePDF <- vals$generatePDF + 1
    })
    rawDiag::rawDiagServer("OrbitrapFun02", vals)
    
    output$downloadPdf <- downloadHandler(
        filename = function() {
            basename(vals$pdfFileName)
        },
        content = function(file) {
            file.copy(
                from = vals$pdfFileName,
                to = file
            )
        }
    )
}

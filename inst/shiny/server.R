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
    
    vals <- reactiveValues(rawfile  = NA, gp = NULL, pdfFileName = NULL, plot = NULL)
    
    observeEvent(input$rawfile, {vals$rawfile = input$rawfile; message(vals$rawfile )})
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
    
    observeEvent(vals$gp, {
        pdfFilename <- tempdir() |>
            file.path(paste0("rawDiag",
                             format(Sys.time(), "_%Y%m%d-%H%M%S_"),
                             vals$plot, '.pdf'))
        
        msg <- paste0("ggplot2::ggsave to file ", pdfFilename)
        message(msg)
        progress <- shiny::Progress$new(session = session, min = 0, max = 1)
        progress$set(message = msg)
        on.exit(progress$close())
        
        gglabs <- ggplot2::labs(
            caption = paste0("Generated on ", 
                             format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                             ", @", Sys.info()["nodename"],
                             " using the rawrr/rawDiag R package versions ", 
                             packageVersion('rawrr'), "/",
                             packageVersion('rawDiag'), " ",
                             R.Version()['version.string'],
                             ". If you want to cite rawrr and rawDiag in your work, use:\n",
                             "T. Kockmann and C. Panse (2021), Journal of Proteome Research doi: 10.1021/acs.jproteome.0c00866; ",
                             "C. Trachsel et al. (2018), Journal of Proteome Research doi: 10.1021/acs.jproteome.8b00173"))
        
        rvgg <- ggplot2::ggsave(
            vals$gp + gglabs,
            filename = pdfFilename,
            dpi = 600,
            device = "pdf",
            width = 297,
            height = 210,
            units = 'mm',
            limitsize = FALSE)
        
        if (file.exists(pdfFilename)){
            vals$pdfFilename <- pdfFilename
        } else{
            vals$pdfFilename <- NULL
        }
    })
}

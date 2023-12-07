#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

stopifnot(require(rawDiag))

if (isFALSE(exists('files'))){
  rawrr::sampleFilePath() ->> files
}

# Define UI for application that draws a histogram
fluidPage(
    
    # Application title
    titlePanel("rawDiag - Brings Orbitrap Mass Spectrometry Data to Life; Fast and Colorful"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tagList(
                # html::img(src='https://github.com/cpanse/rawDiag/blob/main/vignettes/octopussy.png', height = 100),
              selectInput("rawfile", "rawfile",
                            choices = files,
                            selected = files[1], width = "100%", multiple = TRUE),
                downloadButton("downloadPdf", "Download")
            )
        ),
        
        mainPanel(
            rawDiag:::rawDiagUI("OrbitrapFun02")
        )
    )
)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

stopifnot(require(rawDiag))

#rootdir <- file.path(Sys.getenv('HOME'), "/Downloads/20230906/20220915/20220830/autoQC4L/LUMOS_2/")
#files <- file.path(rootdir, list.files(rootdir, pattern = '*.raw'))

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
        
        # Show a plot of the generated distribution
        mainPanel(
            rawDiag:::rawDiagUI("OrbitrapFun02")
            #plotOutput("distPlot")
        )
    )
)

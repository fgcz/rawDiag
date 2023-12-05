#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

rootdir <- "/Users/cp/data/"
files <- file.path(rootdir, list.files(rootdir, pattern = '*.raw'))

# Define UI for application that draws a histogram
fluidPage(
    
    # Application title
    titlePanel("Brings Orbitrap Mass Spectrometry Data to Life; Fast and Colorful"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tagList(
                selectInput("rawfile", "rawrfile",
                            choices = files,
                            selected = files[1], width = "100%", multiple = TRUE),
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            rawDiagUI("test")
            #plotOutput("distPlot")
        )
    )
)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(bfabricShiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(paste("rawDiag - Diagnostic Plots for Mass Spectrometry Data", "version", packageVersion('rawDiag'))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel( 
      
      img(src='octopussy.png ', align = "right"),
      radioButtons("source", "Type of data source:",
                   c("package" = "package",
                     "filesystem" = "filesystem",
                     "bfabric" = "bfabric")),
      br(),
      
      hr(),
      htmlOutput("source"),
      htmlOutput("sourceBfabric"),
      hr(),
      
      sliderInput("graphicsheight", "graphicsheight",
                  min = 480, max = 4096,
                  value = 512),
      sliderInput("hexbinsize", "hexbinsize", min = 1, max = 512, value = 80),
      radioButtons("plottype", "Type of diagnostic plot:",
                   c("overlay" = "overlay",
		     "trellis" = "trellis",
                     "violin" = "violin")),
      
      #actionButton("save", "save"),
      # htmlOutput("render"),
      htmlOutput("downloadLinkButton")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tagList(
        htmlOutput("tabs"),
        hr(),
        downloadButton('foo'))
    )
  )
))
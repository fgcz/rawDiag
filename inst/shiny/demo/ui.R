#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(paste("rawDiag-demo", "version", packageVersion('rawDiag'))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Type of data source:",
                   c("package" = "package",
                     "filesystem" = "filesystem",
                     "bfabric" = "bfabric")),
      hr(),
      htmlOutput("source"),
      hr(),
      sliderInput("graphicsheight", "graphicsheight",
                  min = 480, max = 4096,
                  value = 512),
      radioButtons("plottype", "Type of diagnostic plot:",
                   c("trellis" = "trellis",
                     "violin" = "violin",
                     "overlay" = "overlay")),
       actionButton("load", "load"),
      actionButton("save", "save"),
      # htmlOutput("render"),
      htmlOutput("downloadLinkButton")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("tabs")
    )
  )
))

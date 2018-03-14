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
  titlePanel("rawfileQC"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      htmlOutput("cmd"),
      htmlOutput("root"),
       htmlOutput("rawfile"),
      checkboxInput("usemono", "Use mono", TRUE),
      sliderInput("mccores", "Cores",
                  min = 1, max = 24,
                  value = 12),
      sliderInput("graphicsheight", "graphicsheight",
                  min = 480, max = 4096,
                  value = 512),
      radioButtons("plottype", "Type of diagnostic plot:",
                   c("trellis" = "opt1",
                     "violin" = "opt2",
                     "overlay" = "opt3")),
       actionButton("load", "load"),
      actionButton("save", "save"),
       htmlOutput("render"),
      htmlOutput("downloadLinkButton")
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("tabs")
    )
  )
))

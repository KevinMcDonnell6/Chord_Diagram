library(shiny)
library(plotly)
library(chorddiag)
library(DT)
library(sunburstR)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Circos Example"),
  
  # Sidebar with a slider input for zoom and level of taxa 
  sidebarLayout(
    sidebarPanel(
      width = 2,
      sliderInput("margin", "Margin",  min = 0, max = 200, value = 200),
      DTOutput("tbl")
    ),
    
    # Show a plot of the Circos
    mainPanel(
      chorddiagOutput("distPlot",width="850px",height="850px")#,
      
    )
  ),

  
  fluidRow(
    
    # create table of functions
    column(3,
           DTOutput("tbl2")
           
           
    ),
    
    # create sunburst
    column(4,
           sunburstOutput("sunburst"))
    
  )
  
))

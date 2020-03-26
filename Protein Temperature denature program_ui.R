library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Fit CD temperature-denaturation data test"),
  
  sidebarPanel(
    
    fileInput('file','Choose CD data file to analyze (CSV format)',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/plain',
                '.csv')),
    radioButtons('temp.units','Input temperatures are in:',c('celsius','kelvin'),inline=TRUE)
    
  ),
  
  mainPanel(
    plotOutput('plot'),
    conditionalPanel(
      "output.fileUploaded",
      verbatimTextOutput('text')
    )
  )
))

library(shiny)
library(XLConnect)
library(ggplot2)


shinyUI(fluidPage(
  titlePanel("LCMS_Project_3.0"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file','Upload the file (Excel format), set the parameter, and you will get graphs for analysis.',
                accept = c('text/xls','.xls')),
      textInput("textA", label = "Filtering Criteria 1", value = NULL, placeholder="keyword..."),
      textInput("textB", label = "Filtering Criteria 2", value = NULL,placeholder="keyword..."),
      textInput("textC", label = "Filtering Criteria 3", value = NULL, placeholder="keyword..."),
      sliderInput("sliderB", label="X Axis (time)", min = 0, max= 30, value=7),
      sliderInput("sliderC", label="Y Axis (intensity)", min = 0, max= 55000, value=1500),
      actionButton("actionA", label = "Start")
    ),
    mainPanel(
      actionButton("actionC", label = "Previous Page"),
      actionButton("actionB", label = "Next Page"),
      plotOutput('plot'),
      
      textOutput("dataNumber"),
      textOutput("check2"),
      textOutput("check3"),
      textOutput("page"),
      textOutput("check4"),
      textOutput("check5"),
      textOutput("check6")
      
      
    )
  )
))


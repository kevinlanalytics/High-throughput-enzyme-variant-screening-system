# convert between celsius (input data, analysis) and
# kelvin (used for thermodynamics calculations)
kelvin.factor <- 273.15
celsius.to.kelvin <- function(data) {
  return(data + kelvin.factor)
}
kelvin.to.celsius <- function(data) {
  return(data - kelvin.factor)
}

# kcal/mol/K
R <- 1.9872e-3
# we model the data using this function
gibbs.helmholtz <- function(params,T) {
  dG <- params$dH * (1 - T/params$Tm)
  K <- exp(-dG/(R*T))
  alpha <- K/(1+K)
  sigma.t <- alpha*(params$sigma.f - params$sigma.u) + params$sigma.u
  return(sigma.t)
}

# initial parameters
paramsStart <- list(dH=-50.0,Tm=320.0,sigma.f=-35.0,sigma.u=-15.0)

shinyServer(function(input, output) {
  output$fileUploaded <- reactive({return(!is.null(input$file))})
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  output$plot <- renderPlot({
    if (is.null(input$file))
      return(NULL)
    # read data
    data <- read.csv(input$file$datapath, header=TRUE)
    temperature <- data[,1]
    cd.signal <- data[,2]
    # convert celsius temps to kelvin
    if (input$temp.units == 'celsius')
      temperature <- celsius.to.kelvin(temperature)
    
    # used to compute the difference between the real data and the model given a set of model parameters
    residFun <- function(params) cd.signal - gibbs.helmholtz(params,temperature)
    
    #perform fit
    nls.out <- nls.lm(fn=residFun, par=paramsStart)
    # store the optimized parameters
    paramsOpt <- as.list(coef(nls.out))
    # print out information about the fit
    output$text <- renderPrint({
      print(summary(nls.out))
      cat('\nfitted Tm (degrees C) is',formatC(kelvin.to.celsius(paramsOpt$Tm),digits=4),'\n')
    })
    plot(temperature,cd.signal)
    lines(temperature,gibbs.helmholtz(paramsOpt,temperature),col='red')
  })
})


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
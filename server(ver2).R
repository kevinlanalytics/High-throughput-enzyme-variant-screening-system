library(reshape2)
library(XLConnect)
library(ggplot2)
library(plyr)
library(shiny)
library(minpack.lm)
library(Peaks)
library(grid)
library(httpuv)
library.dynam('Peaks', 'Peaks', lib.loc=NULL) 



# Using the file uploaded to ui.r
shinyServer(function(input, output) {
  output$fileUploaded <- reactive({return(!is.null(input$file))})
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  
  
  output$plot <- renderPlot({
    if (is.null(input$file))
      return(NULL)

wb1 <- loadWorkbook(input$file$datapath)
sheets1 <- readWorksheet(wb1, sheet=getSheets(wb1))
data1 <- readWorksheet(wb1, getSheets(wb1), startRow=2, startCol=2, endCol=3)
header <- readWorksheet(wb1, sheet = getSheets(wb1), startRow = 1, startCol = 1,endRow = 1, endCol = 1,header=FALSE)


# Making the filter function 

list1 <- grep(input$textA, header)

list2 <- grep(input$textB, header)

list3 <- grep(input$textC, header)

list3 <- intersect(list3, intersect(list1, list2))

output$dataNumber <- renderPrint({cat("number of data:", length(list3))})


#Loop for the Action button
w = input$actionA-1
q = input$actionA

t= abs(q-w)
if( q!=0 && t==1 ){

# funtion that can apply SpectrumSearch to all data in list3  
peak<-lapply(data1[list3], function(z){
  result <- SpectrumSearch(z[,2], sigma=input$sliderA, threshold=10.0, 
                           background = FALSE, iterations=13, markov=FALSE, window=3)
})


# Setting up the parameter of the for loop
x= abs(input$actionB-input$actionC) 
output$page <- renderPrint({cat("page",x)})

start<- 1+4*x 

end<- 4+4*x 
output$check2 <- renderPrint({cat("Data index:", start, "-", end)})
end<- min(end, length(list3))

peaklist<-list()

plots<-list()

for(i in start: end){
  
    # For SpectrumSearch data processing
    peaky <- data1[[list3[i]]]$Y[peak[[i]][[1]]]
    
    peakx <- data1[[list3[i]]]$X[peak[[i]][[1]]]
  
    peaklist[[i]] <- data.frame(peakx, peaky) 
  
    # plotting with ggplot2
    p<- ggplot(data=data1[[list3[i]]], aes(x=data1[[list3[i]]]$X, y=data1[[list3[i]]]$Y))
    p<-p + geom_line(color='Black')
    p<-p + xlab("Retention Time-(min)") + ylab("Intensity-(counts)") 
    p<-p + ggtitle(header[[list3[i]]][1,])
    p<-p + coord_cartesian(xlim=c(0,input$sliderB), ylim=c(0,input$sliderC))
    p<-p + geom_point(data=peaklist[[i]], aes(x=peakx, y=peaky), color='Red')

    plots[[i]] <- p

  }



# Loop for the ViewPort
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
vp <- list(viewport(layout.pos.row = 1, layout.pos.col = 1), viewport(layout.pos.row = 1, layout.pos.col = 2), viewport(layout.pos.row = 2, layout.pos.col = 1), viewport(layout.pos.row = 2, layout.pos.col = 2))





for (i in start:end) {
 
  print(plots[[i]],vp=vp[[i-4*x]])

} 

}else(stop()) # The end action button loop 
  })
})
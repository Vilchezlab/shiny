# HeatMap
# Define UI for dataset
library(shiny)
library(pheatmap)
  ui = fluidPage("Main",
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           accept = c( "csv","commaSeparatedValues,plain",".csv")
                 ), 
                 
                 checkboxInput("header", "Header", TRUE),
                 # Input: Number of observations to view 
                 numericInput("obs", "Number of observations to view:", 6),
                 actionButton("update", "Update View")
                ),
               tabPanel('map', 
                        sidebarLayout(
                          sidebarPanel(actionButton('getHmap', 'get Heatmap')
                          ),
                          mainPanel('Heat Map',
                                    plotOutput("map"),
                                    #tableOutput
                                    h4("Observations"),
                                    tableOutput("view")
                          )
                        ))
  )

server = function(input, output, session) {
 hm <- eventReactive(input$update,{
      inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    mylist <- read.csv(inFile$datapath, header=input$header) 
    return(mylist)
  })
    output$view <- renderTable({
    head(hm(), n = isolate(input$obs))
  })
  
  plotdata <- eventReactive(input$getHmap, {
    hm <- as.matrix(hm()[-1])
    row.names(hm) <- hm()$Name
    hm[is.na(hm)] <- 0
    hm
  })
  
  output$map = renderPlot({ 
    pheatmap(plotdata())
  })
}

# Create Shiny app ----
shinyApp(ui, server, options = list(height = 600))

#library(rsconnect)
#deployApp()
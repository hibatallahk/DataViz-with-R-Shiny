library(shiny)
library(ggplot2)
library(factoextra)
library(cluster)
library("FactoMineR")
library(dplyr)
library(corrplot)

ui <- fluidPage(
  titlePanel("kabbaj hibatallah"),
  sidebarLayout(
    sidebarPanel(
      fileInput("browser", "choose your file",
                accept = "txt/csv/sav",
                placeholder = "No file selected",
                buttonLabel = "browse"),

      uiOutput("ix"),
      uiOutput("iy"),
      numericInput("cluster1", "numbre of clusters", value = 2,min = 2, max = 49),
      
      radioButtons("radioG", "statistics",
                   choices = c("summarize",
                               "head",
                               "str")
                   )
   
      
    ),
    mainPanel(
      ui<- fluidPage(
          tabsetPanel(
            tabPanel("Statistics",verbatimTextOutput("res_sta"),tableOutput("res_sta1")),
            tabPanel("Scatter plot", plotOutput("scatter") ),
            tabPanel("PCA", plotOutput("mat_corr"),verbatimTextOutput("val_prp")),
            tabPanel("K_means",plotOutput("kmeans"))

          )
      )
       
    )
  )
  
  
  
)

server <- function(input, output, session) {
  data<- reactive({
    file1<-input$browser
    if (is.null(file1)) {return()}
    read.csv(file1$datapath, stringsAsFactors = F)
    
  })
  
  output$ix <-renderUI({selectInput("x","x",colnames(data()) )})
  
  output$iy <-renderUI({selectInput("y","y",colnames(data()) )})
  
  
  output$res_sta <- renderPrint({ 
    
    if(input$radioG=="summarize"){ summary(data())} 
    
   
    
    else if (input$radioG=="str") { str(data()) }

    })

  
  output$res_sta1 <- renderTable({
    
    if (input$radioG=="head") { head(data()) }
    
  })
  
  
  output$scatter <- renderPlot({ ggplot(data(), 
                                        aes(data()[input$x],data()[input$y])) +
                                          geom_point()
  
  })
  
  
  clusters <- reactive({
    kmeans(data1(), input$cluster1)
  })
  
  data1<- reactive({
    data()[,c(input$x,input$y)]
    
  })
  
  
  output$kmeans <- renderPlot({ 
    
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(data1(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  
 
  
  data1<- reactive({
    data()[,c(input$x,input$y)]
    
  })
  


  res.pca<-reactive({
    PCA(select_if(data(),is.numeric),scale.unit = T, graph = F )
    
  })  
 output$mat_corr <- renderPlot({
   
   var<- get_pca_var(res.pca())
   corrplot(var$cos2, is.corr = F)
 })

 
 
 output$val_prp <- renderPrint({
   
   get_eigenvalue(res.pca())
 })
 

 
}

shinyApp(ui, server)
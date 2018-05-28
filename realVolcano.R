## Only run examples in interactive R sessions
library(shiny)
library(openxlsx)
library(ggplot2)
library(ggrepel)
#library(data.table)


if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        
        #dropdown menu to select columns
        uiOutput("droplistx"),
        uiOutput("droplisty"),
        textInput(inputId="title", label="Title", value = "", width = NULL, placeholder = NULL),
        textInput(inputId="pcut", label="-log10(p-value) cutoff (>)", value = 10, width = NULL, placeholder = 10),
        textInput(inputId="fccut", label="foldchange cutoff (>)", value = 10, width = NULL, placeholder = 10),
        uiOutput("droplistlab"),
        downloadButton('filename', "Download")
        
      ),
      mainPanel(plotOutput(outputId = "main_plot", height = "300px"))
      
    ),
    #for running tests
    textOutput("test"),
    
    
    dataTableOutput(outputId = "contents")
    
    
  )
  
  
  server <- function(input, output) {
    
    #initial data processing
    options(shiny.maxRequestSize=30*1024^2)
    inFile <- reactive(input$file1)
    FileData<- reactive({
      if(is.null(inFile()))
        return(NULL)
      read.xlsx(xlsxFile = inFile()$datapath,sheet = 1)

      })
    
    ##Dropdown Menus
    clist <- reactive({
      if (is.null(inFile()))
        NULL
      else
        colnames(FileData())
    })
    
    output$droplistx <- renderUI({
      
      
      selectInput(inputId="fccol", label="select foldchange column", choices=clist(), selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    })
    
    output$droplisty <- renderUI({
      
      
      selectInput(inputId="pcol", label="select p-value column", choices=clist(), selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    })
    output$droplistlab <- renderUI({
      
      
      selectInput(inputId="labelcol", label="select column for datapoint labels", choices=clist(), selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL)
    })
    
    
    
    #process data      
    Data<- reactive({
      if(is.null(clist()))
        return(NULL)
      df<-read.xlsx(xlsxFile = inFile()$datapath,sheet = 1)
      df['negativelog10p']<- -log10(df[input$pcol])
      nameColumn <- input$labelcol
      pCutoff <- input$pcut
      pCol <- input$pcol
      df[df$negativelog10p < 10, nameColumn] <- ''
      df
    })
    
    
    fc<-reactive({input$fccol})
    p<-reactive({input$pcol})
    title<-reactive({input$title})
    labelcol<-reactive({input$labelcol})
    #fccut<-reactive({input$fccut})
    #pcut<-reactive({input$pcut})
    ylabel<- reactive({
      if (is.null(inFile()))
        NULL
      paste0("-log10(",p(),")")})
    
    

    #logp <-reactive(-log10(FileData()[[p()]]))
   # Data<- reactive({
    #  if(is.null(FileData()))
     #   return(NULL)

      #df<-FileData()
      #df$negativelog10p<-logp()
      #df$input$labelcol[df$negativelog10p>input$pcut]<- ""
      #df
      
    #})
    
    #title variable for new column
    p10<-"negativelog10p"
    #Make data table
    output$contents <- renderDataTable({
      #   # input$file1 will be NULL initially. After the user selects
      #   # and uploads a file, it will be a data frame with 'name',
      #   # 'size', 'type', and 'datapath' columns. The 'datapath'
      #   # column will contain the local filenames where the data can
      #   # be found.
      
      
      if (is.null(inFile()))
        return(NULL)
      Data()
      
      
    })
    

    
    ##Make scatterplot 
    plotInput<- reactive({
      if(is.null(Data()))
        return(NULL)
      ggplot(Data(), aes_string(x = fc(), y = p10))+ #abs(Data()[[fc()]])>fccut()
        geom_point() +geom_text_repel(data=Data(),aes_string(label=input$labelcol))+
        ggtitle(title())+ 
        labs(y = ylabel())+
        theme(plot.title = element_text(hjust = 0.5))})
    
    
    
    output$main_plot <- renderPlot(expr={
      
      if(is.null(inFile()))
        return(NULL)
      plotInput()
    }
    )
    # goto: https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
    #This doesn't work in the Rshiny test environment. It does work in the browser.
    output$filename <- downloadHandler(
      filename = function() { paste(title(), '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "png")
      }
    )
    #for running tests
    output$test <-renderText({
    if  (is.null(inFile()))
      NULL
    ma<-sum(Data()[[input$labelcol]]!="")
    ma  
    })  
    
  }
  
  shinyApp(ui, server)
}

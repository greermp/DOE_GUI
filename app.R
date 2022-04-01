library(shiny)
library(DT)
library(reticulate)

source_python('DoeMaker.py')

# Define UI
ui <- shinyUI(fluidPage(
    
    radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
    
    
    radioButtons('doeType', 'Choose a DOE type',,choices = c("2 level Full Factorial",",",":"), selected=","),
              # accept = c(
              #     'text/csv',
              #     'text/comma-separated-values',
              #     '.csv'
              # )),
    dataTableOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {
    
    flights <- ff2n(5)
    print(flights)
    
    df_products_upload <- reactive({
        if (input$doeType == "2 level Full Factorial"){
        doe <- ff2n(5)
        # print (inFile)
        if (is.null(doe))
            return(NULL)
        df <- data.frame(doe)
        # df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
        return(df)
        }
    })
    
    
    # df_products_upload <- reactive({
    #     inFile <- input$target_upload
    #     if (is.null(inFile))
    #         return(NULL)
    #     df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    #     return(df)
    # })
    
    output$sample_table<- DT::renderDataTable({
        df <- df_products_upload()
        DT::datatable(df)
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)

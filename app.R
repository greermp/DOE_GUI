library(shiny)
library(DT)
library(reticulate)

source_python('DoeMaker.py')

# Define UI
ui <- shinyUI(fluidPage(
    
    numericInput("numFactors", 
                 h3("Number of Factors:"), 
                 value = 3),
    
    fluidRow(
        column(6,
        radioButtons('doeType', 'Choose a DOE type',choices = c("2 level Full Factorial",
                                                                "2 level Fractional Factorial",
                                                                "Central Composite Design"), 
                                selected="2 level Full Factorial")),
        #call the inputs based on the results of the if statment from the user selection
        column(6,uiOutput("ui"))
        ),
    fluidRow(
        column(12, uiOutput('colnames'))
    ),
    
    
    dataTableOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {
    # Fractional Factorial...
    df_products_upload <- reactive({
        if (input$doeType == "2 level Fractional Factorial"){
            doe <- fracFact(as.character(input$doeString))

            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            
            return(df)
        }
        # Full Factorial...
        else if (input$doeType == "2 level Full Factorial"){
            doe <- ff2n(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else if (input$doeType == "Central Composite Design"){
            n = as.integer(input$numFactors)

            doe=NULL
            if (! is.null(input$Alpha)){
                doe <- ccd(n, input$CPFB, input$CPSB, input$Alpha, input$Face)
            }
            
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else{
            print("hi")
        }

        
    })
    
    
    output$sample_table<- DT::renderDataTable({
        df <- df_products_upload()
        DT::datatable(df, )
    })
    
    output$colnames <- renderUI({
        textInput("test", label = "Test!")
    })
    
    output$ui <- renderUI({
        
        
        
        
        input_selections=NULL
        if (input$doeType == "2 level Fractional Factorial"){

            input_selections <- list(textInput("doeString", "Alias Structure", value = 'A B AB')
            )
            
        }
        else if (input$doeType == "Central Composite Design"){
            
            input_selections <- list(numericInput("CPFB", "# Center Points (Factorial Block)", 
                                                  value = 1, ),
                                     numericInput("CPSB", "# Center Points (Star Block)", 
                                                  value = 1),
                                     radioButtons("Alpha", "Alpha:  ",
                                                  choices = c("Orthogonal",
                                                              "Rotatable")),
                                     radioButtons('Face', 'Spar Point Location',
                                                  choices = c("Circumscribed (CCC)",
                                                               "Inscribed (CCI)",
                                                               "Faced (CCF)")) 
            )
            
        }
        input_selections
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)

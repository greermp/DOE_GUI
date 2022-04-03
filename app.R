library(shiny)
library(DT)
library(reticulate)
library(dplyr)
library(tibble)


reticulate::virtualenv_create(envname="doeEnv", packages=c("pyDOE2", "numpy"))
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
                                                                "Plackett-Burman",
                                                                "Central Composite Design",
                                                                "Box-Behnken",
                                                                "Latin Hypercube"), 
                                selected="2 level Full Factorial")),
        #call the inputs based on the results of the if statment from the user selection
        column(6,uiOutput("ui"))
        ),
    # uiOutput('colnames'),
    actionButton("save", "Save", width = 200),
    uiOutput("Dynamic"),
    dataTableOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {
    # Fractional Factorial...
    df_products_upload <- reactive({
        if (input$doeType == "2 level Fractional Factorial"){
            
            doe=NULL
            if (is.null(input$doeString)){
                doe = NULL
            }
            else {
                doe <- fracFact(as.character(input$doeString))
            }

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
            print (n)
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
        else if (input$doeType == "Box-Behnken"){
            doe=NULL
            if (! is.null(input$bbcenter)){
                n = as.integer(input$numFactors)
                doe <- bbd(n, input$bbcenter)
            }
            
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else if (input$doeType == "Plackett-Burman"){
            doe <- pbd(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else if (input$doeType == "Latin Hypercube"){
            n = as.integer(input$numFactors)
            print (n)
            doe=NULL
            if(is.null(input$criterion)){
                # criterion = "random"
                return(NULL)
            }
            else if (input$criterion == "Center within the sampling intervals"){
                criterion = "c"
            }else if (input$criterion == "Maximize minimum distance between points"){
                criterion = "m"
            }else if (input$criterion =="Minimize maximum correlation coefficient"){
                criterion = "cm"
            }else{
                criterion = "random"
            }

            doe <- lhc(as.integer(input$numFactors),as.numeric(input$samples), criterion)
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else{
            print("Latin Hypercube")
        }

        
    })
    
    
    output$sample_table<- DT::renderDataTable({
        dfz <- df_products_upload()
        if (! is.null(dfz)){
            colnames(dfz) <- paste('Factor', colnames(dfz),  sep='_') 
            
            
            # dfz <- rownames_to_column(dfz, var="Cases")
            
            #TODO: Convert to loop
            colnames(dfz)[1] <- input$Factor1
            colnames(dfz)[2] <- input$Factor2
            colnames(dfz)[3] <- input$Factor3

            
            DT::datatable( dfz, editable = TRUE,  extensions = "Buttons",
            options = list(pageLength = 100,buttons = c('copy', 'csv'),
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
            ))
        }
    })
    
    
    output$Dynamic <- renderUI({
        LL <- vector("list",as.integer(input$numFactors))   
        dfz <- df_products_upload()
        for(i in 1:as.integer(input$numFactors)){
            # LL[[i]] <- list(radioButtons(inputId = paste0("Factor",i), label = paste0("Factor",i), choices = c("A","B","C")))
            LL[[i]] <- list(column(width=(floor(12/input$numFactors)),textInput(inputId = paste0("Factor",i), label = paste0("Factor",i, " Name"), value = colnames(dfz)[i] )))
        }      
        return(LL)                     
    })
    

    observeEvent(input$save,{
        dfz <- df_products_upload()
        colnames(dfz) <- paste('Factor', colnames(dfz),  sep='_') 
        write.csv(dfz,'test.csv')
    })
    
    output$ui <- renderUI({
        
        
        
        
        input_selections=NULL
        if (input$doeType == "2 level Fractional Factorial"){

            input_selections <- list(textInput("doeString", "Alias Structure", value = 'A B AB')
            )
            
        }
        else if (input$doeType == "Central Composite Design"){
            
            input_selections <- list(fluidRow(
                                        column(6,numericInput("CPFB", "# Center Points (Factorial Block)", 
                                                  value = 1, )),
                                        column(6,radioButtons("Alpha", "Alpha:  ",
                                                              choices = c("Orthogonal",
                                                                          "Rotatable")))),
                                     fluidRow(
                                         column(6, numericInput("CPSB", "# Center Points (Star Block)", 
                                                                value = 1)),
                                         column(6, radioButtons('Face', 'Spar Point Location',
                                                  choices = c("Circumscribed (CCC)",
                                                               "Inscribed (CCI)",
                                                               "Faced (CCF)")))) 
            )
            
        }
        else if (input$doeType == "Box-Behnken"){
            
            input_selections <- list(numericInput("bbcenter", "# Center Points", 
                                                  value = 1 )
            )
            
        }
        else if (input$doeType == "Latin Hypercube"){
            
            input_selections <- list(numericInput("samples", "Sample points per factor:", 
                                                  value = as.integer(input$numFactors)),
                                     radioButtons('criterion', 'Choose how to sample the points:',
                                                  choices = c("Randomize points within the intervals",
                                                              "Center within the sampling intervals",
                                                              "Maximize minimum distance between points",
                                                              "Minimize maximum correlation coefficient"))
            )
            
        }
        input_selections
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)

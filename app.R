library(shiny)
library(DT)
library(reticulate)
library(dplyr)
library(tibble)
library(stringr)


# Create virtual conda environment, require pyDOE2 and numpy, source the pyDOE wrapper
reticulate::virtualenv_create(envname="doeEnv", packages=c("pyDOE2", "numpy"))
source_python('DoeMaker.py')


# Define UI
ui <- shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    fluidRow(column(6,selectInput('doeType', h3('Choose a DOE type'),
                                  choices = c("Mixed-Level Full Factorial",
                                             "2 level Full Factorial",
                                             "2 level Fractional Factorial",
                                             "Plackett-Burman",
                                             "Central Composite Design",
                                             "Box-Behnken",
                                             "Latin Hypercube"), 
                                  selected="2 level Full Factorial")), 
        column(6, numericInput("numFactors", h3("Number of Factors:"),  value = 3))
        # column(2,actionButton("save", h5("Save DOE"))),
    ),
    fluidRow(
        #call the inputs based on the results of the if statment from the user selection
        column(12,uiOutput("ui"))
        ),
    # Render text boxes to allow user to rename columns for DT 
    fluidRow(
        #call the inputs based on the results of the if statment from the user selection
        column(12,uiOutput("Dynamic"))),
    # TODO: fix this
    dataTableOutput("sample_table")
)
)

# Define server logic
server <- shinyServer(function(input, output) {

        observeEvent(input$doeType, {
        if(input$doeType <= '2 level Fractional Factorial'){
            shinyjs::disable("numFactors")
        }else{
            shinyjs::enable("numFactors")
        }
    })
    
    # Fractional Factorial...
    df_products_upload <- reactive({
        if (input$doeType == "Mixed-Level Full Factorial"){
            
            doe=NULL
            args = vector(length=as.integer(input$numFactors))   
            for(i in 1:as.integer(as.numeric(input$numFactors))){
                var=paste0('FactorLevels',i)
                if (is.null(input[[var]])){
                    df <- data.frame(ID = c(1, 2, 3, 4, 5),
                                     var1 = c('a', 'b', 'c', 'd', 'e'),
                                     var2 = c(1, 1, 0, 0, 1))
                    return(df)
                }
                else{
                    args[i] <- as.numeric(input[[var]])
                }
            }

            df <- mlff(args)
          
            df <- data.frame(df)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            return(df)
        }
        else if (input$doeType == "2 level Fractional Factorial"){
            
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
    
    # Show datatable with DOE
    output$sample_table<- DT::renderDataTable({
        dfz <- df_products_upload()
        if (! is.null(dfz)){
            # colnames(dfz) <- paste('Factor', colnames(dfz),  sep='_') 
            
            # Update column names with user input
            if(input$doeType <= '2 level Fractional Factorial'){
                # numFactors = length(input$doeString)
                numFactors = str_count(input$doeString, "\\w+")
            }else{
                numFactors = as.integer(input$numFactors)
            }
            for(i in 1:numFactors){
                var=paste0('Factor',i)
                if (is.null(input[[var]])){
                    colnames(dfz)[i] <- ' '
                }
                else{
                    colnames(dfz)[i] <-input[[var]]
                }
                
            }
            
            # TODO: Make better, figure out buttons
            DT::datatable( dfz, editable = TRUE,  extensions = "Buttons",
            options = list(sDom  = '<"top">lrtB<"bottom">ip', pageLength = 10,buttons = c('csv'),
                           
                           buttons = c('copy', 'csv', 'excel'),
                           
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
            ))
        }
    })
    
    # This will currently break if > 12 factors
    output$Dynamic <- renderUI({
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
            print(numFactors)
        }else{
            numFactors = as.integer(input$numFactors)
        }
        
        LL <- vector("list",numFactors)   
        dfz <- df_products_upload()
        for(i in 1:numFactors){
            LL[[i]] <- list(column(width=(floor(12/numFactors)),textInput(inputId = paste0("Factor",i), label = paste0("Factor ",i), value = colnames(dfz)[i] )))
        }      
        return(LL)                     
    })
    
    # observeEvent(input$save,{
    #     dfz <- df_products_upload()
    #     for(i in 1:as.integer(input$numFactors)){
    #         var=paste0('Factor',i)
    #         # colnames(dfz)[i] <- input$Factor1
    #         if (is.null(input[[var]])){
    #             colnames(dfz)[i] <- ' '
    #         }
    #         else{
    #             colnames(dfz)[i] <-input[[var]]
    #         }
    #         
    #     }
    #     write.csv(dfz,'test.csv')
    # })
    
    # This updates UI (input options) based on DOE type..
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
        }else if (input$doeType == "Mixed-Level Full Factorial"){
            LL <- vector("list",as.integer(input$numFactors))   
            # dfz <- df_products_upload()
            for(i in 1:as.integer(input$numFactors)){
                LL[[i]] <- list(column(width=(floor(12/input$numFactors)),
                                       textInput(inputId = paste0("FactorLevels",i), 
                                                 label = paste0("Factor",i, " # of Levels?"), value = 2 )))
            }      
            # return(LL)  
            input_selections <- LL
            
        }
        input_selections
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)

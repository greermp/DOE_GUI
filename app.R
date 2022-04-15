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
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    titlePanel(""),
    sidebarLayout(position = "left",
        sidebarPanel(width = 2,
    fluidRow(uiOutput("NumberRows")),
    fluidRow(selectInput('doeType', h3('DOE Type:'),
                                  choices = c("2 level Full Factorial",
                                             "2 level Fractional Factorial",
                                             "Plackett-Burman",
                                             "Mixed-Level Full Factorial",
                                             "Central Composite Design",
                                             "Box-Behnken",
                                             "Latin Hypercube"), 
                                  selected="2 level Full Factorial")), 
    fluidRow(numericInput("numFactors", h3("Number of Factors:"),  value = 3)),
    hr(),
    fluidRow(uiOutput("pickacol")),
    fluidRow(uiOutput("someLevels")),
    fluidRow(textInput("replaceWith","Replace With:",placeholder ='new value')),
    fluidRow(actionButton("boom", label = "Update"))
    

    ),
    mainPanel(
    fluidRow(
        # This updates UI (input options) based on DOE type
        column(12,uiOutput("dynamicParameters"))
        ),
    fluidRow(
        # Render text boxes to allow user to rename columns 
        column(12,uiOutput("RenameCols"))
        # column(12,uiOutput("codeFactors"))
        ),
    # Render DOE as data table
    dataTableOutput("doe_table")))))

# Define server logic
server <- shinyServer(function(input, output) {
    # If Fractional Factorial selected, use user string as input.  
    #  deactivate numFactors button
    observeEvent(input$doeType, {
        if(input$doeType == '2 level Fractional Factorial'){
            shinyjs::disable("numFactors")
        }else{
            shinyjs::enable("numFactors")
        }
    })
    
    # Reactive value holds contents of DOE.  Starts out as placeholder dataframe
    DF <- reactiveValues(data=data.frame(ID = c(1, 2, 3, 4, 5),
                                                var1 = c('a', 'b', 'c', 'd', 'e'),
                                                var2 = c(1, 1, 0, 0, 1)))

    # Main reactive function for accessing pyDOE backend
    df_products_upload <- reactive({
        input$Factor1
        if (input$doeType == "Mixed-Level Full Factorial"){
            if (is.null(input$numFactors))
                return (NULL)
            
            args = vector(length=as.integer(input$numFactors))   
            for(i in 1:as.integer(as.numeric(input$numFactors))){
                var=paste0('FactorLevels',i)
                if (is.null(input[[var]])){
                    doe=NULL
                                    }
                else{
                    args[i] <- as.numeric(input[[var]])
                }
            }
            doe <- mlff(args)
        }
        else if (input$doeType == "2 level Fractional Factorial"){
            if (is.null(input$doeString)){
                doe = NULL
            }
            else {
                doe <- fracFact(as.character(input$doeString))
            }

            if (is.null(doe))
                return(NULL)
        }
        else if (input$doeType == "2 level Full Factorial"){
            doe <- ff2n(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
        }
        else if (input$doeType == "Central Composite Design"){
            n = as.integer(input$numFactors)
            doe=NULL
            if (! is.null(input$Alpha)){
                doe <- ccd(n, input$CPFB, input$CPSB, input$Alpha, input$Face)
            }
            
            if (is.null(doe))
                return(NULL)
        }
        else if (input$doeType == "Box-Behnken"){
            n = as.integer(input$numFactors)
            doe <- bbd(n, input$bbcenter)
        }
        else if (input$doeType == "Plackett-Burman"){
            doe <- pbd(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
        }
        else if (input$doeType == "Latin Hypercube"){
            n = as.integer(input$numFactors)
            doe=NULL
            if(is.null(input$criterion)){
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

            doe <- lhc(as.integer(input$numFactors),as.numeric(input$samples), 
                       criterion)
            if (is.null(doe))
                return(NULL)
        }
        
        # Reset as placeholder DF if input is null...
        if (is.null(doe))
            doe <- data.frame(ID = c(1, 2, 3, 4, 5),
                              var1 = c('a', 'b', 'c', 'd', 'e'),
                              var2 = c(1, 1, 0, 0, 1))
        
        df <- data.frame(doe)
        # Prefix row names with 'CASE'
        rownames(df) <- paste0("Case",seq(nrow(df)))
        
        # Access number of factors...
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
        }else{
            numFactors = as.integer(input$numFactors)
        }
    
        
    
        #  Rename columns, if user input is present.  Otherwise use X+i
        if (numFactors == length(df) ){
            for(i in 1:numFactors){
                var=paste0('Factor',i)
                if (is.null(input[[var]]) ){
                    colnames(df)[i] <- paste0('X',i)
                }else{
                    if (input[[var]]=="")
                        colnames(df)[i] <- paste0('X',i)
                    else
                        colnames(df)[i] <-input[[var]]
                }
                
            }
        }

        # Convert columns to a character, since input can be numeric or strings
        df <- df %>%
          mutate(across(everything(), as.character))
        DF$data=df
    })
    
    
    # Output doe as datatable
    output$doe_table<- DT::renderDataTable({
        dfz <- DF$data
        DT::datatable( dfz, editable = TRUE,  extensions = "Buttons",
            options = list(sDom  = '<"top">lrtB<"bottom">ip', pageLength = 25,
                           buttons = c('copy', 'csv', 'excel'),

                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
            ))
    })
    
    # Notify user of number of design points
    output$NumberRows <- renderUI({
        dfz <- DF$data
        nrowz <- vector("list",1)   
        nrowz[[1]] = paste0("Design Points: ",nrow(dfz))
        return (nrowz)
    })
      
    # For coding DOE... User can pick one of the columnns to modify
    output$pickacol <- renderUI({
        selectInput("aCol", label = "Pick a Col to Code:", 
                    choices = colnames(df_products_upload()) )
    })
    
    # For coding DOE...Display levels of currently selected factor
    output$someLevels <- renderUI({
        dfz <- DF$data
        col=input$aCol
        print(dfz[[col]] %>% unique())
        
        x=unique(dfz[[col]]) 
        print(x)
        selectInput("aLevel", label = "Pick a Level to Code:", 
                    choices = x)
    })
    
    # Modify aCol, update aLevel with replaceWith
    observeEvent(input$boom, {
        dfz <- DF$data
        # str(dfz)
        # print("----")
        # print(input$aCol)
        # print(input$aLevel)
        # print(input$replaceWith)
        # print("----")
        var=input$aCol
        level=input$Level1
        print(dfz[[sym(var)]])
        dfz <-dfz %>% mutate("{var}" := case_when(
            !! sym(var) == input$aLevel ~ input$replaceWith,
                            TRUE ~ as.character(!! sym(var)))
        )
        DF$data=dfz
    })
    
    # Creates a UI column containing a textInput where user can rename columns (factors)
    output$RenameCols <- renderUI({
        # If Frac Factorial, use number of words to calculate length (A B C = 3)
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
        }else{
            numFactors = as.integer(input$numFactors)
        }

        textInputs <- vector("list",numFactors)   
        dfz <- DF$data
        for(i in 1:numFactors){
            # Dynamically sets width based on number of text inputs
          textInputs[[i]] <- list(column(width=(ifelse(floor(12/numFactors)<1, 1,
                                                 floor(12/numFactors))),
                textInput(inputId = paste0("Factor",i), label = paste0("Factor ",i), 
                          value = colnames(dfz)[i])))
        }      
        return(textInputs)                     
    })
    
    
    # This updates UI (input options) based on DOE type..
    output$dynamicParameters <- renderUI({
        input_selections=NULL
        if (input$doeType == "2 level Fractional Factorial"){

            input_selections <- list(textInput("doeString", 
                                               "Alias Structure", value = 'A B C ABC')
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
            if (is.null(input$numFactors)){
                return(NULL)
            }
          input_selections <- vector("list",as.integer(input$numFactors))   
            for(i in 1:as.integer(input$numFactors)){
              input_selections[[i]] <- list(column(width=(ifelse(floor(12/input$numFactors)<1, 1,floor(12/input$numFactors))),
                                       textInput(inputId = paste0("FactorLevels",i), 
                                                 label = paste0("Factor",i, " # of Levels?"), value = 2 )))
            }      
            
        }
        input_selections
    })
    
}
)

# Run the application 
shinyApp(ui = ui, server = server)

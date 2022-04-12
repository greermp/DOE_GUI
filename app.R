library(shiny)
library(DT)
library(reticulate)
library(dplyr)
library(tibble)
library(stringr)

# testDf <- read.csv("test.csv")
# Create virtual conda environment, require pyDOE2 and numpy, source the pyDOE wrapper
reticulate::virtualenv_create(envname="doeEnv", packages=c("pyDOE2", "numpy"))
source_python('DoeMaker.py')


# Define UI
ui <- shinyUI(fluidPage(
    titlePanel(""),
    sidebarLayout(position = "left",
        sidebarPanel(width = 2,
            
       
    
    fluidRow(selectInput('doeType', h3('DOE Type:'),
                                  choices = c("Mixed-Level Full Factorial",
                                             "2 level Full Factorial",
                                             "2 level Fractional Factorial",
                                             "Plackett-Burman",
                                             "Central Composite Design",
                                             "Box-Behnken",
                                             "Latin Hypercube"), 
                                  selected="2 level Full Factorial")), 
    fluidRow(numericInput("numFactors", h3("Number of Factors:"),  value = 3)),
    fluidRow(h3(uiOutput("pickacol"))),
    fluidRow(h3(uiOutput("NumberRows"))),

    ),
    mainPanel(#actionButton("toggleSidebar", "Toggle sidebar"),
    fluidRow(
        # This updates UI (input options) based on DOE type
        column(12,uiOutput("dynamicParameters"))
        ),
    fluidRow(
        # Render text boxes to allow user to rename columns for DT 
        column(12,uiOutput("RenameCols")),
        column(12,uiOutput("codeFactors"))),
    # TODO: fix this 
    dataTableOutput("doe_table")))))

# Define server logic
server <- shinyServer(function(input, output) {
    observeEvent(input$toggleSidebar, {
        shinyjs::toggle(id = "Sidebar")
    })
    # If Fractional Factorial selected, use user string as input.  
    #  deactivate numFactors button
    observeEvent(input$doeType, {
        if(input$doeType <= '2 level Fractional Factorial'){
            shinyjs::disable("numFactors")
        }else{
            shinyjs::enable("numFactors")
        }
    })
    
    # Returns DF to be rendered as a datatable 
    # Most logical blocks require null checks on inputs to prevent errors
    # while client-side input is being populated
    df_products_upload <- reactive({
        input$Factor1
        if (input$doeType == "Mixed-Level Full Factorial"){
            if (is.null(input$numFactors))
                return (NULL)
            
            args = vector(length=as.integer(input$numFactors))   
            for(i in 1:as.integer(as.numeric(input$numFactors))){
                var=paste0('FactorLevels',i)
                if (is.null(input[[var]])){
                    # This is a placeholder while the client input loads.. should not be seen
                    df <- data.frame(ID = c(1, 2, 3, 4, 5),
                                     var1 = c('a', 'b', 'c', 'd', 'e'),
                                     var2 = c(1, 1, 0, 0, 1))
                    # return(df)
                }
                else{
                    args[i] <- as.numeric(input[[var]])
                }
            }

            df <- mlff(args)
          
            df <- data.frame(df)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
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
            
            # return(df)
        }
        else if (input$doeType == "2 level Full Factorial"){
            doe <- ff2n(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
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
            # return(df)
        }
        else if (input$doeType == "Box-Behnken"){
            if (is.null(doe))
                return(NULL)
            
            n = as.integer(input$numFactors)
            doe <- bbd(n, input$bbcenter)
            
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        else if (input$doeType == "Plackett-Burman"){
            doe <- pbd(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        else if (input$doeType == "Latin Hypercube"){
            n = as.integer(input$numFactors)
            print (n)
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

            doe <- lhc(as.integer(input$numFactors),as.numeric(input$samples), criterion)
            if (is.null(doe))
                return(NULL)
            df <- data.frame(doe)
            rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        
        
        # if (! is.null(df)){12
            # Update column names with user input
            if(input$doeType <= '2 level Fractional Factorial'){
                numFactors = str_count(input$doeString, "\\w+")
            }else{
                numFactors = as.integer(input$numFactors)
            }
        
            if (is.null(doe))
                return(NULL)
        
        print(df)
        
            # This if statement prevents error messages due to processing delay
            if (numFactors == length(df)){
                for(i in 1:numFactors){
                    var=paste0('Factor',i)
                    if (! is.null(input[[var]])){
                    #     colnames(df)[i] <- 'X'
                    # }
                    # else{
                        colnames(df)[i] <-input[[var]]
                    }
                    
                }
            }
        else{ (print("This aint workin"))}
            
        print(df)
        print(colnames(df))

        return (df)
    })
    
    
    # Show datatable with DOE
    output$doe_table<- DT::renderDataTable({
        dfz <- df_products_upload()
        # if (! is.null(dfz)){12
        #     # Update column names with user input
        #     if(input$doeType <= '2 level Fractional Factorial'){
        #         numFactors = str_count(input$doeString, "\\w+")
        #     }else{
        #         numFactors = as.integer(input$numFactors)
        #     }
        #     # This if statement prevents error messages due to processing delay
        #     if (numFactors == length(dfz)){
        #         for(i in 1:numFactors){
        #             var=paste0('Factor',i)
        #             if (is.null(input[[var]])){
        #                 colnames(dfz)[i] <- 'X'
        #             }
        #             else{
        #                 colnames(dfz)[i] <-input[[var]]
        #             }
        # 
        #         }
        #     }

            DT::datatable( dfz, editable = TRUE,  extensions = "Buttons",
            options = list(sDom  = '<"top">lrtB<"bottom">ip', pageLength = 25,buttons = c('csv'),

                           buttons = c('copy', 'csv', 'excel'),

                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
            ))
        # }
    })
    
    output$NumberRows <- renderUI({
        dfz <- df_products_upload()
        print(nrow(dfz))
        nrowz <- vector("list",1)   
        nrowz[[1]] = paste0(nrow(dfz), " Design Points")
        return (nrowz)
    })
    
    
    output$pickacol <- renderUI({
        # If Frac Factorial, use number of words to calculate length (A B C = 3)
        # if(input$doeType <= '2 level Fractional Factorial'){
        #     numFactors = str_count(input$doeString, "\\w+")
        #     print(numFactors)
        # }else{
        #     numFactors = as.integer(input$numFactors)
        # }
        print(input$Factor1)
        print (colnames(df_products_upload()))
        
        input$doeType
        selectInput("aCol", label = "Pick a Col to Code:", 
                    choices = colnames(df_products_upload()) )
        # }      
        # return(LL) 
    })
    
    
    # Creates a UI column containing a textInput where user can rename columns (factors)
    output$RenameCols <- renderUI({
        # If Frac Factorial, use number of words to calculate length (A B C = 3)
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
            print(numFactors)
        }else{
            numFactors = as.integer(input$numFactors)
        }
        
        LL <- vector("list",numFactors)   
        dfz <- df_products_upload()
        for(i in 1:numFactors){
            # Dynamically sets width based on number of text inputs
            LL[[i]] <- list(column(width=(ifelse(floor(12/numFactors)<1, 1,floor(12/numFactors))),
                textInput(inputId = paste0("Factor",i), label = paste0("Factor ",i), 
                          value = colnames(dfz)[i])))
        }      
        return(LL)                     
    })
    
    
    # This updates UI (input options) based on DOE type..
    output$dynamicParameters <- renderUI({
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
            if (is.null(input$numFactors)){
                return(NULL)
            }
            LL <- vector("list",as.integer(input$numFactors))   
            for(i in 1:as.integer(input$numFactors)){
                LL[[i]] <- list(column(width=(ifelse(floor(12/input$numFactors)<1, 1,floor(12/input$numFactors))),
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

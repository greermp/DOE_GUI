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
testCounter=0
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
    fluidRow(actionButton("boom", label = "Update"))
    

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
    # If Fractional Factorial selected, use user string as input.  
    #  deactivate numFactors button
    observeEvent(input$doeType, {
        if(input$doeType == '2 level Fractional Factorial'){
            shinyjs::disable("numFactors")
        }else{
            shinyjs::enable("numFactors")
        }
    })
    
    # filteredData <- reactiveVal(df_products_upload)
    
    # Returns DF to be rendered as a datatable 
    # Most logical blocks require null checks on inputs to prevent errors
    # while client-side input is being populated

    DF <- reactiveValues(data=data.frame(ID = c(1, 2, 3, 4, 5),
                                                var1 = c('a', 'b', 'c', 'd', 'e'),
                                                var2 = c(1, 1, 0, 0, 1)))
    
    # observe({
    #     DF$data <- df_products_upload()
    # })
    
    df_products_upload <- reactive({
        # print ("hi")
        # print(input$boom)
        # print(testCounter)
        # if (as.numeric(input$boom) > as.numeric(testCounter)){
        #     print("You pressed the button")
        #     testCounter = testCounter+1
        # }
        input$Factor1
        if (input$doeType == "Mixed-Level Full Factorial"){
            if (is.null(input$numFactors))
                return (NULL)
            
            args = vector(length=as.integer(input$numFactors))   
            for(i in 1:as.integer(as.numeric(input$numFactors))){
                var=paste0('FactorLevels',i)
                if (is.null(input[[var]])){
                    # This is a placeholder while the client input loads.. should not be seen
                    doe=NULL
                    
                    # return(df)
                }
                else{
                    args[i] <- as.numeric(input[[var]])
                }
            }
            doe <- mlff(args)
          
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
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
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
            
            # return(df)
        }
        else if (input$doeType == "2 level Full Factorial"){
            doe <- ff2n(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        else if (input$doeType == "Central Composite Design"){
            n = as.integer(input$numFactors)
            doe=NULL
            if (! is.null(input$Alpha)){
                doe <- ccd(n, input$CPFB, input$CPSB, input$Alpha, input$Face)
            }
            
            if (is.null(doe))
                return(NULL)
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        else if (input$doeType == "Box-Behnken"){
            # if (is.null(doe))
            #     return(NULL)
            # 
            n = as.integer(input$numFactors)
            doe <- bbd(n, input$bbcenter)
            
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
        }
        else if (input$doeType == "Plackett-Burman"){
            doe <- pbd(as.integer(input$numFactors))
            if (is.null(doe))
                return(NULL)
            # df <- data.frame(doe)
            # rownames(df) <- paste0("Case",seq(nrow(df)))
            # return(df)
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
        
        
        if (is.null(doe))
            doe <- data.frame(ID = c(1, 2, 3, 4, 5),
                              var1 = c('a', 'b', 'c', 'd', 'e'),
                              var2 = c(1, 1, 0, 0, 1))
        
        df <- data.frame(doe)
        rownames(df) <- paste0("Case",seq(nrow(df)))
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
        }else{
            numFactors = as.integer(input$numFactors)
        }
    
        
    
        # This if statement prevents error messages due to processing delay
        if (numFactors == length(df) ){
            for(i in 1:numFactors){
                var=paste0('Factor',i)
                # cname = input[[var]]
                # print(cname)
                # if (cname == ""){
                #     cname=paste0('x',i)
                # }
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
        # else{ (print("This aint workin"))}
        # print(df)
        DF$data=df
        # return (df)
    })
    
    
    # Show datatable with DOE
    output$doe_table<- DT::renderDataTable({
        # dfz <- df_products_upload()
        dfz <- DF$data
        DT::datatable( dfz, editable = TRUE,  extensions = "Buttons",
            options = list(sDom  = '<"top">lrtB<"bottom">ip', pageLength = 25,
                           buttons = c('copy', 'csv', 'excel'),

                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}")
            ))
        # }
    })
    
    output$NumberRows <- renderUI({
        dfz <- DF$data
        # print(nrow(dfz))
        nrowz <- vector("list",1)   
        nrowz[[1]] = paste0("Design Points: ",nrow(dfz))
        return (nrowz)
    })
      
    output$pickacol <- renderUI({
        # dfz=DF$data
        # print(dfz)
        selectInput("aCol", label = "Pick a Col to Code:", 
                    # choices = colnames(dfz) )
                    choices = colnames(df_products_upload()) )
    })
    
    # observeEvent(input$aCol, {
    output$someLevels <- renderUI({
        dfz <- DF$data
        # print(dfz)
        # print((input$aCol))
        # if (! (is.null(input$aCol) | input$aCol== "")){
        if (! is.null(input$aCol)) {
            if (! input$aCol== "") {
                # print(input$aCol)
                z=dfz %>% select(input$aCol) %>% unique() %>% 
                    arrange(as.numeric(input$aCol))
                # print(z)
                # print(nrow(z))
                num=nrow(z)
                UncodeThese <- vector("list",num)  
                for(i in 1:num){
                    cell=dfz %>% select(input$aCol) %>% unique()
                    cell=cell[i,1]
                    UncodeThese[[i]] <- list(textInput(inputId = paste0("Level",i), 
                                            label = paste0("Level ",i),
                                            placeholder = cell))
                } 
            return(UncodeThese)
            }
            return (1)
        }
        return (1)
    })
    
    observeEvent(input$boom, {
        dfz <- DF$data
        print(input$aCol)
        var=input$aCol
        level=input$Level1
        
        dfz <-dfz %>% mutate("{var}" := case_when(
            !! sym(var) == -1 ~ input$Level1)
        )
        
        # dfz <-dfz %>% mutate(!!var := case_when(X1 == -1 ~ -111)
        #                                         )
        
        # dfz <- dfz %>% mutate(!!var := case_when(
        #                     !!var==1 ~ 111,
        #                     !!var==-1 ~ -111
        # ))
        # 
        print(typeof(dfz[[var]] %>% unique()))
        # dfz %>% mutate(input$aCol=2)
        print(dfz)
        # print(DF$data)
        # iris[col][iris[col]==input$oldVal] <- as.numeric(input$newVal)
        # print(iris)
        # df_products_upload(dfz)
    })
    
    # Creates a UI column containing a textInput where user can rename columns (factors)
    output$RenameCols <- renderUI({
        # If Frac Factorial, use number of words to calculate length (A B C = 3)
        if(input$doeType <= '2 level Fractional Factorial'){
            numFactors = str_count(input$doeString, "\\w+")
            # print(numFactors)
        }else{
            numFactors = as.integer(input$numFactors)
        }
        
        LL <- vector("list",numFactors)   
        dfz <- DF$data
        for(i in 1:numFactors){
            # Dynamically sets width based on number of text inputs
            LL[[i]] <- list(column(width=(ifelse(floor(12/numFactors)<1, 1,
                                                 floor(12/numFactors))),
                textInput(inputId = paste0("Factor",i), label = paste0("Factor ",i), 
                          value = colnames(dfz)[i])))
        }      
        return(LL)                     
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

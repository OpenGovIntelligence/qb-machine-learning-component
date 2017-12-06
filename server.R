#
# This is the server of the OGI machine learning component
#

# Define server logic required to draw a histogram

source("setServerOptions.R")
source("getCubeObservations.R")
source("calculateFeatures.R")

results <-reactiveValues(SME=NULL, NV=NULL,SE=NULL)

ML_server <- shinyServer(function(input, output) {
  setServerOptions(4607, "127.0.0.1") #define the port and host of the app
  
  #initialize some variables
  v <- reactiveValues(data = NULL)
  b <- reactiveValues(data = NULL)

  
  #observe actions in buttons of the UI
  observeEvent(input$action, {
    v$data <- rnorm(100)
  }) #end of observeEvent
  
  observeEvent(input$action2, {
    b$data <- rnorm(100)
  })#end of observeEvent
  
  #########################################################################################
  
  #create the dropdown for the reference period
  output$yearCB <- renderUI({ 
    if (identical(input$var, "")) return()
    
    selectionInput <- reactive(input$var) #get the selected response variable
   
    #currently the years of the dropdown are inserted manually
    selectInput("y", label = "Please select time period", 
                list("2011"=2011,"2012"=2012,"2013"=2013,"2014"=2014))
    
  })#end of renderUI
  
  #########################################################################################
  
  #create the checkboxGroup with the compatible datasets
  output$xdatasets <- renderUI ({
    if (is.null(v$data)) return()
    selectionInput <- reactive(input$var) #get the selected response dataset
    selectionInput2 <- reactive(paste("http://reference.data.gov.uk/id/year/",input$y,sep="")) #get the selected year
   
    #create the query to get 
    q <-paste('{datasets(and:{ componentValues:
                    [{component: \"http://purl.org/linked-data/sdmx/2009/dimension#refArea\" 
                    level: \"http://statistics.gov.scot/def/geography/collection/dz-2001\"} ] 
                    } 
                    or:  { componentValues:[ 
                    {component:\"http://purl.org/linked-data/sdmx/2009/dimension#refPeriod\"  
                    values: ["',selectionInput2(),'"] }]}) 
                    {uri title schema}}', sep="")
    
    #execute the query and get the results
    results <- runQuery(q)
    
    #create the checkboxGroup
    checkboxGroupInput("checkGroup", label = "Please select the predictors", 
                       choices = results$data.datasets.schema,
                       selected = 1)
  })#end of renderUI
  
  #########################################################################################
  
  #create action buttons of the UI
  output$actionbutton <-renderUI ({
    if (identical(input$var, "")) return()
    actionButton("action", label = "Find predictors") 
  }
  )
  output$actionbutton2 <-renderUI ({
    if (is.null(v$data)) return()
    actionButton("action2", label = "Extract features") 
  })#end of renderUI
  
  #########################################################################################
  
  #create and present the plots
  output$plot <- renderPlot({
    if (is.null(b$data)) return()
    x_datasets <-reactive({as.vector(input$checkGroup)}) #get the selected compatible datasets
    y_dataset <- reactive(input$var) #get the selected response dataset
    
    selectionInput2 <- reactive(input$y) #get the selected year
    year<-paste('reference_period:',selectionInput2())

    calculateFeatures(y_dataset, x_datasets, year) #calculate the predictors and create (1) the Lambda-Coefficients plot and (2) the Lambda - MSE Error plot
    
  }, height = "auto", width = "auto") #end of renderPlot
  
  
  #########################################################################################
  
  #return the Lowest MSE value along with the number of variables
  output$vars <- renderText({
    if(is.null(results$MSE)) return()
    paste(c ("Lowest MSE:" , results$MSE , "Number of Variables:" , results$NV ))
  }) #end of renderText
  
  #return the 1 Standard Error value along with the number of variables
  output$vars2 <- renderText({
    if(is.null(results$MSE)) return()
    paste(c ("1 Standard Error :", results$SE, "Number of Variables:", results$NV1 ))
  }) #end of renderText
  
}) #end of shinyServer


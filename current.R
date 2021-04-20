########################
## Import R Libraries ##
########################

## TO DO: remove unused libraries

library(shiny)
library(shinyFiles)
library(htmltools)
library(shinydashboard)
library(stringi)
library(DT)
library(readxl)
library("kSamples", character.only = TRUE)
library("evd",character.only=TRUE)
library("fitdistrplus",character.only=TRUE)
library(shinyjs)
library(plyr)
library(robustbase)
library(ggplot2)
library(tibble)

##########################################
## TO DO: Fix this code to disable tabs ##
##########################################

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
background-color: #d3d3d3 !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #d3d3d3 !important;
}"

####################
## User Interface ##
####################

ui = navbarPage(" ", id="mainPage",  
                
                #####################
                ## File Upload Tab ##
                #####################
                
                tabPanel(title= "File Upload", id = "FileUpload",
                         sidebarPanel(
                           width = 2,
                           fileInput('files', 'Upload control files', multiple = TRUE, 
                                     accept = c('text/csv', 'text/comma-separated-values',
                                                'text/tab-separated-values', 'text/plain','.csv', '.ASC', '.xlsx', '.xls')),
                           fileInput('files2', 'Upload treatment files', multiple = TRUE, 
                                     accept = c('text/csv', 'text/comma-separated-values', 
                                                'text/tab-separated-values', 'text/plain', '.csv', '.ASC', '.xlsx', '.xls')),
                           selectInput(inputId = "columns", label = "Select column", choices = " "),
                           checkboxInput('header', 'First row is column names', FALSE),
                           radioButtons('sep', 'Separator', c(Tab = '\t', Comma = ',', Semicolon = ';', Pipe = '|'), '\t'),
                           radioButtons('quote', 'Quote', c(None = '', 'Double Quote' = '"', 'Single Quote' = "'"),
                                        selected = '')),
                         
                         
                         ###################
                         ## Sampling Menu ##
                         ###################
                         
                         sidebarPanel(
                           width = 2, #take off width and place box under other boxes
                           radioButtons(inputId = 'Sampling', 'Sampling', c(Quantile  = 'Quantile Sampling', Random = 'Random Sampling'), selected = 'Quantile Sampling'),
                           textInput("minEvents", "Minimum number of events per cell", value = 30),
                           textInput("nEvents", "Number of samples per cell", value = 30)
                         ),
                         
                         #######################################
                         ## Control and Treatment Data Tables ##
                         #######################################
                         
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Control", dataTableOutput("table1")),
                             tabPanel("Treatment", dataTableOutput("table2"))
                           )
                         )
                ),
                
                ######################
                ## Summary Data Tab ##
                ######################
                
                tabPanel(title = "Summary Data", id = "Summary",
                         sidebarPanel(
                           width = 3,
                           radioButtons(inputId = "bins", label = "Size of bins", choices = c('0.1' = 0.1, '0.5' = 0.5, '1' = 1, '2' = 2, '5' = 5), selected ='1', inline = TRUE),
                           radioButtons(inputId = "characteristic", label = "Characteristic", inline = TRUE,
                                        choices = c("Amplitude", "Rise Time", "Decay Time", "Other"),
                                        selected = "Amplitude"),
                           radioButtons(inputId = "unit", label = "Unit", inline = TRUE,
                                        choices = c('pA', 'nA', paste("\u03BC", "A", sep =""), 'mA'), 
                                        selected = 'pA'),
                           sliderInput(inputId = "xrange", label = "X-Axis Range", min = 0, max = 50, value = c(0, 50)),
                           downloadButton('downloadValues',"Values After Sampling"),
                           downloadButton("downloadSamplingCDFValues", "CDF"),
                           downloadButton('downloadMeans', "Means"),
                           downloadButton('downloadCells', 'Removed Cells'),
                           fluidRow(
                             column(12, box(title = "Means: Control Cells", width = NULL, 
                                            status = "primary", div(style =" overflow-y:scroll; max-height: 250px", 
                                                                    verbatimTextOutput(outputId = 'MeansCON')))
                             )
                           ),
                           tags$br(),
                           fluidRow(
                             column(12, box(title = "Means: Treatment Cells", width = NULL, 
                                            status = "primary", div(style =" overflow-y:scroll; max-height: 250px", 
                                                                    verbatimTextOutput(outputId = 'MeansTTX')))
                             )
                           ),
                           tags$br(),
                           fluidRow(
                             column(12, box(title = "Unused Cells", width = NULL))
                           ),
                           fluidRow(
                             column(6, "Control Cells"),
                             column(6, "Treatment Cells")
                           ),
                           fluidRow(
                             column(6, verbatimTextOutput(outputId = 'RemovedCellsCon')),
                             column(6, verbatimTextOutput(outputId = 'RemovedCellsTTX'))
                           )
                         ),
                         mainPanel(
                           # Output: Histogram ----
                           plotOutput(outputId = "histControl", dblclick = "histControl_dblclick"),
                           # Output: Histogram ----
                           plotOutput(outputId = "histTTX", dblclick = "histTTX_dblclick"),
                           # Output: CDF ----
                           plotOutput(outputId = "cdfPlot")
                         )
                ),
                tabPanel(title = "Scaling Processes", id = "Scaling",
                         #div(style="display:inline-block", actionButton("btnBackToSummaryGraphs", "<<"), style="float:left"),
                         sidebarPanel(
                           width = 3,
                           # Input: Slider for the size of bins ----
                           #sliderInput(inputId = "bins", label = "Size of bins:", min = 1, max = 20, value = 5)
                           
                           #Input: Radio buttons for the size of bins
                           radioButtons(inputId = "Sampling2", label = "Process", 
                                        choices = c('Rank-Order' = 'RO',
                                                    'Iterative' = 'IT',
                                                    'Comparative Standardization' = 'CS'), selected ='RO'),
                           useShinyjs(),
                           sliderInput(inputId = "scalingxrange", label = "X-Axis Range", 
                                       min = 0, max = 150, value = c(0, 150)),
                           sliderInput(inputId = "scalingyrange", label = "Y-Axis Range Linear Plot", 
                                       min = 0, max = 150, value = c(0, 150)),
                           sliderInput(inputId = "scalingyrangeRatio", label = "Y-Axis Range Ratio Plot", 
                                       min = 0, max = 2, value = c(0, 2), step = 0.01),
                           #checkboxInput('quantiles', 'View quantiles', TRUE),
                           tags$br(),
                           fluidRow(
                             column(12, verbatimTextOutput(outputId = 'R2'))
                           ),
                           fluidRow(
                             column(12, verbatimTextOutput(outputId = 'Slope'))
                           ),
                           fluidRow(
                             column(12, verbatimTextOutput(outputId = 'Intercept'))
                           ),
                           fluidRow(
                             column(12, verbatimTextOutput(outputId = 'KSPVal'))
                           ),
                           fluidRow(
                             column(12, verbatimTextOutput(outputId = 'ADPVal'))
                           ),
                           fluidRow(downloadButton('download',"Metrics"), 
                                    downloadButton('downloadCDFScaling',"CDF"), 
                                    downloadButton('downloadScalingPlots',"Plots"))
                         ),
                         mainPanel(
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), 
                                         plotOutput(outputId = "cdfPlotScaling"), 
                                         plotOutput(outputId = "rankOrderLinearPlot"))
                           ),
                           fluidRow(
                             splitLayout(cellWidths = c("25%", "50%", "25%"),
                                         NULL,
                                         plotOutput(outputId = 'rankOrderRatioPlot'),
                                         NULL)
                           ))
                ),
                tabPanel(title= "Exponentiation", id = "Exp",
                         
                         mainPanel(
                           # fluidRow(
                           #   splitLayout(cellWidths = c("50%", "50%"), 
                           #               plotOutput(outputId = "logPlot"), 
                           #               plotOutput(outputId = "plotResiduals"))
                           # ),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), 
                                         plotOutput(outputId = "plotnls"), 
                                         plotOutput(outputId = "plotRO"))
                                         #plotOutput(outputId = "plotResidualsNls"))
                           ),
                           
                           # fluidRow(
                           #   splitLayout(cellWidths = c("50%", "50%"), 
                           #               plotOutput(outputId = "plotRO"), 
                           #               plotOutput(outputId = "plotROResid"))
                           # ),
                           
                           fluidRow(
                             splitLayout(cellWidths = c("50%", "50%"), 
                                         plotOutput(outputId = "plotIT"),
                                         plotOutput(outputId = "plotCS"))
                                         #plotOutput(outputId = "plotCompStandResidCon"))
                           )
                           #fluidRow(plotOutput(outputId = "testCDF"))
                           
                         )
                )
)

server = function(input, output, session) {
  
  # js$disableTab("Summary")
  # js$disableTab("Scaling")
  # 
  ######################## ACTION BUTTONS ###################################################
  observeEvent(input$btnToSummaryGraphs, {
    validate(need(input$files != "", ""))   #uploaded control data
    validate(need(input$files2 != "", ""))  #uploaded ttx data
    validate(need(grepl("[[:digit:]]",input$minEvents, perl = TRUE) == TRUE && 
                    input$minEvents != 0, "Minimum number of events must be a number larger than 0"))
    #&& input$minEvents <= cell w the largest number of events
    validate(need(grepl("[[:digit:]]",input$nEvents, perl = TRUE) == TRUE &&
                    input$nEvents != 0, "Number of events to sample must be a number larger than 0"))
    # TODO enable next button
    updateTabsetPanel(session = session, inputId = "mainPage", selected = "Summary Graphs")
  })
  
  observeEvent(input$Sampling2, {
    toggleState("scalingyrangeRatio", input$Sampling2 == "RO")
    toggleState("scalingyrange", input$Sampling2 == "RO")
  })
  
  # observeEvent(input$btnData, {
  #   # TODO reset min num of events and num of samples to 30, clear files uploaded, clear radio buttons and 
  #   # column select
  # })
  
  observeEvent(input$btnBackToFileUpload, {
    updateTabsetPanel(session = session, inputId = "mainPage", selected = "File Upload")
  })
  
  observeEvent(input$btnToScalingProcesses, {
    updateTabsetPanel(session = session, inputId = "mainPage", selected = "Scaling Processes")
  })
  
  observeEvent(input$btnBackToSummaryGraphs, {
    updateTabsetPanel(session = session, inputId = "mainPage", selected = "Summary Graphs")
  })
  
  observeEvent(input$characteristic,{
    if(input$characteristic == "Rise Time" || input$characteristic == "Decay Time"){
      enable("unit")
      updateRadioButtons(session = session, inputId = "unit", choices = c(paste("\u03BC", "s", sep =""), "ms", "s"), 
                         inline = TRUE, selected = "ms")
    } else if(input$characteristic == "Amplitude" ){
      enable("unit)")
      updateRadioButtons(session = session, inputId = "unit", choices = c('pA', 'nA', paste("\u03BC", "A", sep =""), 'mA'), 
                         inline = TRUE, selected = "pA")
    } else{
      #hide unit radio buttons and display two text inputs
      disable("unit")
      updateRadioButtons(session = session, inputId = "unit", choices = c('unit'), 
                         inline = TRUE, selected = "unit")
    }
    
  })
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ######################## HIDE TAB UNTIL INPUT IS VALIDATED #####################################
  # observeEvent(input$mainPage, {
  #   hideTab(inputId = "mainPage", target = "Graphs")
  # })
  
  #vectors, lists,...
  vecFileNames <- vector()
  conAmpsByRec <- list()  #base list that is appended to
  
  vecFileNames2 <- vector()
  ttxAmpsByRec <- list()   #base list that is appended to
  
  newVector <- list()
  newVectorTTX <- list()
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ################################### FOLDER UPLOAD #####################################
  # volumes <- getVolumes()
  # shinyDirChoose(input, 'directory', roots=volumes, session=session)
  # path1 <- reactive({
  #   return(print(parseDirPath(volumes, input$directory)))
  # })
  # 
  # dataruw1 <- eventReactive(input$directory, {
  #   datpat <- paste0(path1(),"/data.csv")
  #   dataruw <- read.csv(datpat, header = input$header, sep = input$sep, quote = input$quote,
  #                       stringsAsFactors = FALSE)
  #   dataruw
  # })
  # 
  # output$text <- renderText({
  #   path1()
  # })
  # 
  # output$table <- renderDataTable({
  #   dataruw1()
  # })
  
  
  # lst1 <- reactive({
  #   validate(need(input$directory != "", "select files..."))
  #
  #   if (is.null(input$directory)) {
  #     return(NULL)
  #   } else {
  #
  #     print(parseDirPath(volumes, input$directory)) #access files here
  #     path_list <- as.list(parseDirPath(volumes, input$directory)$datapath)
  #     tbl_list <- lapply(input$directory$datapath, read.table, header=FALSE, sep="\t")
  #
  #
  #     df <- do.call(rbind, tbl_list)
  #     return(df)
  #   }
  # })
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ################################## DYNAMIC COMBO BOX ###################################
  choices_columns <- reactive({
    if (is.null(input$files) || is.null(input$files2)) {
      return(NULL)
    }
    else{
      
      if(tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xlsx" || 
         tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xls"){
        
        x <- read_excel(input$files[[1, 'datapath']], col_names = input$header)
        names <- colnames(x)
        i <- 0
        for (x in names) {
          i <- i + 1
        }
        choices_columns <- c(1:i)
        
      } else{
        
        x <- read.csv(input$files[[1, 'datapath']], header = input$header,
                      sep = input$sep, quote = input$quote)
        names <- colnames(x)
        i <- 0
        for (x in names) {
          i <- i + 1
        }
        choices_columns <- c(1:i)
        
      }
    }
  })
  
  # Default selected column in dropdown list as column 1
  observe({
    updateSelectInput(session = session, inputId = "columns", choices = choices_columns(), selected = 1)
  })
  
  # observe({
  #   hideTab(inputId = "mainPage", target = "Graphs")
  #   hideTab(inputId = "mainPage", target = "Graphs2")
  # })
  
  output$instructions <- renderUI({
    if (is.null(input$files) || is.null(input$files2)){
      tags$h4("Select column by clicking the table below or by choosing from the dropdown menu")
      tags$hr()
    }
  })
  
  # choices_columns2 <- reactive({
  #   if (is.null(input$files2)) {
  #     return(NULL)
  #   }
  #   else{
  #     x <- read.csv(input$files2[[1, 'datapath']], header = input$header,
  #                    sep = input$sep, quote = input$quote)
  #     names <- colnames(x)
  #     i <- 0
  #     for (x in names) {
  #       i <- i + 1
  # })
  # 
  #     }
  #     choices_columns <- c(1:i)
  #   }
  # observe({
  #   updateSelectInput(session = session, inputId = "columns2", choices = choices_columns2(), selected = 3)
  # })
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ############################### VALIDATE INPUT #########################################
  # observeEvent(input$btnData, {
  #   validate(need(input$files != "", ""))   #uploaded control data
  #   validate(need(input$files2 != "", ""))  #uploaded ttx data
  #   validate(need(grepl("[[:digit:]]",input$minEvents, perl = TRUE) == TRUE && 
  #                   input$minEvents != 0, "Minimum number of events must be a number larger than 0"))
  #                   #&& input$minEvents <= cell w the largest number of events
  #   validate(need(grepl("[[:digit:]]",input$nEvents, perl = TRUE) == TRUE &&
  #                   input$nEvents != 0, "Number of events to sample must be a number larger than 0"))
  #   #TODO enable next button
  #   
  #   #showTab(inputId = "mainPage", target = "Summary Graphs")
  #   #showTab(inputId = "mainPage", target = "Scaling Processes")
  #   #updateTabsetPanel(session = session, inputId = "mainPage", selected = "Summary Graphs")
  #   #updateActionButton(session = session, inputId = "btnData", label = "Clear Input")
  #   #max <- max_x()
  #   #updateSliderInput(session = session, inputId = "xrange", min = 0, max = max, value = c(0:max))
  # })
  
  # observeEvent(input$btnBack, {
  #   #clear selected files??
  #   updateTabsetPanel(session = session, inputId = "mainPage", selected = "File Upload")
  # })
  
  # observeEvent(input$btnLockControl, {
  #   if (is.null(input$files)){
  #     #SELECT FILES
  #     hideTab(inputId = "mainPage", target = "Graphs")
  #   } else {
  #     #good for next check
  #   }
  #     
  #   
  #   #if statement-> if btn is labeled lock then do x otherwhise do y?
  #   #Validate Control entry block is filled
  #   #change label
  # })
  # 
  # observeEvent(input$btnLockTTX, {
  #   if (is.null(input$files)){
  #     #SELECT FILES
  #     hideTab(inputId = "mainPage", target = "Graphs")
  #   } else {
  #     #good for next check
  #   }
  #   
  #   #if statement-> if btn is labeled lock then do x otherwhise do y?
  #   #Validate TTX entry block is filled
  #   #change label
  # })
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ########################## HIGHLIGHT COLUMN CHOICE #####################################
  
  output$table1 <- DT::renderDataTable({
    if (is.null(input$files)){
      return(NULL)
    }
    
    #import excel files and render data table
    if(tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xlsx"){
      control_data = read_xlsx(input$files[[1, 'datapath']], col_names = input$header)
    } else{
      #import csv files and render data table
      control_data = read.csv(input$files[[1, 'datapath']], header = input$header, sep = input$sep, quote = input$quote)
    }
    # col_selected = input$columns
    # if(!(is.numeric(col_selected))) {
    #   col_selected = 1
    # }  
    DT::datatable(control_data, selection = 'none', options = list(scrollX = TRUE))
    })
  
  output$table2 <- DT::renderDataTable({
    if (is.null(input$files2)){
      return(NULL)
    }
    
    #import excel files and render data table
    if(tolower(tools::file_ext(input$files2[[1, 'datapath']])) == "xlsx"){
      ttx_data = read_xlsx(input$files2[[1, 'datapath']], col_names = input$header)
      #ttx_data = read_xlsx(input$files2, col_names = input$header)
    } else{
      #import csv files and render data table
      ttx_data = read.csv(input$files2[[1, 'datapath']], header = input$header, sep = input$sep, quote = input$quote)
    }
    # col_selected = input$columns
    # if(!(is.numeric(col_selected))) {
    #   col_selected = 1
    # }
    DT::datatable(ttx_data, selection = 'none', options = list(scrollX = TRUE))
  })
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ####################################### SAMPLING #######################################
  # samplingRandom <- reactive({
  #   #sampling
  #   minEvents <- as.numeric(input$minEvents)
  #   nEvents <- as.numeric(input$nEvents)
  #   
  #   conAmpsByRec <- lst1()
  #   ttxAmpsByRec <- lst2()
  #   
  #   conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
  #   conRecInxs <- conEventCounts >= minEvents
  #   conAmpsByRec <- conAmpsByRec[conRecInxs]
  #   
  #   ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
  #   ttxRecInxs <- ttxEventCounts >= minEvents
  #   ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
  #   
  #   #print(length(conAmpsByRec[[1]]))
  #   
  #   if(input$Sampling == "Random Sampling"){
  #     conAmps_rand <- sapply(conAmpsByRec,sample,nEvents,replace=FALSE)
  #     conAmps_rand <- sort(conAmps_rand)
  #     
  #     ttxAmps_rand <- sapply(ttxAmpsByRec,sample,nEvents,replace=FALSE)
  #     ttxAmps_rand <- sort(ttxAmps_rand)
  #   }
  #   
  # })
  
  # conAmpsByRecFn <- reactive({
  #   minEvents <- as.numeric(input$minEvents)
  #   nEvents <- as.numeric(input$nEvents)
  # 
  #   conAmpsByRec <- lst1()
  # 
  #   conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
  #   conRecInxs <- conEventCounts >= minEvents
  #   conAmpsByRec <- conAmpsByRec[conRecInxs]
  #   return(conAmpsByRec)
  # })
  
  numberOfConSampled <- reactive({
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents)
    
    conAmpsByRec <- lst1()
    
    conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
    #print(conAmpsByRec[conEventCounts < minEvents])
    conRecInxs <- conEventCounts >= minEvents
    conAmpsByRec <- conAmpsByRec[conRecInxs]
    return(conAmpsByRec)
  })
  
  
  conFilesRemoved <- reactive({
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents)
    
    conAmpsByRec <- lst1()
    
    conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
    removed <- conAmpsByRec[conEventCounts < minEvents]
    return(names(removed))
  })
  
  
  samplingCon <- reactive({
    
    # minEvents <- as.numeric(input$minEvents)
    # nEvents <- as.numeric(input$nEvents)
    # 
    # conAmpsByRec <- lst1()
    # 
    # conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
    # conRecInxs <- conEventCounts >= minEvents
    # conAmpsByRec <- conAmpsByRec[conRecInxs]
    
    nEvents <- as.numeric(input$nEvents)
    conAmpsByRec <- numberOfConSampled()
    
    if(input$Sampling == "Quantile Sampling"){
      
      quanSeq <- seq(0.5/nEvents,1-0.5/nEvents,by=1/nEvents)
      
      # compute the quantiles in quanSeq for all vectors in conAmpsByRec
      
      conAmps_quantiles <- sapply(conAmpsByRec,quantile,quanSeq)
      conAmps_quantiles <- sort(conAmps_quantiles)
      
      return(conAmps_quantiles)
      
    } else if(input$Sampling == "Random Sampling"){
      # set.seed(15)
      
      conAmps_rand <- sapply(conAmpsByRec,sample,nEvents,replace=FALSE)
      conAmps_rand <- sort(conAmps_rand)
      
      # sink("conAmpsRandFile.txt", append = TRUE)
      # cat(conAmps_rand)
      # cat("\n")
      # sink()
      return(conAmps_rand)
    }
    
  })
  
  # ttxAmpsByRec <- reactive({
  #   minEvents <- as.numeric(input$minEvents)
  #   nEvents <- as.numeric(input$nEvents)
  #   
  #   ttxAmpsByRec <- lst2()
  #   
  #   ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
  #   ttxRecInxs <- ttxEventCounts >= minEvents
  #   ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
  #   return(ttxAmpsByRec)
  # })
  
  numberOfTTXSampled <- reactive({
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents)
    
    ttxAmpsByRec <- lst2()
    
    ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
    #print(ttxAmpsByRec[ttxEventCounts < minEvents])
    ttxRecInxs <- ttxEventCounts >= minEvents
    ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
    
    return(ttxAmpsByRec)
  })
  
  ttxFilesRemoved <- reactive({
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents)
    
    ttxAmpsByRec <- lst2()
    
    ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
    removed = ttxAmpsByRec[ttxEventCounts < minEvents]
    return(names(removed))
  })
  
  samplingTTX <- reactive({
    # minEvents <- as.numeric(input$minEvents)
    # nEvents <- as.numeric(input$nEvents)
    # 
    # ttxAmpsByRec <- lst2()
    # 
    # ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
    # ttxRecInxs <- ttxEventCounts >= minEvents
    # ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
    
    nEvents <- as.numeric(input$nEvents)
    ttxAmpsByRec <- numberOfTTXSampled()
    
    #print(length(conAmpsByRec[[1]]))
    
    if(input$Sampling == "Quantile Sampling"){
      
      quanSeq <- seq(0.5/nEvents,1-0.5/nEvents,by=1/nEvents)
      
      # compute the quantiles in quanSeq for all vectors in conAmpsByRec
      
      ttxAmps_quantiles <- sapply(ttxAmpsByRec,quantile,quanSeq)
      ttxAmps_quantiles <- sort(ttxAmps_quantiles)
      
      sink("TTXAmpsQuantilesFile.txt", append = TRUE)
      cat(ttxAmps_quantiles)
      cat("\n")
      sink()
      return(ttxAmps_quantiles)
    } else if(input$Sampling == "Random Sampling"){
      #set.seed(15)
      
      ttxAmps_rand <- sapply(ttxAmpsByRec,sample,nEvents,replace=FALSE)
      ttxAmps_rand <- sort(ttxAmps_rand)
      
      return(ttxAmps_rand)
    }
  })
  
  samplingTTXYVals <- reactive({
    ttxYVals <- seq(1/length(samplingTTX()),1,by=1/length(samplingTTX()))
    return(ttxYVals)
  })
  
  samplingConYVals <- reactive({
    conYVals <- seq(1/length(samplingCon()), 1,by=1/length(samplingCon()))
    return(conYVals)
  })
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ############################################
  
  ####################################### SCALING PROCESSES #######################################
  
  ##################### Rank Order ##################
  
  rankOrderCon <- reactive({
    conExp()
    ttxExp()
    test()
    
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents) #we didn't use this bc the samples drawn are based on the
    # length of the shortest list of data
    
    #compute conAmpsByRec
    conAmpsByRec <- lst1()
    conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
    conRecInxs <- conEventCounts >= minEvents
    conAmpsByRec <- conAmpsByRec[conRecInxs]
    
    #compute ttxAmpsByRec
    ttxAmpsByRec <- lst2()
    ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
    ttxRecInxs <- ttxEventCounts >= minEvents
    ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
    
    #get sampling vectors
    conAmpEmpCdf <- samplingCon()
    ttxAmpEmpCdf <- samplingTTX()
    
    #Quantile Sampling
    if(input$Sampling == "Quantile Sampling"){
      if (length(conAmpsByRec) != length(ttxAmpsByRec)) {
        conRankedAmps <- sort(sapply(conAmpsByRec,quantile,probs=seq(0.5/length(ttxAmpsByRec),1-0.5/length(ttxAmpsByRec),by=1/length(ttxAmpsByRec))))
        return(conRankedAmps)
      } else {
        conRankedAmps <- conAmpEmpCdf
        return(conRankedAmps)
      }
    }else if(input$Sampling == "Random Sampling"){
      if (length(conAmpEmpCdf) > length(ttxAmpEmpCdf) ) {
        conRankedAmps <- sort(sample(conAmpEmpCdf,length(ttxAmpEmpCdf),replace=FALSE))
        return(conRankedAmps)
      } else if (length(ttxAmpEmpCdf) > length(conAmpEmpCdf)) {
        conRankedAmps <- conAmpEmpCdf
        return(conRankedAmps)
      } else {
        conRankedAmps <- conAmpEmpCdf
        return(conRankedAmps)
      }
    }
    
  })
  
  rankOrderTTX <- reactive({
    
    minEvents <- as.numeric(input$minEvents)
    nEvents <- as.numeric(input$nEvents)
    
    #compute conAmpsByRec
    conAmpsByRec <- lst1()
    conEventCounts <- as.numeric(lapply(conAmpsByRec,length))
    conRecInxs <- conEventCounts >= minEvents
    conAmpsByRec <- conAmpsByRec[conRecInxs]
    
    #compute ttxAmpsByRec
    ttxAmpsByRec <- lst2()
    ttxEventCounts <- as.numeric(lapply(ttxAmpsByRec,length))
    ttxRecInxs <- ttxEventCounts >= minEvents
    ttxAmpsByRec <- ttxAmpsByRec[ttxRecInxs]
    
    #get sampling vectors
    conAmpEmpCdf <- samplingCon()
    ttxAmpEmpCdf <- samplingTTX()
    
    #Quantile Sampling
    if(input$Sampling == "Quantile Sampling"){
      if (length(conAmpsByRec) != length(ttxAmpsByRec)) {
        ttxRankedAmps <- sort(sapply(ttxAmpsByRec,quantile,probs=seq(0.5/length(conAmpsByRec),1-0.5/length(conAmpsByRec),by=1/length(conAmpsByRec))))
        return(ttxRankedAmps)
      } else {
        ttxRankedAmps <- ttxAmpEmpCdf
        return(ttxRankedAmps)
      }
    }else if(input$Sampling == "Random Sampling"){
      if (length(conAmpEmpCdf) > length(ttxAmpEmpCdf) ) {
        ttxRankedAmps <- ttxAmpEmpCdf
        return(ttxRankedAmps)
      } else if (length(ttxAmpEmpCdf) > length(conAmpEmpCdf)) {
        ttxRankedAmps <- sort(sample(ttxAmpEmpCdf,length(conAmpEmpCdf),replace=FALSE))
        return(ttxRankedAmps)
      } else {
        ttxRankedAmps <- ttxAmpEmpCdf
        return(ttxRankedAmps)
      }
    }
    
  })
  
  rankOrderScaledTTXAmps <- reactive({
    lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
    intercept <- lmRanked$coefficients[1]
    slope <- lmRanked$coefficients[2]
    scaledTTXAmps <- (samplingTTX()-intercept)/slope 

    return(scaledTTXAmps)
  })
  
  ##################### Comparative Standardization ##################
  pooledShape <- reactive ({
    conAmpEmpCdf <- samplingCon()
    ttxAmpEmpCdf <- samplingTTX()
    pooled <- c(conAmpEmpCdf, ttxAmpEmpCdf)
    pooledGEVD <- fitdist(pooled,"gev",start=list(loc=10,scale=4,shape=0.4))
    pooledShape <- as.numeric(pooledGEVD$estimate[3])
    return(pooledShape)
  })
  
  conGEVD <- reactive ({
    conAmpEmpCdf <- samplingCon()
    #ttxAmpEmpCdf <- samplingTTX()
    #pooled <- c(conAmpEmpCdf, ttxAmpEmpCdf)
    #pooledGEVD <- fitdist(pooled,"gev",start=list(loc=10,scale=4,shape=0.4))
    #pooledShape <- as.numeric(pooledGEVD$estimate[3])
    pooledShape <- pooledShape()
    conGEVD <- fitdist(conAmpEmpCdf,"gev",start=list(loc=10,scale=4),fix.arg=list(shape=pooledShape))
    return(conGEVD)
  })
  
  ttxGEVD <- reactive ({
    #conAmpEmpCdf <- samplingCon()
    ttxAmpEmpCdf <- samplingTTX()
    #pooled <- c(conAmpEmpCdf,ttxAmpEmpCdf)
    #pooledGEVD <- fitdist(pooled,"gev",start=list(loc=10,scale=4,shape=0.4))
    #pooledShape <- as.numeric(pooledGEVD$estimate[3])
    pooledShape <- pooledShape()
    ttxGEVD <- fitdist(ttxAmpEmpCdf,"gev",start=list(loc=10,scale=4),fix.arg=list(shape=pooledShape))
    return(ttxGEVD)
  })
  
  multFactor <- reactive({
    ttxGEVD <- ttxGEVD()
    conGEVD <- conGEVD()
    multFactor <- ttxGEVD$estimate[2]/conGEVD$estimate[2]
    return(multFactor)
  })
  
  addFactor <- reactive({
    ttxGEVD <- ttxGEVD()
    conGEVD <- conGEVD()
    addFactor <- ttxGEVD$estimate[1] - multFactor()*conGEVD$estimate[1]
    return(addFactor)
  })
  
  scaledCon <- reactive ({
    multFactor <- multFactor()
    addFactor <- addFactor()
    conAmpEmpCdf <- samplingCon()
    scaledCon <- multFactor * conAmpEmpCdf + addFactor
    return(scaledCon)
  })
  
  ############################ Iterative #################################################
  
  setTestFactors <- reactive({
    
    # gradient descent
    m = 0
    c = 0
    
    L = 0.000001  # The learning rate
    epochs = 100000  # The number of iterations to perform gradient descent
    
    # should this be sampling con and sampling ttx
    #X = rankOrderCon() # Rank-ordered control data
    #Y = rankOrderTTX() # Rank-ordered observed experimental data 
    X = samplingCon()
    Y = samplingTTX()
    n = length(X) # Number of elements in X
    
    # Performing Gradient Descent 
    for(i in 1:epochs){
      Y_pred = m*X + c # The current predicted value of Y
      D_m = (-2/n) * sum(X * (Y - Y_pred))  # Derivative wrt m
      #D_c = (-2/n) * sum(Y - Y_pred)  # Derivative wrt c
      m = m - L * D_m  # Update m
      #c = c - L * D_c  # Update c
    }
    print("m")
    print(m)
    print("c")
    print(c)
    
    scaled <- Y/m
    ksResults <- ks.test(X, scaled)
    print(ksResults$p.value)
    
    # Making predictions
    Y_pred = m*X + c
    
    factMin <- 1	# minimum scaling factor to test
    factMax <- 2	# maximum scaling factor to test
    factStep <- 0.001	# step size between factors

    testFactors <- seq(factMin,factMax,by=factStep)

    return(testFactors)
  })
  
  iterative <- reactive ({
    options(warn=-1)
    
    g1CDF <- samplingCon()
    g2CDF <- samplingTTX()

    # choose whether or not to discard subthreshold events from the scaled treated
    #  distribution - it'll usually be true but sometimes it's helpful to turn it
    #  off for troubleshooting
    doThreshold <- TRUE
    
    testFactors <- setTestFactors()
    ##print Constant and error
    
    # vectors to store results
    pValsByFactor <- vector("numeric",length(testFactors))
    fracDiscarded <- vector("numeric",length(testFactors))
    
    print("p-values: ")
    
    for (tf in 1:length(testFactors)) {
      # scale group 2 (treatment) down by a factor (this method scales treatment
      #  to control, not control to treatment)
      scaled <- g2CDF/testFactors[tf]

      # if thresholding is on, discard scaled values less than the smallest value
      #  in g1 (it's using the smallest detected value in g1 as the "threshold")
      if (doThreshold) {
        #scaled <- scaled[scaled >= 4.01]
        scaled <- scaled[scaled >= g1CDF[1]]
        #print("Threshold value:")
        #print(g1CDF[1])
      }
      
      # use KS test to compare - might want to eventually put in AD as on option,
      #  but it's *really* slow for this
      ksResults <- ks.test(g1CDF,scaled)
      
      # calculate what fraction of g2 was removed by the threshold
      fracDiscarded[tf] <- 1 - (length(scaled)/length(g2CDF))
      
      # store the p value comparing scaled to g1
      pValsByFactor[tf] <- ksResults$p.value
      #print(paste(ksResults$p.value, testFactors[tf], sep = "   "))
    }
    
    # find the index of the largest p value - this corresponds to the factor that
    #  produced the greatest similarity between g1 and scaled. This p value and the
    #  corresponding factor and discarded fraction are the "result" of the method,
    #  but you'll also need to make plots of testFactors (x-axis) vs pValsByFactor
    #  (y-axis) and testFactors vs fracDiscarded. Subsequent runs of the method
    #  should "zoom in" (smaller range, larger step size) on the area of the factor
    #  vs. p value plot where the p value is largest
    maxInx <- which.max(pValsByFactor)
    bestFactor <- testFactors[maxInx]
    bestPVal <- pValsByFactor[maxInx]
    bestFrac <- fracDiscarded[maxInx]
    
    result <- list(maxInx, bestFactor, bestPVal, bestFrac, pValsByFactor, fracDiscarded)
    
    return(result)
    
  })
  
  iterativerScaledTTXAmps <- reactive({
    result <- iterative()
    slope <- result[[2]]
    scaledTTXAmps <- samplingTTX()/slope
    
    return(scaledTTXAmps)
  })
  
  ####################################### Exponentiation ###########################################
  conExp <- reactive({
    #y <- rankOrderTTX()
    #test = lm(rankOrderTTX() ~ rankOrderCon())
    #exp_model = lm(y ~ x)
    return((samplingCon()^1.07))
  })
  
  ttxExp <- reactive({
    # ttx <- samplingTTX()
    # ttxExponent = (ttx^1.06)
    return((samplingTTX()^1.07))
  })
  
  test <- reactive ({
    
    ksRes <- ks.test(conExp(), samplingTTX())
    ksPVal <- ksRes$p.value
    roundedPVal <- signif(ksPVal, 15)
    print("K-S Test Exp: ")
    print(roundedPVal)
    # 0.005570355
    
    adRes <- ad.test(conExp(), samplingTTX())
    adPVal <- adRes$ad[1,3]
    roundedPValAD <- signif(adPVal, 15)
    print("A-D Test Exp: ")
    print(roundedPValAD)
    #0.0035921
  })
  
  output$testCDF <- renderPlot({
    conYVals <- seq(1/length(conExp()), 1, by=1/length(conExp()))
    ttxYVals <- seq(1/length(samplingTTX()), 1, by=1/length(samplingTTX()))
    plot(samplingTTX(), ttxYVals)
    lines(conExp(), conYVals)
  })
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  
  ################################ SAVE COLUMN AS VECTOR #################################
  
  ####################### CON ##########################
  lst1 <- reactive({
    
    validate(need(input$files != "", ""))
    
    if (is.null(input$files)) {
      return(NULL)
    } else {
      #tryCatch({
        
        colChoice = input$columns
        col = as.numeric(colChoice)
        
        if(tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xlsx" ||
           tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xls"){
          tbl_list <- lapply(input$files$datapath, read_excel, col_names = input$header)
          
          #Only used for Excel files
          i <- 1
          for(y in tbl_list){
            newVector[[i]] <- as.matrix(y)
            i <- i+1
          }
          
          i <- 1
          for(y in newVector){ #tbl_list
            conAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files$name){
            x = substring(x, 0, (stri_length(x)-5))
            vecFileNames <- c(vecFileNames, x)
          }
          
          for (r in 1:length(conAmpsByRec)){
            names(conAmpsByRec)[[r]] <- vecFileNames[r]
          }
          
          return(conAmpsByRec)
          
        } 
        else if(tolower(tools::file_ext(input$files[[1, 'datapath']])) == "xls"){
          tbl_list <- lapply(input$files$datapath, read_excel, col_names = input$header)
          
          #Only used for Excel files
          i <- 1
          for(y in tbl_list){
            newVector[[i]] <- as.matrix(y)
            i <- i+1
          }
          
          i <- 1
          for(y in newVector){ #tbl_list
            conAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files$name){
            x = substring(x, 0, (stri_length(x)-4))
            vecFileNames <- c(vecFileNames, x)
          }
          
          for (r in 1:length(conAmpsByRec)){
            names(conAmpsByRec)[[r]] <- vecFileNames[r]
          }
          
          return(conAmpsByRec)
        }
        else{
          tbl_list <- lapply(input$files$datapath, read.table, header=input$header, sep=input$sep, quote=input$quote)
          
          i <- 1
          for(y in tbl_list){ #tbl_list
            conAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files$name){
            x = substring(x, 0, (stri_length(x)-4))
            vecFileNames <- c(vecFileNames, x)
          }

          for (r in 1:length(conAmpsByRec)){
            names(conAmpsByRec)[[r]] <- vecFileNames[r]
          }
          
          return(conAmpsByRec)
          }
        
      }
    # , 
    #   error=function(err) {
    #     message(err)
    #     return(NULL)
    #   })
    # }
  })
  
  ####################### TTX ##########################
  lst2 <- reactive({
    
    validate(need(input$files2 != "", ""))
    
    if (is.null(input$files2)) {
      return(NULL)
    } else {
      # tryCatch({
        
        colChoice = input$columns
        col = as.numeric(colChoice)
        
        if(tolower(tools::file_ext(input$files2[[1, 'datapath']])) == "xlsx"){
          tbl_list2 <- lapply(input$files2$datapath, read_excel, col_name = input$header)
          
          #Only used for Excel files 
          i <- 1
          for(y in tbl_list2){
            newVectorTTX[[i]] <- as.matrix(y)
            i <- i+1
          }
          
          i <- 1
          for(y in newVectorTTX){
            ttxAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files2$name){
            x = substring(x, 0, (stri_length(x) -5))
            vecFileNames2 <- c(vecFileNames2, x)    #create vector of just names of the files
          }
 
          for (r in 1:length(ttxAmpsByRec)){
            names(ttxAmpsByRec)[[r]] <- vecFileNames2[r]
          }
          
          return(ttxAmpsByRec)
          
        }
        else if(tolower(tools::file_ext(input$files2[[1, 'datapath']])) == "xls"){
          tbl_list2 <- lapply(input$files2$datapath, read_excel, col_name = input$header)
          
          #Only used for Excel files
          i <- 1
          for(y in tbl_list2){
            newVectorTTX[[i]] <- as.matrix(y)
            i <- i+1
          }
          
          i <- 1
          for(y in newVectorTTX){
            ttxAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files2$name){
            x = substring(x, 0, (stri_length(x) -4))
            vecFileNames2 <- c(vecFileNames2, x)    #create vector of just names of the files
          }
          
          for (r in 1:length(ttxAmpsByRec)){
            names(ttxAmpsByRec)[[r]] <- vecFileNames2[r]
          }
          
          return(ttxAmpsByRec)
          
        }
        else{
          tbl_list2 <- lapply(input$files2$datapath, read.table, header=input$header, sep=input$sep, quote=input$quote)
        
          i <- 1
          for(y in tbl_list2){
            ttxAmpsByRec[[i]] <- c(y[,col])
            i <- i+1
          }
          
          for(x in input$files2$name){
            x = substring(x, 0, (stri_length(x) -4))
            vecFileNames2 <- c(vecFileNames2, x)    #create vector of just names of the files
          }
          
          for(r in 1:length(ttxAmpsByRec)){
            names(ttxAmpsByRec)[[r]] <- vecFileNames2[r]
          }
          
          return(ttxAmpsByRec)
          }
        
      }
    # , 
    #   error=function(cond) {
    #     message("ERROR")
    #     return(NULL)
    #   })
    # }
  })
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ####################################### GET MAX X & Y ###################################
  max_x <- reactive({
    if(max(samplingCon()) > max(samplingTTX())){
      updateSliderInput(session = session, inputId = "xrange", value = c(0, round_any(max(samplingCon()), 5, ceiling)), 
                        min = 0, max = round_any(max(samplingCon()), 5, ceiling), step = 5)
      
      # if(input$Sampling2 == 'CS'){
        updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(samplingCon()), 5, ceiling)), 
                          min = 0, max = round_any(max(samplingCon()), 5, ceiling), step = 5)
      # } else if(input$Sampling2 == 'RO'){
      #   
      #   if(max(samplingCon()) > max(rankOrderCon())){
      #     updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(samplingCon()), 5, ceiling)), 
      #                       min = 0, max = round_any(max(samplingCon()), 5, ceiling), step = 5)
      #   } else{
      #     updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(rankOrderCon()), 5, ceiling)), 
      #                       min = 0, max = round_any(max(rankOrderCon()), 5, ceiling), step = 5)
      #   }
      #   
      # }
      
      return(round_any(max(samplingCon()), 5, ceiling))
    } else{
      updateSliderInput(session = session, inputId = "xrange", value = c(0, round_any(max(samplingTTX()), 5, ceiling)), 
                        min = 0, max = round_any(max(samplingTTX()), 5, ceiling), step = 5)
      
      # if(input$Sampling2 == 'CS'){
        updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(samplingTTX()), 5, ceiling)), 
                          min = 0, max = round_any(max(samplingTTX()), 5, ceiling), step = 5)
      # } else if(input$Sampling2 == 'RO'){
      #   
      #   if(max(samplingCon()) > max(rankOrderCon())){
      #     updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(samplingCon()), 5, ceiling)), 
      #                       min = 0, max = round_any(max(samplingCon()), 5, ceiling), step = 5)
      #   } else{
      #     updateSliderInput(session = session, inputId = "scalingxrange", value = c(0, round_any(max(rankOrderCon()), 5, ceiling)), 
      #                       min = 0, max = round_any(max(rankOrderCon()), 5, ceiling), step = 5)
      #   }
      #   
      # }
      
      return(round_any(max(samplingTTX()), 5, ceiling))
    }
  })
  
  max_y <- reactive({
    
    binNum = as.numeric(input$bins)
    binsize_con = ceiling((ceiling(max(samplingCon())))/binNum)
    binsize_ttx = ceiling((ceiling(max(samplingTTX())))/binNum)
    
    histCon = hist(samplingCon(), breaks = binsize_con, freq = FALSE)
    histTTX = hist(samplingTTX(), breaks = binsize_ttx, freq = FALSE)
    
    if(max(histCon$density) > max(histTTX$density)){
      return(signif(max(histCon$density), 2))
    }else{
      return(signif(max(histTTX$density), 2))
    }
    
    # colChoice = input$columns
    # col = as.numeric(colChoice)
    # binNum = as.numeric(input$bins)
    # 
    # binsize_con = floor(max(samplingCon())/binNum)
    # con <- hist(samplingCon(), breaks = binsize_con, freq = TRUE, col = "#808080", border = "white",
    #      xlab = paste("Amplitude", input$unit, sep = " "), main = "Control", xlim = c(0, max_x()))
    # #print(con$counts)
    # 
    # binsize_ttx = floor(max(samplingTTX())/binNum)
    # ttx <- hist(samplingTTX(), breaks = binsize_ttx, col = "#808080", border = "white",
    #      xlab = paste("Amplitude", input$unit, sep = " "), main = "TTX", xlim = c(0, max_x()))
    # #print(ttx$counts)
    # 
    # if(max(con$counts) > max(ttx$counts)){
    #   return(max(con$counts))
    # }
    # else{
    #   return(max(ttx$counts))
    # }
  })
  
  linearMaxYVal <- reactive({
    lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
    intercept <- lmRanked$coefficients[1]
    slope <- lmRanked$coefficients[2]
    
    max_yval = ceiling(max(predict(lmRanked)))
    #print(max_yval)
    return(max_yval)
  })
  
  rankOrderMaxY <- reactive({
    rankorder_y = round_any(ceiling(max(rankOrderTTX())), 5, ceiling)
    ratio_y = round_any(max((rankOrderTTX()/rankOrderCon())), 0.1, ceiling)
    y_min = round_any(min((rankOrderTTX()/rankOrderCon())), 0.1, floor)
    
    updateSliderInput(session = session, inputId = "scalingyrange", value = c(0, rankorder_y), 
                      min = 0, max = rankorder_y, step = 5)
    updateSliderInput(session = session, inputId = "scalingyrangeRatio", value = c(y_min, ratio_y), 
                      min = 0, max = ratio_y, step = .1)
  })
  
  # observe({
  #   if (is.null(max_x())) {
  #     #do nothing
  #   } else {
  #     updateSliderInput(session = session, inputId = "xrange", value = c(0, max_x()), min = 0, max = max_x())
  #   }
  # })
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ##################################### OUTPUT ############################################
  output$text1 <- renderPrint({
    lst1()
  })
  
  output$text2 <- renderPrint({
    lst2()
  })
  
  flagControl <- FALSE
  flagTreatment <- FALSE
  
  output$histControl <- renderPlot({

    if(flagControl == FALSE){
      colChoice = input$columns
      col = as.numeric(colChoice)
      
      binNum = as.numeric(input$bins) #input from the "bins" radio buttons
      binsize = ceiling((ceiling(max(samplingCon())))/binNum)
      
      hist1 <- hist(samplingCon(), breaks = binsize, freq = FALSE, col = "#808080", border = "white",
                    xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
                    main = paste(length(numberOfConSampled()), "control cells sampled via", input$Sampling, "at", input$nEvents, "samples per cell"), 
                    xlim = c(input$xrange[1], input$xrange[2]),
                    ylim = c(0, max_y()))

      axis(1, at = seq(0, max_x(), by = 5))
    }else if(flagControl == TRUE){
      updateTextInput(session = session, inputId = "histControl_min_x", value = input$histControl_min_x)
      updateTextInput(session = session, inputId = "histControl_max_x", value = input$histControl_max_x)
      updateTextInput(session = session, inputId = "histControl_interval_x", value = input$histControl_interval_x)
      updateTextInput(session = session, inputId = "histControl_interval_y", value = input$histControl_interval_y)
      
      colChoice = input$columns
      col = as.numeric(colChoice)
      
      binNum = as.numeric(input$bins) #input from the "bins" radio buttons
      binsize = ceiling((ceiling(max(samplingCon())))/binNum)
      
      hist1 <- hist(samplingCon(), breaks = binsize, freq = FALSE, col = "#808080", border = "white",
                    xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
                    main = paste(length(numberOfConSampled()), "control cells sampled via", input$Sampling, "at", input$nEvents, "samples per cell"), 
                    xlim = c(input$xrange[1], input$xrange[2]),
                    ylim = c(0, max_y()))
      
      axis(1, at = seq(input$histControl_min_x, input$histControl_max_x, by = input$histControl_interval_x))
    }
  })
  
  observeEvent(input$histControl_dblclick, {
    showModal(modalDialog(
      title = tags$h4("Histogram: Control"),
      verticalLayout(
        # fluidRow(
        #   column(width = 6, tags$h4(renderText("X-Axis"))),
        #   column(width = 6, tags$h4(renderText("Y-Axis")))
        # ),
        fluidRow(
          column(width = 6, textInput(inputId = "histControl_min_x", "Min x-axis", value = 0, width = '50%')),
          column(width = 6, textInput(inputId = "histControl_max_y", "Max x-axis", value = 100, width = '50%'))
        ),
        # fluidRow(
        #   column(width = 6, textInput(inputId = "histControl_max_y", "Max", value = 1, width = '50%'))
        # ),
        fluidRow(
          column(width = 6, textInput(inputId = "histControl_interval_x", "Interval x-axis", value = 1, width = '50%')),
          column(width = 6, textInput(inputId = "histControl_interval_y", "Interval y-axis", value = 0.1, width = '50%'))
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("histControl_ok", "OK")
      ),
      easyClose = TRUE
      
    ))
  })
  
  observeEvent(input$histControl_ok, {
    updateSliderInput(session = session, inputId = "xrange", value = c(input$histControl_min_x, 
                                                                       input$histControl_max_x), 
                      min = input$histControl_min_x, max = input$histControl_max_x, 
                      step = input$histControl_interval_x)
    # updateTextInput(session = session, inputId = "histControl_min_x", value = input$histControl_min_x)
    # updateTextInput(session = session, inputId = "histControl_max_x", value = input$histControl_max_x)
    # updateTextInput(session = session, inputId = "histControl_interval_x", value = input$histControl_interval_x)
    # updateTextInput(session = session, inputId = "histControl_interval_y", value = input$histControl_interval_y)
    flagControl <- TRUE
    
    removeModal()
  })
  
  output$histTTX <- renderPlot({
    
    colChoice = input$columns
    col = as.numeric(colChoice)
    
    binNum = as.numeric(input$bins)
    binsize = ceiling((ceiling(max(samplingTTX())))/binNum)
    
    hist(samplingTTX(), breaks = binsize, freq = FALSE, col = "#808080", border = "white",
         xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         main = paste(length(numberOfTTXSampled()) ,"treatment cells sampled via", input$Sampling, "at", input$nEvents, "samples per cell"), 
         xlim = c(input$xrange[1], input$xrange[2]),
         ylim = c(0, max_y()))
    axis(1, at = seq(0, max_x(), by = 5))
  })
  
  observeEvent(input$histTTX_ok, {
    # updateSliderInput(session = session, inputId = "xrange", value = c(input$histTTX_min_x, input$histTTX_max_x), 
    #                   min = input$histTTX_min_x, max = input$histTTX_max_x, step = 5)
    flagTreatment <- TRUE
    removeModal()
  })
  
  histTTX_interval_x_update <- reactive({
    if(is.null(input$histTTX_interval_x)){
      interval <- input$histTTX_interval_x
    } else{
      interval <- 5
    }
    
    return(interval)
  })
  
  observeEvent(input$histTTX_dblclick, {
    showModal(modalDialog(
      title = tags$h4("Histogram: Treatment"),
      verticalLayout(
        fluidRow(
          column(width = 6, tags$h4(renderText("X-Axis"))),
          column(width = 6, tags$h4(renderText("Y-Axis")))
        ),
        fluidRow(
          column(width = 6, textInput(inputId = "histTTX_min_x", "Min", value = 0, width = '50%')),
          column(width = 6, textInput(inputId = "histTTX_min_y", "Min", value = 0, width = '50%'))
        ),
        fluidRow(
          column(width = 6, textInput(inputId = "histTTX_max_x", "Max", value = 100, width = '50%')),
          column(width = 6, textInput(inputId = "histTTX_max_y", "Max", value = 1, width = '50%'))
        ),
        fluidRow(
          column(width = 6, textInput(inputId = "histTTX_interval_x", "Interval", value = 1, width = '50%')),
          column(width = 6, textInput(inputId = "histTTX_interval_y", "Interval", value = 0.1, width = '50%'))
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("histTTX_ok", "OK")
      ),
      easyClose = TRUE
    ))
  })
  
  output$cdfPlot <- renderPlot({
    
    plot(samplingCon(),samplingConYVals(),
         type = "l", xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), ylab = "Cumulative Fraction", 
         xlim = c(input$xrange[1], input$xrange[2]), lwd=2)
    
    # percentile lines for cumulative plots
    fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
    dataPercs <- quantile(samplingCon(),fractions) # Values of those percentiles in the data
    
    for (i in 1:length(fractions)) {
      lines(c(dataPercs[i],dataPercs[i]),c(0,fractions[i])) # vertical line
      lines(c(0,dataPercs[i]),c(fractions[i],fractions[i])) # horizontal line
    }
    
    axis(1, at = seq(0, max_x(), by = 5))
    
    # Add TTX data to plot in a different color
    lines(samplingTTX(), samplingTTXYVals(), col="darkorange2", lwd=2)
    legend("bottomright", legend=c("Control", "Treatment"), col=c("black", "darkorange2"), lty = c(1, 1), lwd = c(2, 2), box.lty = 0)
    
  })
  
  output$cdfPlotScaling <- renderPlot({
    
    if(input$Sampling2 == 'RO'){
      max_x()
      
      scaledTTXAmps <- rankOrderScaledTTXAmps()
      
      plot(samplingCon(), samplingConYVals(),type="l", axes=FALSE, lwd = 2,
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
           ylab = "Cumulative Fraction")
      
      # percentile lines for cumulative plots
      fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
      dataPercs <- quantile(samplingCon(),fractions) # Values of those percentiles in the data
      
      for (i in 1:length(fractions)) {
        lines(c(dataPercs[i],dataPercs[i]),c(0,fractions[i])) # vertical line
        lines(c(0,dataPercs[i]),c(fractions[i],fractions[i])) # horizontal line
      }
      
      lines(samplingTTX(),samplingTTXYVals(),col="darkorange2", lwd = 2)
      lines(scaledTTXAmps,samplingTTXYVals(),col="darkorange2",lty=2, lwd = 2)
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
      legend("bottomright",c("Control","Scaled Treatment","Treatment"),
             col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1),bty="n",box.lty = 0, 
             xpd=TRUE,seg.len=0.9)
    } else if(input$Sampling2 == 'CS'){
      plot(samplingCon(),samplingConYVals(),type="l",axes=FALSE, lwd = 2,
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
           ylab = "Cumulative Fraction")
      
      # percentile lines for cumulative plots
      fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
      dataPercs <- quantile(samplingCon(),fractions) # Values of those percentiles in the data
      
      for (i in 1:length(fractions)) {
        lines(c(dataPercs[i],dataPercs[i]),c(0,fractions[i])) # vertical line
        lines(c(0,dataPercs[i]),c(fractions[i],fractions[i])) # horizontal line
      }
      
      lines(samplingTTX(),samplingTTXYVals(),col="darkorange2", lwd = 2)
      lines(scaledCon(),samplingConYVals(),col = "darkorange2", lty=2, lwd = 2)
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
      legend("bottomright",legend=c("Control","Scaled Control","Treatment"),
             col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1), bty="n", box.lty = 0,
             xpd=TRUE, seg.len=0.9)
    } else if(input$Sampling2 == 'IT'){
      scaledTTXIterative <- iterativerScaledTTXAmps()
      
      plot(samplingCon(),samplingConYVals(),type="l",axes=FALSE, lwd = 2,
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
           ylab = "Cumulative Fraction")
      
      # percentile lines for cumulative plots
      fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
      dataPercs <- quantile(samplingCon(),fractions) # Values of those percentiles in the data
      
      for (i in 1:length(fractions)) {
        lines(c(dataPercs[i],dataPercs[i]),c(0,fractions[i])) # vertical line
        lines(c(0,dataPercs[i]),c(fractions[i],fractions[i])) # horizontal line
      }
      
      lines(samplingTTX(),samplingTTXYVals(),col="darkorange2", lwd = 2)
      lines(scaledTTXIterative,samplingTTXYVals(),col = "darkorange2",lty=2, lwd = 2)
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
      legend("bottomright",legend=c("Control","Scaled Treatment","Treatment"),
             col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1), bty="n", box.lty = 0,
             xpd=TRUE, seg.len=0.9)
    }
  })
  
  output$rankOrderLinearPlot <- renderPlot({
    if(input$Sampling2 == 'RO'){
      
      max_x()
      rankOrderMaxY()
      
      lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
      intercept <- lmRanked$coefficients[1]
      slope <- lmRanked$coefficients[2]
      
      plot(rankOrderCon(), rankOrderTTX(), axes=FALSE,
           xlab = paste("Control (", input$unit, ")", sep = ""), 
           ylab = paste("Treatment (", input$unit, ")", sep = ""),
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           ylim = c(input$scalingyrange[1], input$scalingyrange[2]), cex = 1.25)

      # percentile lines for ranked/linear plots - this assumes the control data will
      # be on the x axis and treatment data will be on the y axis
      fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
      conPercs <- quantile(rankOrderCon(),fractions) # need the percentiles for both
      trtPercs <- quantile(rankOrderTTX(),fractions) # data sets being plotted
      
      for (i in 1:length(fractions)) {
        lines(c(conPercs[i],conPercs[i]),c(0,trtPercs[i])) # vertical line
        lines(c(0,conPercs[i]),c(trtPercs[i],trtPercs[i])) # horizontal
      }
      
      lines(rankOrderCon(),fitted(lmRanked), col = 'darkorange2', lty = 2, lwd = 2)
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
      abline(a=0, b=1, col = 'gray', lwd = 2)
      
      if(intercept >= 0){
        legend("bottomright", legend=c(paste("y = ", signif(slope, 4), "x + ", signif(intercept, 3), sep = ""), 
                                       "Identity"), col=c('darkorange2', 'gray'), lty = c(2, 1), lwd = 2,  bty="n", box.lty = 0, xpd=TRUE, seg.len=0.9)
      }else if(intercept < 0){
        legend("bottomright", legend=c(paste("y = ", signif(slope, 4), "x ", signif(intercept, 3), sep = ""), 
                                       "Identity"), col=c('darkorange2', 'gray'), lty = c(2, 1), lwd = 2,  bty="n", box.lty = 0, xpd=TRUE, seg.len=0.9)
      }
      
    } else if(input$Sampling2 == 'CS'){
      #histogram
      conGEVD <- conGEVD()
      pooledShape <- pooledShape()
      distXVals <- seq(0,100,by=0.1)
      conGEVDLine <- dgev(distXVals,conGEVD$estimate[1],conGEVD$estimate[2],pooledShape)
      
      binNum = as.numeric(input$bins) #input from the "bins" radio buttons
      binsize = ceiling((ceiling(max(samplingCon())))/binNum)
      
      hist(samplingCon(), breaks=seq(0,200,by=2), freq=FALSE, axes=FALSE, lwd=2, 
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           ylim=c(0, (max_y()+0.02)), 
           xlab= paste(input$characteristic, " (", input$unit, ")", sep = ""),ylab="Density",main=NA)
      axis(1, at = seq(0, max_x(), by = 10))
      lines(distXVals,conGEVDLine,lwd=2,col="black")
      text(40,0.07,bquote(mu[con] == .(signif(conGEVD$estimate[1],2))))
      text(40,0.06,bquote(sigma[con] == .(signif(conGEVD$estimate[2],2))))
      text(40,0.05,bquote(xi[shared] == .(signif(pooledShape,2))))
      axis(2,at=seq(0, max_y(), by = 0.02),lwd=1)
      #axis(1,lwd=1)
      #box(lwd=1)
      legend("topright",c("GEV"),col=c("black"),lwd=c(2),bty="n")
    } else if(input$Sampling2 == 'IT'){
      #testFactors (x-axis) vs pValsByFactor (y-axis) 
      result <- iterative()
      testFactors <- setTestFactors()
      plot(testFactors,result[[5]], axes = FALSE, type = "b", xlab = "Scaling Factor", ylab = "p-value")
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
    }
  })
  
  output$rankOrderRatioPlot <- renderPlot({
    if(input$Sampling2 == 'RO'){
      
      max_x()
      rankOrderMaxY()
      
      plot(rankOrderCon(),(rankOrderTTX()/rankOrderCon()), axes = FALSE,
           xlab= paste("Control (", input$unit, ")", sep = ""),
           ylab="Treatment / Control",
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]), 
           ylim = c(input$scalingyrangeRatio[1], input$scalingyrangeRatio[2]), cex = 1.25)
      
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
      
      # # percentile lines for ratio plots - this assumes that the ratio will start at
      # # 1 and increase so it might need to be adjusted for ratios less than 1
      # fractions <- c(0.25,0.5,0.75) # Fractions at which to draw lines
      # dataPercs <- quantile(rankOrderCon(),fractions)
      # lineHeight <- 1.5 # position on the y-axis of the end of the line
      # lineOffset <- 0.05 # additional length added to alternate lines
      # 
      # for (i in 1:length(fractions)) {
      #   # add offset height to alternate lines so percentage labels don't overlap
      #   # this was originally for small-size figures so the values of the height
      #   # and offset might need tweaking
      #   yVal <- lineHeight+lineOffset*(i %% 2)
      # 
      #   # draw vertical line
      #   lines(c(dataPercs[i],dataPercs[i]),c(1,yVal),col="black")
      # 
      #   # add percentage label above line: scalar values are the offset between the
      #   # exact end of the line and where the label is drawn, these can be adjusted
      #   # as needed. The cex parameter decreases the font size of the label from
      #   # the figure's default, I think as a fraction (so the text drawn by this
      #   # code will be 75% of the size of the default text size in the figure)
      #   text(percLines[i]+1.1,yVal+0.03,paste(fractions[i]*100,"%",sep=""),cex=0.75)
      # }
      
    } else if(input$Sampling2 == 'CS'){
      #histogram
      ttxGEVD <- ttxGEVD()
      pooledShape <- pooledShape()
      distXVals <- seq(0,100,by=0.1)
      ttxGEVDLine <- dgev(distXVals,ttxGEVD$estimate[1],ttxGEVD$estimate[2],pooledShape)
      
      binNum = as.numeric(input$bins) #input from the "bins" radio buttons
      binsize = ceiling((ceiling(max(samplingTTX())))/binNum)
      
      hist(samplingTTX(), breaks=seq(0,200,by=2),freq=FALSE,axes=FALSE,lwd=2,
           xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
           ylim= c(0, (max_y()+0.02)),
           xlab= paste(input$characteristic," (", input$unit, ")", sep = ""),
           ylab="Density",main=NA)
      axis(1, at = seq(0, max_x(), by = 10))
      lines(distXVals,ttxGEVDLine,lwd=2,col="darkorange2")
      text(40,0.07,bquote(mu[treatment] == .(signif(ttxGEVD$estimate[1],2))))
      text(40,0.06,bquote(sigma[treatment] == .(signif(ttxGEVD$estimate[2],2))))
      text(40,0.05,bquote(xi[shared] == .(signif(pooledShape,2))))
      axis(2,at=seq(0, max_y(), by = 0.02),lwd=1)
      #axis(1,lwd=1)
      #box(lwd=1)
      legend("topright",c("GEV"),col=c("darkorange2"),lwd=c(2),bty="n")
    } else if(input$Sampling2 == 'IT'){
      #testFactors vs fracDiscarded
      result <- iterative()
      testFactors <- setTestFactors()
      plot(testFactors,result[[6]], axes = FALSE, type = "b", xlab = "Scaling Factor", ylab = "Fraction Discarded")
      axis(2,lwd=1)
      axis(1,lwd=1)
      box(lwd=1)
    }
  })
  
  # all x-axis for CS, for hist, match largest y-axis
  
  #Calculates R2 or multiplicative factor (CS), or scaling factor (IT)
  calcR2_MultFactor <- reactive({
    if(input$Sampling2 == 'RO'){
      lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
      R2 <- summary(lmRanked)$r.squared
      print(summary(lmRanked)$r)
      roundedR2 <- signif(R2, 3)
      return(roundedR2)
      paste("R", "\u00B2", ": ", roundedR2, sep = "")
    } else if(input$Sampling2 == 'CS'){
      roundedMultFactor <- signif(multFactor(), 4)
      return(roundedMultFactor)
    } else if(input$Sampling2 == 'IT'){
      result <- iterative()
      roundedMultFactor <- signif(result[[2]], 4)
      return(roundedMultFactor)
    }
  })
  
  #Prints out the R2 or multiplicative factor or scaling factor to the screen
  output$R2 <- renderText({
    if(input$Sampling2 == 'RO'){
      paste("R", "\u00B2", ": ", calcR2_MultFactor(), sep = "")
    } else if(input$Sampling2 == 'CS'){
      paste("Multiplicative Factor: ", calcR2_MultFactor(), sep = "")
    } else if(input$Sampling2 == 'IT'){
      paste("Scaling Factor: ", calcR2_MultFactor(), sep = "")
    }
  })
  
  #Calculates slope or additive factor (could be included with function below)
  calcSlope_AddFactor <- reactive({
    if(input$Sampling2 == 'RO'){
      lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
      slope <- lmRanked$coefficients[2]
      roundedSlope <- signif(slope, 4)
      return(roundedSlope)
    } else if(input$Sampling2 == 'CS'){
      roundedAddFactor <- signif(addFactor(), 4)
      return(roundedAddFactor)
    } else if(input$Sampling2 == 'IT'){
      result <- iterative()
      bestFraction <- signif(result[[4]], 4)
      return(bestFraction)
    }
  })
  
  #Prints out the slope or additive factor or fraction to the screen
  output$Slope <- renderText({
    if(input$Sampling2 == 'RO'){
      paste("Slope:", calcSlope_AddFactor(), sep = " ")
    } else if(input$Sampling2 == 'CS'){
      paste("Additive Factor: ", calcSlope_AddFactor(), sep = "")
    } else if(input$Sampling2 == 'IT'){
      paste("Fraction Discarded: ", calcSlope_AddFactor(), sep = "")
    }
  })
  
  #Calculates intercept
  calcIntercept <- reactive({
    lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
    intercept <- lmRanked$coefficients[1]
    roundedIntercept <- signif(intercept, 3)
    return(roundedIntercept)
  })
  
  #Prints out intercept to the screen
  output$Intercept <- renderText({
    if(input$Sampling2 == 'RO'){
      paste("Intercept:", calcIntercept(), sep = " ")
    }
  })
  
  #Calculates KS p-value
  calcKSPVal <- reactive({
    if(input$Sampling2 == 'RO'){
      #lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
      ksRes <- ks.test(rankOrderScaledTTXAmps(),samplingCon())
      ksPVal <- ksRes$p.value
      roundedPVal <- signif(ksPVal,3)
      return(roundedPVal)
    } else if(input$Sampling2 == 'CS'){
      ksRes <- ks.test(scaledCon(), samplingTTX())
      ksPVal <- ksRes$p.value
      roundedPVal <- signif(ksPVal,3)
      return(roundedPVal)
    }
  })
  
  #Prints out KS p-value to the screen
  output$KSPVal <- renderText({
    if(input$Sampling2 == 'RO'){
      paste("p-value, Kolmogorov-Smirnov test:", calcKSPVal(), sep = "\n")
    } else if(input$Sampling2 == 'CS'){
      paste("p-value, Kolmogorov-Smirnov test:", calcKSPVal(), sep = "\n")
    } else if(input$Sampling2 == 'IT'){
      result <- iterative()
      roundedPVal <- signif(result[[3]],3)
      paste("p-value, Kolmogorov-Smirnov test:", roundedPVal, sep = "\n")
    }
  })
  
  #Calculates AD p-value
  calcADPVal <- reactive({
    if(input$Sampling2 == 'RO'){
      #lmRanked <- lm(rankOrderTTX() ~ rankOrderCon())
      adRes <- ad.test(rankOrderScaledTTXAmps(),samplingCon())
      adPVal <- adRes$ad[1,3]
      roundedPVal <- signif(adPVal,3)
      return(roundedPVal)
    } else if(input$Sampling2 == 'CS'){
      adRes <- ad.test(scaledCon(), samplingTTX())
      adPVal <- adRes$ad[1,3]
      roundedPVal <- signif(adPVal,3)
      return(roundedPVal)
    } else if(input$Sampling2 == 'IT'){
      adRes <- ad.test(iterativerScaledTTXAmps(), samplingCon())
      adPVal <- adRes$ad[1,3]
      roundedPVal <- signif(adPVal,3)
      return(roundedPVal)
    }
  })
  
  #Prints out AD p-value to the screen
  output$ADPVal <- renderText({
    if(input$Sampling2 == 'RO'){
      paste("p-value, Anderson-Darling test:", calcADPVal(), sep = "\n")
    } else if(input$Sampling2 == 'CS'){
      paste("p-value, Anderson-Darling test:", calcADPVal(), sep = "\n")
    } else if(input$Sampling2 == 'IT'){
      paste("p-value, Anderson-Darling test:", calcADPVal(), sep = "\n")
    }
  })
  
  output$MeansCON <- renderText({
    conAmpsByRec = numberOfConSampled()
    
    i = 1
    means = vector()
    for(x in conAmpsByRec){
      means[i] = mean(x)
      i = i+1
    }
    
    paste(names(conAmpsByRec), "\t", signif(means, 3), "\n", sep = "")
    
  })
  
  output$MeansTTX <- renderText({
    ttxAmpsByRec = numberOfTTXSampled()
    
    j = 1
    means_ttx = vector()
    for(x in ttxAmpsByRec){
      means_ttx[j] = mean(x)
      j = j+1
    }
    
    paste(names(ttxAmpsByRec), "\t", signif(means_ttx, 3), "\n", sep = "")
    
  })
  
  output$RemovedCellsCon <- renderText({
    con = conFilesRemoved()
    
    paste(con, "\n", sep = "")
    
  })
  
  output$RemovedCellsTTX <- renderText({
    ttx = ttxFilesRemoved()
    
    paste(ttx, "\n", sep = "")
    
  })
  
  output$logPlot <- renderPlot({

    conVals = rankOrderCon()
    ttxVals = rankOrderTTX()

    lm_test = lm(log(ttxVals) ~ log(conVals))

    intercept <- lm_test$coefficients[1]
    slope <- lm_test$coefficients[2]
    scaledLog <- ((log(ttxVals))-intercept)/slope
    
    conYVals <- seq(1/length(conVals), 1,by=1/length(conVals))
    ttxYVals <- seq(1/length(ttxVals), 1,by=1/length(ttxVals))
    scaledYVals <- seq(1/length(scaledLog), 1,by=1/length(scaledLog))
    
    plot(conVals,conYVals,type = "l", xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         ylab = "Cumulative Fraction", lwd=2)
    
    lines(ttxVals,ttxYVals,col="darkorange2", lwd = 2)
    lines(exp(scaledLog), exp(scaledYVals), col = "darkorange2", lty = 2, lwd = 2)
    #lines(log(x_vals), log(x_vals) ^ lm_test$coefficients[2], col = "red")
    
  })
  
  output$plotResiduals <- renderPlot({
    conVals = rankOrderCon()
    #x_vals = x_vals[0:4000]
    ttxVals = rankOrderTTX()
    #y_vals = y_vals[0:4000]
    original_y_vals = samplingCon()
    
    lm_test = lm(log(ttxVals) ~ log(conVals))
    intercept <- lm_test$coefficients[1]
    slope <- lm_test$coefficients[2]
    scaledLog <- (log(ttxVals)-intercept)/slope
    #residuals = resid(lm_test)
    #residuals = (original_y_vals-(exp(fitted(lm_test))))
    residuals = (conVals-exp(scaledLog))
    # print(x_vals[1:40])
    # print(fitted(lm_test)[1:40])
    # print(exp(fitted(lm_test))[1:40])
    plot(residuals, ylab = "Residuals")
    #print("Log")
    #print(summary(resid(lm_test)))
    abline(h=0)
  })
  
  output$plotnls <- renderPlot({
    conVals = samplingCon()
    ttxVals = samplingTTX()

    conShortVals = rankOrderCon()
    ttxShortVals = rankOrderTTX()
    
    fit_nls = nls(formula = (ttxShortVals ~ (conShortVals ^ b)), start = c(b = 2), trace = F)

    print("nls coefficient")
    print(coef(fit_nls))
    
    conYVals <- seq(1/length(conVals), 1,by=1/length(conVals))
    ttxYVals <- seq(1/length(ttxVals), 1,by=1/length(ttxVals))
    scaledCon <- conVals^coef(fit_nls)
    scaledConYVals <- seq(1/length(scaledCon), 1,by=1/length(scaledCon))
    
    plot(conVals, conYVals, main = "Exponentiation", type="l", axes=FALSE, lwd = 2,
         xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
         xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         ylab = "Cumulative Fraction")
    lines(ttxVals, ttxYVals, col = "darkorange2", lwd = 2)
    lines(scaledCon, scaledConYVals, col = "darkorange2", lty = 2, lwd = 2)
    axis(2,lwd=1)
    axis(1,lwd=1)
    box(lwd=1)
    legend("bottomright",c("Control","Scaled Control","Treatment"),
           col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1),bty="n",box.lty = 0, 
           xpd=TRUE,seg.len=0.9)
    
    ksResults <- ks.test(scaledCon, ttxVals)
    ksPVal <- ksResults$p.value
    roundedPValKS <- signif(ksPVal,3)
    
    adRes <- ad.test(scaledCon, ttxVals)
    adPVal <- adRes$ad[1,3]
    roundedPValAD <- signif(adPVal, 3)
    
    text(80,0.7,paste("K-S test p-value:", roundedPValKS, sep = " "))
    text(80,0.6,paste("A-D test p-value:", roundedPValAD, sep = " "))
    
  })
  
  output$plotResidualsNls <- renderPlot({
    #nls
    conVals = rankOrderCon()
    ttxVals = rankOrderTTX()

    fit_nls = nls(formula = (conVals ~ (ttxVals ^ b)), start = c(b = 2), trace = F)
    
    scaledCon <- ttxVals^coef(fit_nls)
    residuals = (conVals-scaledCon)
    
    #residuals
    plot(residuals, ylim = c(-10,10), ylab = "Error")
    abline(h=0)
    #print("NLS")
    #print(summary(resid(fit_nls)))
  })
  
  output$plotRO <- renderPlot({
    max_x()
    
    scaledTTXAmps <- rankOrderScaledTTXAmps()
    
    plot(samplingCon(), samplingConYVals(), type="l", main = "Rank-Order", axes=FALSE, lwd = 2,
         xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
         xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         ylab = "Cumulative Fraction")

    lines(samplingTTX(),samplingTTXYVals(),col="darkorange2", lwd = 2)
    lines(scaledTTXAmps,samplingTTXYVals(),col="darkorange2",lty=2, lwd = 2)
    axis(2,lwd=1)
    axis(1,lwd=1)
    box(lwd=1)
    legend("bottomright",c("Control","Scaled Treatment","Treatment"),
           col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1),bty="n",box.lty = 0, 
           xpd=TRUE,seg.len=0.9)
    
    ksResults <- ks.test(scaledTTXAmps, samplingCon())
    ksPVal <- ksResults$p.value
    roundedPValKS <- signif(ksPVal,3)
    
    adRes <- ad.test(scaledTTXAmps, samplingCon())
    adPVal <- adRes$ad[1,3]
    roundedPValAD <- signif(adPVal, 3)
    
    text(80,0.7,paste("K-S test p-value:", roundedPValKS, sep = " "))
    text(80,0.6,paste("A-D test p-value:", roundedPValAD, sep = " "))
  })
  
  output$plotROResid<-renderPlot({
    scaledTTXAmps <- rankOrderScaledTTXAmps()
    residuals = (rankOrderCon()-scaledTTXAmps)
    
    plot(residuals, ylim = c(-10,10))
    #print("Rank Order")
    #print(summary(resid(lmRanked)))
    abline(h=0)
  })
  
  output$plotCS <- renderPlot({
    plot(samplingCon(),samplingConYVals(),type="l", main = "Comparative Standardization", axes=FALSE, 
         lwd = 2, xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
         xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         ylab = "Cumulative Fraction")
    
    lines(samplingTTX(),samplingTTXYVals(),col="darkorange2", lwd = 2)
    lines(scaledCon(),samplingConYVals(),col = "darkorange2", lty=2, lwd = 2)
    axis(2,lwd=1)
    axis(1,lwd=1)
    box(lwd=1)
    legend("bottomright",legend=c("Control","Scaled Control","Treatment"),
           col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1), bty="n", box.lty = 0,
           xpd=TRUE, seg.len=0.9)
    
    ksRes <- ks.test(scaledCon(), samplingTTX())
    ksPVal <- ksRes$p.value
    roundedPValKS <- signif(ksPVal,3)
    
    adRes <- ad.test(scaledCon(), samplingTTX())
    adPVal <- adRes$ad[1,3]
    roundedPValAD <- signif(adPVal,3)
    
    text(80,0.7,paste("K-S test p-value:", roundedPValKS, sep = " "))
    text(80,0.6,paste("A-D test p-value:", roundedPValAD, sep = " "))
  })
  
  output$plotIT <- renderPlot({
    scaledTTXIterative <- iterativerScaledTTXAmps()
    
    plot(samplingCon(),samplingConYVals(),type="l", main = "Iterative", axes=FALSE, lwd = 2,
         xlim = c(input$scalingxrange[1], input$scalingxrange[2]),
         xlab = paste(input$characteristic, " (", input$unit, ")", sep = ""), 
         ylab = "Cumulative Fraction")
    
    lines(samplingTTX(),samplingTTXYVals(), col="darkorange2", lwd = 2)
    lines(scaledTTXIterative,samplingTTXYVals(),col = "darkorange2",lty=2, lwd = 2)
    axis(2,lwd=1)
    axis(1,lwd=1)
    box(lwd=1)
    legend("bottomright",legend=c("Control","Scaled Treatment","Treatment"),
           col=c("black","darkorange2","darkorange2"),lwd=c(2,2,2),lty=c(1,3,1), bty="n", box.lty = 0,
           xpd=TRUE, seg.len=0.9)
    
    result <- iterative()
    roundedPVal <- signif(result[[3]],3)
    
    adRes <- ad.test(iterativerScaledTTXAmps(), samplingCon())
    adPVal <- adRes$ad[1,3]
    roundedPValAD <- signif(adPVal,3)

    text(80,0.7,paste("K-S test p-value:", roundedPVal, sep = " "))
    text(80,0.6,paste("A-D test p-value:", roundedPValAD, sep = " "))
  })
  
  output$plotCompStandResidCon <- renderPlot({
    originalTTX <- samplingTTX()
    scaledControl <- scaledCon()
    res <- vector()
    
    #conYVals <- seq(1/length(samplingCon()), 1, by=1/length(samplingCon()))
    #HERE
    print("CS residuals: ")
    #resid(multFactor * conAmpEmpCdf + addFactor)
    print("LENGTH")
    print(length(originalTTX))
    print(length(scaledControl))
    #There are fewer ttx values than scaledcon values, also don't get how sampling is done
    for (m in 1:length(originalTTX)) {
      res[[m]] <- (originalTTX[[m]] - scaledControl[[m]])
    }

    #residuals = residuals(conGEVDLine)
   
    plot(originalTTX, res, ylim = c(-10,10))
    abline(h=0)
    print("Control GEVD")
    #print(summary(resid(conGEVDLine)))
  })
  
  # output$plotCompStandTTX <- renderPlot({
  #   ttxGEVD <- ttxGEVD()
  #   pooledShape <- pooledShape()
  #   distXVals <- seq(0,100,by=0.1)
  #   ttxGEVDLine <- dgev(distXVals,ttxGEVD$estimate[1],ttxGEVD$estimate[2],pooledShape)
  #   
  #   binNum = 1
  #   binsize_con = ceiling((ceiling(max(samplingTTX())))/binNum)
  #   
  #   hist(samplingTTX(), breaks=binsize_con, freq=FALSE, axes=FALSE, lwd=2, ylab="Density",main=NA)
  #   axis(1, at = seq(0, max_x(), by = 10))
  #   lines(distXVals,ttxGEVDLine,lwd=2,col="black")
  # })
  # 
  # output$plotCompStandResidTTX <- renderPlot({
  #   
  #   ttxGEVD <- ttxGEVD()
  #   pooledShape <- pooledShape()
  #   distXVals <- seq(0,100,by=0.1)
  #   param1 <- ttxGEVD$estimate[1]
  #   param2 <- ttxGEVD$estimate[2]
  #   ttxGEVDLine <- dgev(distXVals, param1, param2, pooledShape)
  #   
  #   # expResiduals <- resid(dgev(distXVals, param1, param2, pooledShape))
  #   # print("Residuals: ")
  #   # print(expResiduals)
  #   plot(samplingTTX(), samplingTTX())
  #   abline(h=0)
  #   # print("Treatment GEVD")
  #   # print(summary(resid(ttxGEVDLine)))
  # })
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
  
  ##################################### DOWNLOADS ############################################
  
  #*************SUMMARY GRAPHS********************
  
  #Download values after sampling--Summary Graphs
  output$downloadValues <- downloadHandler(
    filename = function(){
      paste("ValuesAfterSampling", ".txt", sep = "")
    }, 
    content = function(file){
      original_con = signif(samplingCon(), 3)
      original_ttx = signif(samplingTTX(), 3)
      length_con = length(original_con)
      length_ttx = length(original_ttx)
      
      if(length_con > length_ttx){
        fixed_length = rep(" ", length_con-length_ttx)
        new_vector = c(original_ttx, fixed_length)
        
        write(paste("Control and Treatment Values After ", input$Sampling, "\t", sep = ""), file)
        write(paste("Control", "\t", "Treatment", "\t", sep = ""), file, append = TRUE)
        write(paste(original_con, "\t", new_vector, "\t", sep = ""), file, append = TRUE) 
        
      } else if(length_ttx > length_con){
        fixed_length = rep(" ", length_ttx-length_con)
        new_vector = c(original_con, fixed_length)
        
        write(paste("Control and Treatment Values After ", input$Sampling, "\t", sep = ""), file)
        write(paste("Control", "\t", "Treatment", "\t", sep = ""), file, append = TRUE)
        write(paste(new_vector, "\t", original_ttx, "\t", sep = ""), file, append = TRUE) 
        
      } else{
        write(paste("Control and Treatment Values After ", input$Sampling, "\t", sep = ""), file)
        write(paste("Control", "\t", "Treatment", "\t", sep = ""), file, append = TRUE)
        write(paste(original_con, "\t", original_ttx, "\t", sep = ""), file, append = TRUE) 
      }
    }
  )
  
  #Download CDF values after sampling (4 columns)--Summary Graphs
  output$downloadSamplingCDFValues <- downloadHandler(
    filename = function(){
      paste("CDFValues", ".txt", sep = "")
    }, 
    content = function(file){
      
      original_con = signif(samplingCon(), 3)
      original_ttx = signif(samplingTTX(), 3)
      
      original_con_y_vals = format(signif(samplingConYVals(), 3), nsmall = 6)
      original_ttx_y_vals = format(signif(samplingTTXYVals(), 3), nsmall = 6)
      
      length_con = length(original_con)
      length_ttx = length(original_ttx)
      
      length_con_y_vals = length(original_con_y_vals)
      length_ttx_y_vals = length(original_ttx_y_vals)
      
      if(length_con > length_ttx){
        
        fixed_length = rep(" ", length_con-length_ttx)
        new_vector = c(original_ttx, fixed_length)
        
        fixed_length_y = rep("\t", length_con_y_vals-length_ttx_y_vals)
        new_vector_y = c(original_ttx_y_vals, fixed_length_y)
        
        write(paste("Control", "\t\t", "Treatment", "\t"), file)
        write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), file, append = TRUE)
        write(paste(original_con, "\t", original_con_y_vals, "\t", new_vector, "\t", 
                    new_vector_y, "\t", sep = ""), file, append = TRUE) 
        
      } else if(length_ttx > length_con){
        
        fixed_length = rep(" ", length_ttx-length_con)
        new_vector = c(original_con, fixed_length)
        
        fixed_length_y = rep("\t", length_ttx_y_vals-length_con_y_vals)
        new_vector_y = c(original_con_y_vals, fixed_length_y)
        
        write(paste("Control", "\t", "Treatment", "\t"), file)
        write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), file, append = TRUE)
        write(paste(new_vector, "\t", new_vector_y, "\t", original_ttx, "\t", 
                    original_ttx_y_vals, "\t", sep = ""), file, append = TRUE) 
        
      } else{
        
        write(paste("Control", "\t", "Treatment", "\t"), file)
        write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), file, append = TRUE)
        write(paste(original_con, "\t", original_con_y_vals, "\t", original_ttx, "\t", 
                    original_ttx_y_vals, "\r", sep = ""), file, append = TRUE) 
      }
    }
  )
  
  #*************SCALING PROCESSES********************
  
  #Download metrics
  output$download <- downloadHandler(
    filename = function(){
      if(input$Sampling2 == 'RO'){
        paste("RankOrder", ".txt", sep = "")
      } else if(input$Sampling2 == 'CS'){
        paste("ComparativeStandardization", ".txt", sep = "")
      }
    }, 
    content = function(file){
      if(input$Sampling2 == 'RO'){
        R2 <- paste("R", "\u00B2", sep = "")
        
        write(paste(R2, "\t", "Slope", "\t", "Intercept", "\t", "p-value, Kolmogorov-Smirnov test",
                    "\t", "p-value, Anderson-Darling test", "\r\n", sep = ""), file)
        write(paste(calcR2_MultFactor(), calcSlope_AddFactor(), calcIntercept(), "\t", calcKSPVal(), "\t\t", calcADPVal(), sep = "\t"), file, append = TRUE)
        
        
      } else if(input$Sampling2 == 'CS'){
        
        write(paste("Multiplicative Factor", "\t", "Additive Factor", "\t\t", "p-value, Kolmogorov-Smirnov test",
                    "\t", "p-value, Anderson-Darling test", "\r\n", sep = ""), file)
        write(paste(calcR2_MultFactor(), "\t", calcSlope_AddFactor(), "\t", calcKSPVal(), "\t\t\t", calcADPVal(), sep = "\t"), file, append = TRUE)
        
      }
    }
  )
  
  #Download CDF values (6 columns)--Scaling Processes
  output$downloadCDFScaling <- downloadHandler(
    filename = function(){
      if(input$Sampling2 == "RO"){
        paste("CDFValues_", "RankOrder" , ".txt", sep = "")
      }else if(input$Sampling2 == "CS"){
        paste("CDFValues_", "ComparativeStandardization" , ".txt", sep = "")
      }
      #iterative
    }, 
    content = function(file){
      
      original_con = signif(samplingCon(), 3)
      original_ttx = signif(samplingTTX(), 3)
      
      original_con_y_vals = format(signif(samplingConYVals(), 3), nsmall = 6)
      original_ttx_y_vals = format(signif(samplingTTXYVals(), 3), nsmall = 6)
      
      original_scaledTTXAmps <- signif(rankOrderScaledTTXAmps(), 3)
      original_scaledCon <- signif(scaledCon(), 3)
      
      length_con = length(original_con)
      length_ttx = length(original_ttx)
      
      length_con_y_vals = length(original_con_y_vals)
      length_ttx_y_vals = length(original_ttx_y_vals)
      
      if(length_con > length_ttx){
        
        if(input$Sampling2 == "RO"){
          fixed_length = rep(" ", length_con-length_ttx)
          new_vector = c(original_ttx, fixed_length)
          
          fixed_length_y = rep("\t", length_con_y_vals-length_ttx_y_vals) #the exact amount of padding to add
          new_vector_y = c(original_ttx_y_vals, fixed_length_y) #the y values of TTX padded with spaces
          new_scaling_vector = c(original_scaledTTXAmps, fixed_length_y) #the x values of scaled TTX padded with spaces
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Treatment", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(original_con, "\t", original_con_y_vals, "\t", new_vector, "\t",
                      new_vector_y, "\t", new_scaling_vector, "\t", new_vector_y,
                      "\t", sep = ""), file, append = TRUE)
        } else if(input$Sampling2 == "CS"){
          fixed_length = rep(" ", length_con-length_ttx)
          new_vector = c(original_ttx, fixed_length)
          
          fixed_length_y = rep("\t", length_con_y_vals-length_ttx_y_vals) #the exact amount of padding to add
          new_vector_y = c(original_ttx_y_vals, fixed_length_y) #the y values of TTX padded with spaces
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Control", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(original_con, "\t", original_con_y_vals, "\t", new_vector, "\t", 
                      new_vector_y, "\t", original_scaledCon, "\t", original_con_y_vals, 
                      "\t", sep = ""), file, append = TRUE)
        }
        
      } else if(length_ttx > length_con){
        
        if(input$Sampling2 == "RO"){
          fixed_length = rep(" ", length_ttx-length_con)
          new_vector = c(original_con, fixed_length)
          
          fixed_length_y = rep("\t", length_ttx_y_vals-length_con_y_vals) #the exact amount of padding to add
          new_vector_y = c(original_con_y_vals, fixed_length_y) #the y values of TTX padded with spaces
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Treatment", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(new_vector, "\t", new_vector_y, "\t", original_ttx, "\t", 
                      original_ttx_y_vals, "\t", original_scaledTTXAmps, "\t", original_ttx_y_vals, 
                      "\t", sep = ""), file, append = TRUE)
          
        } else if(input$Sampling2 == "CS"){
          fixed_length = rep(" ", length_ttx-length_con)
          new_vector = c(original_con, fixed_length)
          
          fixed_length_y = rep("\t", length_ttx_y_vals-length_con_y_vals) #the exact amount of padding to add
          new_vector_y = c(original_con_y_vals, fixed_length_y) #the y values of TTX padded with spaces
          new_scaling_vector = c(original_scaledCon, fixed_length_y) #the x values of scaled TTX padded with spaces
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Control", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(new_vector, "\t", new_vector_y, "\t", original_ttx, "\t",
                      original_ttx_y_vals, "\t", new_scaling_vector, "\t", new_vector_y,
                      "\t", sep = ""), file, append = TRUE)
        }
        
      } else{
        
        if(input$Sampling2 == "RO"){
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Treatment", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(original_con, "\t", original_con_y_vals, "\t", original_ttx, "\t", 
                      original_ttx_y_vals, "\t", original_scaledTTXAmps, "\t", original_ttx_y_vals,
                      "\t", sep = ""), file, append = TRUE) 
          
        } else if(input$Sampling2 == "CS"){
          
          write(paste("Control", "\t\t", "Treatment","\t\t", "Scaled Control", "\t"), file)
          write(paste("X", "\t", "Y", "\t", "X", "\t", "Y", "\t", "X", "\t", "Y", "\t", sep = ""), 
                file, append = TRUE)
          write(paste(original_con, "\t", original_con_y_vals, "\t", original_ttx, "\t", 
                      original_ttx_y_vals, "\t", original_scaledCon, "\t", original_con_y_vals,
                      "\t", sep = ""), file, append = TRUE) 
        }
        
      }
    }
  )
  
  #Download values from sclaing plots--Scaling Processes
  output$downloadScalingPlots <- downloadHandler(
    filename = function(){
      if(input$Sampling2 == "RO"){
        paste("PlotValues_", "RankOrder" , ".txt", sep = "")
      }else if( input$Sampling2 == "CS"){
        paste("PlotValues_", "ComparativeStandardization" , ".txt", sep = "")
      }
      # #iterative
    }, 
    content = function(file){
      
      if(input$Sampling2 == "RO"){
        original_con = signif(rankOrderCon(), 3)
        original_ttx = signif(rankOrderTTX(), 3)
        lmRanked <- lm(original_ttx ~ original_con)
        fitted_y = signif(fitted(lmRanked), 3)
        ratio_values = signif(original_ttx/original_con, 3)
        
        #write(paste("Rank Order Linear and Ratio Plots", "\t", sep = ""), file)
        write(paste("Control", "\t", "Treatment", "\t", "Linear R", "\t", "Ratio", "\t", sep = ""), file, append = TRUE)
        write(paste(original_con, "\t", original_ttx, "\t", fitted_y, "\t", ratio_values, "\t", sep = ""), file, append = TRUE)
        
      } else if(input$Sampling2 == "CS"){
        
        #histogram 1
        conGEVD <- conGEVD()
        pooledShape <- pooledShape()
        distXVals <- seq(0,100,by=0.1)
        conGEVDLine <- dgev(distXVals,conGEVD$estimate[1],conGEVD$estimate[2],pooledShape)
        
        binNum = 1
        binsize_con = ceiling((ceiling(max(samplingCon())))/binNum)
        binsize_ttx = ceiling((ceiling(max(samplingTTX())))/binNum)
        
        histCon <- hist(samplingCon(),breaks=binsize_con,freq=FALSE)
        
        con_mu = signif(conGEVD$estimate[1],2)
        con_sigma = signif(conGEVD$estimate[2],2)
        con_xi = signif(pooledShape,2)
        
        #histogram 2
        ttxGEVD <- ttxGEVD()
        ttxAmpEmpCdf <- samplingTTX()
        ttxGEVDLine <- dgev(distXVals,ttxGEVD$estimate[1],ttxGEVD$estimate[2],pooledShape)
        
        histTTX <- hist(ttxAmpEmpCdf,breaks=binsize_ttx,freq=FALSE)
        
        ttx_mu = signif(ttxGEVD$estimate[1],2)
        ttx_sigma = signif(ttxGEVD$estimate[2],2)
        ttx_xi = signif(pooledShape,2)
        
        Mu = "\u03BC"
        #Sigma = paste(sigma, sep = "")
        #Xi = paste(xi, sep = "")
        #<- '\u03BC' ['Con']
        
        #TO DO: fix breaks
        
        max_length = max(c(length(conGEVDLine), length(ttxGEVDLine), 
                           length(histCon$breaks), length(histTTX$breaks), 
                           length(histCon$density), length(histTTX$density),
                           length(distXVals)))
        
        # print(histCon$breaks)
        # print(histCon$density)
        # print(histTTX$breaks)
        # print(histTTX$density)
        
        i = 1
        conBreaks = vector()
        while(i < length(histCon$breaks)){
          conBreaks[i] = mean(c(histCon$breaks[i], histCon$breaks[i+1])) 
          i = i+1
        }
        
        j = 1
        ttxBreaks = vector()
        while(j < length(histTTX$breaks)){
          ttxBreaks[j] = mean(c(histTTX$breaks[j], histTTX$breaks[j+1])) 
          j = j+1
        }
        
        #print(conBreaks)
        #print(ttxBreaks)
        
        conGEVD_fixed = rep(" ", (max_length - length(conGEVDLine)))
        ttxGEVD_fixed = rep(" ", (max_length - length(ttxGEVDLine)))
        histcon_fixed = rep(" ", (max_length - length(conBreaks)))
        histttx_fixed = rep(" ", (max_length - length(ttxBreaks)))
        dist_fixed = rep(" ", (max_length - length(distXVals)))
        parameters_fixed = rep(" ", (max_length - 1))
        
        new_vector_conGEVD = c(signif(conGEVDLine, 3), conGEVD_fixed)
        new_vector_ttxGEVD = c(signif(ttxGEVDLine, 3), ttxGEVD_fixed)
        new_vector_histcon = c(conBreaks, histcon_fixed)
        new_vector_histttx = c(ttxBreaks, histttx_fixed)
        new_vector_condensity = c(signif(histCon$density, 3), histcon_fixed)
        new_vector_ttxdensity = c(signif(histTTX$density, 3), histttx_fixed)
        new_vector_dist = c(distXVals, dist_fixed)
        new_vector_conmu = c(con_mu, parameters_fixed)
        new_vector_consigma = c(con_sigma, parameters_fixed)
        new_vector_conxi = c(con_xi, parameters_fixed)
        new_vector_ttxmu = c(ttx_mu, parameters_fixed)
        new_vector_ttxsigma = c(ttx_sigma, parameters_fixed)
        new_vector_ttxxi = c(ttx_xi, parameters_fixed)
        
        write(paste("Bins", "Density", "X", "Y", Mu, "Sigma", "Xi",
                    "Bins", "Density", "X", "Y", Mu, "Sigma", "Xi", "\t", sep = "\t"), file)
        write(paste(new_vector_histcon, new_vector_condensity, new_vector_dist, new_vector_conGEVD, 
                    new_vector_conmu, new_vector_consigma, new_vector_conxi,
                    new_vector_histttx, new_vector_ttxdensity, new_vector_dist, new_vector_ttxGEVD, 
                    new_vector_ttxmu, new_vector_ttxsigma, new_vector_ttxxi, "\t", sep = "\t"), file, append = TRUE)
        
      }
    }
  )
  
  #Download means-- Summary Data
  output$downloadMeans <- downloadHandler(
    filename = function(){
      paste("Means" , ".txt", sep = "")
    }, 
    content = function(file){
      
      ttxAmpsByRec = numberOfTTXSampled()
      conAmpsByRec = numberOfConSampled()
      
      i = 1
      means = vector()
      for(x in conAmpsByRec){
        means[i] = mean(x)
        i = i+1
      }
      
      j = 1
      means_ttx = vector()
      for(x in ttxAmpsByRec){
        means_ttx[j] = mean(x)
        j = j+1
      }
      
      length_con = length(means)
      length_ttx = length(means_ttx)
      
      if(length_con > length_ttx){
        
        fixed_length = rep(" ", length_con-length_ttx)
        fixed_length_names = rep(" ", length_con-length_ttx)
        new_vector_means = c(signif(means_ttx, 3), fixed_length)
        new_vector_names = c(names(ttxAmpsByRec), fixed_length_names)
        
        write(paste("Control", "\t\t", "Treatment", "\t", sep = ""), file)
        write(paste(names(conAmpsByRec), "\t", signif(means, 3), "\t", 
                    new_vector_names, "\t", new_vector_means, "\t", sep = ""), file, append = TRUE)
        
      } else if(length_ttx > length_con){
        
        fixed_length = rep(" ", length_ttx-length_con)
        fixed_length_names = rep(" ", length_ttx-length_con)
        new_vector_means = c(signif(means, 3), fixed_length)
        new_vector_names = c(names(conAmpsByRec), fixed_length_names)
        
        write(paste("Control", "\t\t", "Treatment", "\t", sep = ""), file)
        write(paste(new_vector_names, "\t", new_vector_means, "\t", 
                    names(ttxAmpsByRec), "\t", signif(means_ttx, 3), "\t", sep = ""), file, append = TRUE)
        
      } else{
        write(paste("Control", "\t\t", "Treatment", "\t", sep = ""), file)
        write(paste(names(conAmpsByRec), "\t", signif(means, 3), "\t", 
                    names(ttxAmpsByRec), "\t", signif(means_ttx, 3), "\t", sep = ""), file, append = TRUE)
      }
      
    }
  )
  
  #Download cells removed-- Summary Data
  output$downloadCells <- downloadHandler(
    filename = function(){
      paste("CellsRemoved" , ".txt", sep = "")
    }, 
    content = function(file){
      
      con = conFilesRemoved()
      ttx = ttxFilesRemoved()
      
      write(paste("Control", "\r\n", sep = ""), file)
      write(paste(con, "\r\n", sep = ""), file, append = TRUE)
      write(paste("\r\n", "Treatment", "\r\n", sep = ""), file, append = TRUE)
      write(paste(ttx, "\r\n", sep = ""), file, append = TRUE)
    }
  )
  
  #Download summary plots-- Summary Data
  output$download_cdfPlot <- downloadHandler(
    filename = function(){
      paste("SummaryPlots" , ".png", sep = "")
    }, 
    content = function(file){
      png(file)
      cdfPlot()
      dev.off()
    }
  )
  
  ############################### ~~~~~~~~~~~~~~~~~~~~ ###################################
}

shinyApp(ui = ui, server = server)
library(data.table)
library(ggplot2)
library(pROC)
library(shinythemes)
library(shinydashboard)

source('src/RobustScoring.R')

# data = data.table(input$data_file)
# data = data.table(read.csv('train.csv'))

#variable_names = c("", colnames(data))
#target_classes = c(0,1)
dbHeader <- dashboardHeader(title = "Scorecard Builder",
                            tags$li(a(href = 'https://softserveinc.com/en-US/',
                                      img(src = '.\\SoftServe_primary-RGB.png',
                                          title = "SoftServeInc Home", height = "45px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))


sidebar <- dashboardSidebar(

    helpText("Specify target variable and modeling parameters."),
    fileInput("data_file", "Choose data file to upload", accept = c('text/csv', 'text/comma-separated-values', '.csv')),
    
    # selectInput("target_variable", "Target variable:", variable_names, selected = ""),
    uiOutput('target_variable'),
    # htmlOutput("selectUI"),
    #selectInput("target_class", "Target class:", c(1,0), selected = ""),
    uiOutput('ID_column'),
    numericInput("training_perc", "Percent of data for the training subset, %:", 80, step = 5),
    numericInput("good_perc", "Percent of target class for the training subset, %:", 0, step = 5),
    
    sliderInput("cut_off", "Cut-off :", min = 0, max = 1, value = 0.3)
    
)

body <- dashboardBody(
  tabsetPanel(
    tabPanel(h4("Data Modeling"),
             div(class="shiny-progress-container", id="img-pop-up", style="display:none;"),
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "shiny.css"),
               tags$script(src="images.js")
             ),
              div(actionButton("GoButton","Run Analysis and Scoring"),style="display:inline-block;"),
             # div(style="display:inline-block",downloadButton('downloadtable', 'Save scorecard to .csv')),
             # 
             # br(),
             tabsetPanel(
               tabPanel("Input Data", dataTableOutput("data"), icon = icon("table")),
               tabPanel("Correlation Matrix", plotOutput("corr_matrix", height = "600"), icon = icon("percent")),
               tabPanel("Information value", dataTableOutput("woe_table"), icon = icon("table"))
             )),
    tabPanel(h4("Scorecard Statictics"),
             # div(actionButton("GoButton","Run Scoring Calc"),style="display:inline-block;"),
              div(style="display:inline-block",downloadButton('downloadtable', 'Save scorecard to .csv')),
             # 
             # br(),
             tabsetPanel(
              
               tabPanel("Scorecard",dataTableOutput("scorecard"),								 icon = icon("table")),
               tabPanel("ROC", plotOutput("ROC"), icon = icon("bar-chart")),
               tabPanel("Regression Model Coefficients", dataTableOutput("reg_coef"), icon = icon("table")),
               tabPanel("Calculated probabilities", dataTableOutput("probabilities"), icon = icon("table")),
               tabPanel("Error matrix", dataTableOutput("error_matrix"), icon = icon("table"))
             ))
  )
  		
)
dashboardPage(
  dbHeader,
  sidebar,
  #dashboardSidebar(disable = TRUE),
  body
)


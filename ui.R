#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("yeti"),
                  # Application title
                  titlePanel("Understanding the Opioid Epidemic"),
                  navbarPage("Opioid Trends App",
                             tabPanel(h2(strong("Introduction")),br(),
                                      title = "INTRODUCTION", 
                                      h5("In this app, I will attempt to look at the prescribing patterns of opioids from 1996 to 2015.", br(),
                                         "When I was in pharmacy school in the early 2000's, I can remember the professors saying that it was rare people became
                                      addicted to opioids and that pain needs to be treated aggressively. We were taught, along with all other medical professionals, 
                                      that pain was the 5th vital sign. This was all in response to JCAHO's pain standards around 2000- 2001. Since then we have seen
                                      a surge in opioid prescriptions and subsequently opioid related deaths."), br(),
                                      h4(strong("Goals of the App")),
                                      p(h5("
                                The overall goal of this app is to look at prescribing patterns that may have led to the current opioid epidemic
                                that we see today. Using this app, we can investigate the trends of prescribing patterns of opioids throughout the years."
                                      )), br(),
                                      h4(strong("The Data- Medical Expenditure Panel Survey (MEPS)")),
                                      p(h5("The data in this app is from the", 
                                           a("Medical Expenditure Panel Survey", href = "https://meps.ahrq.gov/mepsweb/data_stats/download_data_files.jsp"), 
                                           ", The dataset is a combination of full year populations characteristics and the medication files for 1996 to 2015.
                                         MEPS is a natianally representative survery of noninstitutionalized respondents. Among other things, the survery collects
                                information on demographics and prescribed medications. Medicataions are verifed by the repsondants pharamcy when possible. 
                                To calcualte morphine equivalents a dataset from the ",
                                           a("CDC webite", href= "https://www.cdc.gov/drugoverdose/resources/data.html"), " was also merged in. This dataset was orginally
                                     created for my capstone project at Duke. However that dataset contained about 5.6 million observations. For the purpose of this
                                     project I randomly selected 500 observations from each year. This made the data manageable and the app work a little bit quicker")), br(),
                                      h4(strong("Navigating the App")),
                                      p(h5("This app has several tabs to explore. The first one (Data Exploration) will produce contingency tables for the categorial 
                                  variables and histograms for the numeric variables")),
                                      
                                      p(h5("The second tab (Data Exploration2) allows the user to see the relationship of the variables to average morphine equivalents, 
                                  opioid refills, and the number of opioid users")),
                                      
                                      p(h5("The third tab (PCA), is the unsupervised learning model. There are three subtabs to explore")),
                                      
                                      p(h5("The fourth tab (Models), has the results from a logistic regression trying to predict if someone will receive an opioid 
                                  script or if they are a high user of opioids. It also has a boosted tree model to predict if someone will recieve an opioid prescription")),
                                      
                                      p(h5("The fifth and final tab (Raw Data), allows the user to download the entire dataset")),
                                      
                                      h4(strong("MathJax")),
                                      p(h5("One of the requirements of the assignment was to use a mathjax object. I don't know where to do that so I'll just put it here")),
                                      uiOutput('ex2')
                             ),
                             
                             tabPanel(h2("DATA EXPLORATION- Contingency Table and Histograms"), br(),
                                      title= "DATA EXPLORATION 1",
                                      sidebarLayout(
                                          sidebarPanel(
                                              checkboxGroupInput("sum", "Summary Method", c("Histogram", "Contigency Table"), selected= "Histogram"),
                                              
                                              
                                              conditionalPanel(condition="input.sum.includes('Histogram')", 
                                                               selectInput("numvar", "Select a Variable", c(Age= "age", Income= "INCOME",Office_Visits= "Office_Visits", Er_Visits= "ER_Visits", Poverty_Status= "Poverty_Status")),
                                                               sliderInput("bins", "Number of bins:", min=1,max=150,value=50)
                                              ),
                                              conditionalPanel(condition= "input.sum.includes('Contigency Table')",
                                                               selectInput("colum", "Select Column Variable for Frequency table", 
                                                                           c(Race= "Race", Education= "Degree", 
                                                                             Marital_Status= "Marital_Status", Gender= "gender", 
                                                                             Insurance= "Insurance", "Any Opioid Rxs"= "any_opioid", 
                                                                             "Poverty_Status"= "Poverty_Status")) ,
                                                               selectInput("rowvar", "Select Row Variable for Frequency Table", 
                                                                           c(Race= "Race", Education= "Degree", 
                                                                             Marital_Status= "Marital_Status", Gender= "gender", 
                                                                             Insurance= "Insurance", "Any Opioid Rxs"= "any_opioid", 
                                                                             "Poverty_Status"= "Poverty_Status"))),
                                          ),
                                          mainPanel(
                                              conditionalPanel(condition="input.sum.includes('Contigency Table')",
                                                               tableOutput('foo')),
                                              conditionalPanel(condition="input.sum.includes('Histogram')",
                                                               plotlyOutput("distPlot"))
                                          )
                                      )),
                             tabPanel(h2("Data Exploration 2"), br(),
                                      title= "DATA EXPLORATION",
                                      sidebarLayout(
                                          sidebarPanel(
                                              selectInput("xaxis", "Independant Variable",choices= list(Age= "age",Gender= "gender",Race= "Race", Education= "Degree", MaritalStaus= "Marital_Status", Year= "year", Office_Visits= "Office_Visits", ER_Visits= "ER_Visits", Poverty_Status="Poverty_Status" )
                                              ),
                                              selectInput("yaxis", "Dependant Variable", choices= list("Average Morphine Equivalents" = "`Averge Morphine Equivalents`", "Average Opioid Refills"= "`Average # of Opioid Refills`", "Number of Opioid Users"= "`Number of Opioid Users`")
                                              ),
                                              radioButtons("button",
                                                           "Plot Type",
                                                           c("Bar Plot"= "bar", "Scatter Plot"= "point")),
                                              downloadButton('download',"Download the data")
                                          ),
                                          mainPanel(
                                              plotlyOutput("Plot"),
                                              h3(strong("To download the plot, select the camera icon above the plot")), br(),
                                              dataTableOutput("Table")
                                              
                                          )
                                      )),
                             
                             
                             tabPanel("PCA",
                                      p("The dataset is rather large and the it takes a long time for the PCA. To help shorten the wait time, 
                            the data can  be filtered by year. Even so, the plots take a good couple of minutes to load"),
                                      selectInput("year", "Select a year", choices= c(1996,1997, 1998, 1999, 2000, 2001,2002, 2003, 2004, 2005, 
                                                                                      2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)),
                                      tabsetPanel(
                                          tabPanel("PCA Table",
                                                   dataTableOutput("PCTable")),
                                          tabPanel("PCA Summary",
                                                   verbatimTextOutput("pcSumm")),
                                          tabPanel("Plots",
                                                   selectizeInput('var1', 'Select PC on x-axis',
                                                                  choices = seq(1:13),
                                                                  selected = 1),
                                                   selectizeInput('var2', 'Select PC on y-axis',
                                                                  choices = seq(1:13),
                                                                  selected = 8),
                                                   plotOutput("biPlot", height = 500, width = 500),
                                                   plotOutput("screePlot"),
                                                   plotOutput("cummVarPlot")
                                          )
                                          
                                      )
                             ),
                             tabPanel(h2("MODELS"), br(),
                                      title= "MODELS",
                                      p("The Models may take a minute to load.") ,
                                      
                                      
                                      tabsetPanel(
                                          tabPanel(
                                              p("Logistic Regression Model"),
                                              p("A new variable was created (high_dose) for any person that has a total morphine equivalent above 1500mg"),
                                              sidebarLayout(
                                                  sidebarPanel(
                                                      uiOutput("var1_select"),
                                                      uiOutput("rest_var_select")),
                                                  
                                                  mainPanel(
                                                      verbatimTextOutput("logModel1")))),
                                          tabPanel(
                                              p("Gradient Boosting Model"),
                                              p("This model takes a very time to run and so can be filtered by year to make it a little faster. The whole dataset takes too long and somethings
                                 gives a memory exhasuted error"),
                                              selectInput("yearB", "Select a year", choices= c(1996,1997, 1998, 1999, 2000, 2001,2002, 2003, 2004, 2005, 
                                                                                               2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)),
                                              sidebarLayout(
                                                  sidebarPanel(
                                                      sliderInput("cvNum", "Select the folds for cross validation", min=1, max=10, value=10),
                                                      
                                                  ),
                                                  mainPanel(
                                                      verbatimTextOutput("boosted"),
                                                      verbatimTextOutput("varimp")
                                                  )
                                              )
                                          ),
                                          tabPanel(
                                              p("Testing the Model"),
                                              p("Preformance of Logistic Regression Model On test Data"),
                                              br(),
                                              conditionalPanel(condition="input.ind_var_select.includes('opioidRX')",
                                                               verbatimTextOutput("TestRX")),
                                              conditionalPanel(condition="input.ind_var_select.includes('high_dose')",
                                                               verbatimTextOutput("TestHigh")),
                                              br(),
                                              p("Preformance of Boosted Model on the test Data"),
                                              verbatimTextOutput("pBoost"),
                                              
                                          )
                                      )
                             ),
                             
                             
                             
                             tabPanel(h2("RAW DATA"), br(),
                                      title= "RAW DATA",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h5("The data contains about 10,000 observations"),
                                              #radioButtons("filter", "Would you like to filter the data before downloading it?", choices=c("yes", "no"), select= "no"),
                                              # conditionalPanel("input.filter.includes('yes')", 
                                              #                selectInput("filterby", "Which variable to filter by?", choices= c(Race= "Race", Education= "Degree", Gender= "gender")),
                                              
                                              #conditionalPanel("input.filterby.includes('Race')",
                                              #                                 selectInput("filterRace", "Which Race", choices= c("White", "Black", "Asian", "Hispanic", "AmerIndian", "Other"))),
                                              # conditionalPanel("input.filterby.includes('Degree')",
                                              #                                 selectInput("filterDegree", "What level of Education", choices= c("College", "High_School", "No_degree", "No_info"))),
                                              #conditionalPanel("input.filterby.includes('gender')",
                                              #                                   selectInput("filtergender", "Male/ Female", choices= c("male", "female")
                                              #                ))),
                                              downloadButton("downloadDataSet", "Download")
                                          ),
                                          
                                          mainPanel(
                                              dataTableOutput("dataset")
                                          )
                                          
                                      )
                             )
                  )))



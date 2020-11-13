#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
#library(chron)
#library(lubridate)
library(hms) # for raw data table to display properly
library(stats)
library(plotly)
#library(janitor)
#library(psych)
#library(GGally)
library(DT)
library(caret)
library(xgboost)



# read in data

opioidData<- read_csv("subset.csv")

# Remove data for any age not reported (in Meps this is reported as a -9)
opioidData<- opioidData %>% filter(AGE>=0) %>% rename(age= AGE, Race= Eth, Marital_Status= MARRY)

#Create training and test datasets for model building
set.seed(2011)

opioidData$any_opioid<- as.factor(opioidData$any_opioid)
trainIndex <- createDataPartition(opioidData$any_opioid, p = 0.8, list = FALSE)
opioidDataTrain<- opioidData[trainIndex, ]
opioidDataTest<- opioidData[-trainIndex, ]
opioidDataTrain<- as.data.frame(opioidDataTrain)
opioidDataTest<- as.data.frame(opioidDataTest)

# Data Exploration



server <- function(input, output, session) {
    filterData <- reactive({
        x_axis<- input$xaxis
        newData<-opioidData%>%filter(any_opioid=="yes") %>% group_by_at(x_axis) %>% 
            summarise(`Averge Morphine Equivalents`= round(mean(totalME),2), `Average # of Opioid Refills`= round(mean(TotalOpioidsFills),2), `Number of Opioid Users`= length(any_opioid))
    })
    dataset<- reactive({dataset1<- opioidData})
    
    
    #To create plots on data exploration page
    output$Plot <- renderPlotly({
        x_axis<- input$xaxis
        y_axis<- input$yaxis
        switch(input$button,
               "bar" = opioidData %>%filter(any_opioid== "yes")%>% group_by_at(x_axis) %>% summarise(`Averge Morphine Equivalents`= mean(totalME), `Average # of Opioid Refills`= mean(TotalOpioidsFills), `Number of Opioid Users`= length(any_opioid)) %>%  
                   ggplot(aes_string(x_axis, y_axis))+ geom_col(),
               "point" =opioidData %>% filter(any_opioid== "yes") %>% group_by_at(x_axis) %>% summarise(`Averge Morphine Equivalents`= mean(totalME), `Average # of Opioid Refills`= mean(TotalOpioidsFills), `Number of Opioid Users`= length(any_opioid)) %>%  
                   ggplot(aes_string(x_axis, y_axis))+ geom_point()
               
        )
    }
    )
    #Table Summaries on Data exploration page
    output$Table<- renderDataTable({
        filterData()
    })
    
    output$download <- downloadHandler(
        filename= function(){"data.csv"},
        content=function(file){
            write.csv(filterData(), file)
        }
    )
    
    output$dataset<- renderDataTable({
        dataset()
    })
    ###########Raw Data page- to download entire dataset#########
    #dataset2<- reactive({dataset2<- opioidData %>% filter(input$filterby== input$)})
    output$downloadDataSet <- downloadHandler(
        filename= function(){"dataset.csv"},
        content=function(file){
            write.csv(dataset(), file)
        }
    )
    ##############Data Exploration Page 2############
    # to create the contigency tables
    s<- reactive(opioidData)
    observe({
        updateSelectInput(session, "colum", choices = c(Race= "Race", Education= "Degree", 
                                                        Marital_Status= "Marital_Status", Gender= "gender", 
                                                        Insurance= "Insurance", "Any Opioid Rxs"= "any_opioid", 
                                                        "Poverty_Status"= "Poverty_Status"))
    })
    observe({
        updateSelectInput(session, "rowvar", choices = c(Race= "Race", Education= "Degree",
                                                         Marital_Status= "Marital_Status", Gender= "gender", 
                                                         Insurance= "Insurance", "Any Opioid Rxs"= "any_opioid",
                                                         "Poverty_Status"= "Poverty_Status"))
    })
    
    output$foo <- renderTable({
        with(s(), table(get(input$rowvar),get(input$colum)))
    })
    output$tbl2<- renderTable
    output$distPlot <- renderPlotly({
        numvar<- input$numvar
        opioidData %>%filter(age>0 & INCOME >0)%>% ggplot(aes_string(numvar))+ geom_histogram(bins= input$bins)
        
    })
    ########### PCA page
    
    observe({
        PCAdata <- opioidData  %>% mutate(male= ifelse(gender=="male", 1,0),  
                                          white= ifelse(Race == "White", 1, 0), 
                                          black= ifelse(Race == "Black", 1, 0), 
                                          college= ifelse(Degree== "College", 1, 0), 
                                          HighSchool= ifelse(Degree== "High_School", 1, 0), 
                                          No_degree= ifelse(Degree== "No degree", 1, 0))  %>% 
            select(-c(DUPERSID, Race:Insurance, ID, any_opioid))%>% na.omit() %>% filter(age>= 0, INCOME>= 0, year== input$year) %>% select(-c(year))
        
        pca<-prcomp(PCAdata, center=TRUE, scale=TRUE)
        
        princComps <- as.data.frame(pca$rotation)
        output$PCTable <- renderDataTable({
            datatable(round(princComps,3))
        })
        
        # Variability of PC
        variability <- pca$sdev^2
        # Variance explained by each PC
        variabilityExpl <- variability / sum(variability)
        
        #ScreePlot
        output$screePlot <- renderPlot({
            # Plot variance explained for each principal component
            screeplot(pca, type = "lines")
        })
        
        output$cummVarPlot <- renderPlot({
            #Plot cumulative proportion of variance explained
            plot(cumsum(variabilityExpl), xlab = "Principal Component",
                 ylab = "Cumulative Proportion of Variance Explained",
                 ylim = c(0, 1), type = "b")
        })
        
        #BiPlot
        output$biPlot <- renderPlot({
            if(input$var1 == input$var2){
                print("Please Select two different PCs")
            }
            biplot(pca, choices = c(as.numeric(input$var1),
                                    as.numeric(input$var2)), cex = 0.8,
                   xlim = c(-0.08, 0.1), ylim = c(-0.07, 0.1))
        })
        
        #Print PCA Summary
        output$pcSumm <- renderPrint({
            summary(pca)
        })
        
    })
    ############# Modeling Page
    
    #data<- reactive({train<- opioidDataTrain %>% mutate(high_dose= ifelse(totalME>1500, 1,0), opioid= ifelse(any_opioid=="yes", 1, 0)) %>%select(-c(totalME, TotalOpioidsFills))})
    #test<- reactive({test<- opioidDataTest %>% mutate(high_dose= ifelse(totalME>1500, 1,0), opioid= ifelse(any_opioid== "yes", 1,0)) %>% select(-c(totalME, TotalOpioidsFills))})
    
    
    output$var1_select<-renderUI({
        selectInput("ind_var_select","Select Independent Var", choices =list("opioidRX", "high_dose"),multiple = FALSE)
    })
    output$rest_var_select<-renderUI({
        checkboxGroupInput("other_var_select","Select other Var",choices= list(Age= "age", Income= "INCOME",Office_Visits= "Office_Visits", ER_Visits= "ER_Visits", 
                                                                               Poverty= "Poverty_Status", Race= "Race", Year= "year",Education= "Degree", 
                                                                               Marital_Status= "Marital_Status", Gender= "gender", 
                                                                               Insurance= "Insurance"), 
                           select= c("age", "INCOME", "Office_Visits", "year"))
    })
    
    ###### Logistic Regression Model ######
    train<-  opioidDataTrain %>% mutate(high_dose= ifelse(totalME>1500, 1,0), opioidRX= ifelse(any_opioid=="yes", 1, 0)) %>%
        select(-c(totalME, TotalOpioidsFills)) 
    
    test<-opioidDataTest %>% mutate(high_dose= ifelse(totalME>1500, 1,0), opioidRX= ifelse(any_opioid== "yes", 1,0)) %>% 
        select(-c(totalME, TotalOpioidsFills))
    
    variables<- reactive({
        modelvars<- paste0(input$other_var_select, collapse= "+")
    })
    fitLogModel<- reactive({
        Logfit<- glm(as.formula(paste0(input$ind_var_select,"~", variables())), data=train, family= binomial())
        return(Logfit)
    })
    output$logModel1<- renderPrint({
        summary(fitLogModel())
    })
    #### Logistic Model test on testing data set
    
    predLog<- reactive({
        logpred<- predict(fitLogModel(), newdata= test)
        return(logpred)
    })
    
    
    testLogRX<- reactive({
        LogPostRX<- postResample(predLog(), obs= test$opioidRX)
        return(LogPostRX)
    })
    
    testLogHigh<- reactive({
        LogPosthigh<- postResample(predLog(), obs= test$high_dose)
        return(LogPosthigh)
    })
    
    
    output$TestRX<- renderPrint({
        testLogRX()
    })
    output$TestHigh<- renderPrint({
        testLogHigh()
    })
    
    ### testing Boosted Model
    
    
    #### Boosted tree tab
    
    observe({
        trainingData<- opioidDataTrain %>% filter(year== input$yearB) %>% select(-c(DUPERSID, ID, totalME, TotalOpioidsFills, year))
        testingData<- opioidDataTest %>% filter(year== input$yearB) %>% select(-c(DUPERSID, ID, totalME, TotalOpioidsFills, year))
        #trainBoost<- trainingData[-year]
        #testBoost<- testingData[-year]
        BoostReg<- train(any_opioid ~.,  data= trainingData, method= "xgbTree",
                         #preProcess = c("center", "scale"),
                         trControl=trainControl(method = "cv", number= input$cvNum))
        output$boosted<- renderPrint({
            print(BoostReg)})
        output$varimp<- renderPrint({
            print(varImp(BoostReg))})
        predBoost<- predict(BoostReg, newdata= testingData)
        boostpred1<- postResample(predBoost, obs=testingData$any_opioid)
        output$pBoost<- renderPrint({
            print(boostpred1)
        })
        
    })
    ### MathJax object
    output$ex2 <- renderUI({
        withMathJax(
            helpText('example calculation of morphine equivalents $$30\\times 20\\times 0.15$$'))
        
    })
    
    
    
}

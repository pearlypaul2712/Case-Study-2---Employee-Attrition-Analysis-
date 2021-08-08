#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

df<-c(colnames(employeedata))
#employeedata[df[2]]

source("helpers.R")


employee<-read.csv("data/CaseStudy.csv")
Beers_and_Breweries_merged<-read.csv("data/CaseStudy.csv")
employeedata<-employee


employeedata <- select(employee,-c(ID, EmployeeCount, EmployeeNumber, Over18,StandardHours))


employeedata$Age <- cut(employeedata$Age,breaks=c(-Inf,20, 30, 40, 50, 60,Inf),labels=c("0-19 years","20-29 years","30-39 years","40-49 years","50-59 years","60 +"),right=FALSE)


employeedata$DailyRate <- cut(employeedata$DailyRate,breaks=c(-Inf,500, 1000,1500, 2000,Inf),labels=c("0-499 Range","500-999 Range","1000-1499 Range","1500 + Range","60 +"),right=FALSE)


employeedata$DistanceFromHome <- cut(employeedata$DistanceFromHome,breaks=c(-Inf,10, 20,30,Inf),labels=c("1-9 miles","20-29 miles","30-40 miles","40+ miles" ),right=FALSE)


employeedata$MonthlyIncome <- cut(employeedata$MonthlyIncome,breaks=c(-Inf,5000, 10000,20000,Inf),labels=c("Low","Medium","Higher","Highest" ),right=FALSE)



employeedata$MonthlyRate <- cut(employeedata$MonthlyRate,breaks=c(-Inf,5000, 10000,20000,Inf),labels=c("Low","Medium","Higher","Highest" ),right=FALSE)

employeedata$YearsAtCompany <- cut(employeedata$YearsAtCompany,breaks=c(-Inf, 10,15,Inf),labels=c("1-9 years","10 -14 years","15 + years" ),right=FALSE)

employeedata$TotalWorkingYears <- cut(employeedata$TotalWorkingYears,breaks=c(-Inf, 10,15,Inf),labels=c("1-9 years","10 -14 years","15 + years" ),right=FALSE)



df<-c(colnames(employeedata))


employeedata$Education=as.factor(if_else(employeedata$Education == 1,"Below College", if_else(employeedata$Education == 2, "College", if_else(employeedata$Education == 3, "Bachelor", if_else(employeedata$Education == 4, "Master","Doctor")))))

employeedata$EnvironmentSatisfaction = as.factor(if_else(employeedata$EnvironmentSatisfaction == 1,"Low",if_else(employeedata$EnvironmentSatisfaction == 2, "Medium", if_else(employeedata$EnvironmentSatisfaction == 3, "High", "Very High"))))

employeedata$JobInvolvement = as.factor(if_else(employeedata$JobInvolvement == 1,"Low",if_else(employeedata$JobInvolvement == 2, "Medium",if_else(employeedata$JobInvolvement == 3, "High", "Very High"))))


employeedata$JobSatisfaction = as.factor(if_else(employeedata$JobSatisfaction == 1, "Low",if_else(employeedata$JobSatisfaction == 2, "Medium",if_else(employeedata$JobSatisfaction == 3, "High","Very High"))))

employeedata$PerformanceRating = as.factor(if_else(employeedata$PerformanceRating == 1, "Low",if_else(employeedata$PerformanceRating == 2, "Good", if_else(employeedata$PerformanceRating == 3, "Excellent", "Outstanding"))))

employeedata$RelationshipSatisfaction = as.factor(if_else(employeedata$RelationshipSatisfaction == 1, "Low",if_else(employeedata$RelationshipSatisfaction == 2, "Medium", if_else(employeedata$RelationshipSatisfaction == 3, "High", "Very High"))))

employeedata$WorkLifeBalance = as.factor(if_else(employeedata$WorkLifeBalance == 1, "Bad",if_else(employeedata$WorkLifeBalance == 2, "Good", if_else(employeedata$WorkLifeBalance == 3, "Better", "Best"))))



employeedata$JobLevel = as.factor(if_else(employeedata$JobLevel == 1, "Low",if_else(employeedata$JobLevel == 2, "Medium",if_else(employeedata$JobLevel  == 3, "High","Very High"))))


employeedata$YearsSinceLastPromotion = cut(employeedata$YearsSinceLastPromotion ,breaks=c(-Inf, 10,15,Inf),labels=c("1-9 years","10 -14 years","15 + years" ),right=FALSE)

employeedata$PercentSalaryHike  = cut(employeedata$PercentSalaryHike  ,breaks=c(-Inf, 10,15,20,Inf),labels=c("1-9 percent","10 -14 percent","15 + percent","20+ percent" ),right=FALSE)

employeedata$YearsInCurrentRole= cut(employeedata$YearsInCurrentRole ,breaks=c(-Inf, 10,15,20,Inf),labels=c("1-9 years","10 -14 years","15 + years","20+ years" ),right=FALSE)


employeedata$YearsWithCurrManager= cut(employeedata$YearsWithCurrManager ,breaks=c(-Inf, 10,15,20,Inf),labels=c("1-9 years","10 -14 years","15 + years","20+ years" ),right=FALSE)

# Define UI for application that draws a histogram
ui <-  tagList(
    shinythemes::themeSelector(),
    navbarPage(
        theme = "sandstone",  # <--- To use a theme, uncomment this
        "Employee Attrition Analysis",
        tabPanel("Summary",
                 sidebarPanel(
                     
                     # Input: Select a dataset ----
                     selectInput("dataset", "Choose a dataset:",
                                 choices = c("Employee_Attrition")),
                     
                     # Input: Specify the number of observations to view ----
                     numericInput("obs", "Number of observations to view:", 5),
                     
                     # Include clarifying text ----
                     helpText("Note: while the data view will show only the specified",
                              "number of observations, the summary will still be based",
                              "on the full dataset."),
                     
                     # Input: actionButton() to defer the rendering of output ----
                     # until the user explicitly clicks the button (rather than
                     # doing it immediately when inputs change). This is useful if
                     # the computations required to render output are inordinately
                     # time-consuming.
                     actionButton("update", "Update View")
                     
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Summary of Employee Data",
                                  # Output: Header + summary of distribution ----
                                  h4("Summary"),
                                  verbatimTextOutput("summary"),
                                  
                                  # Output: Header + table of distribution ----
                                  h4("Observations"),
                                  tableOutput("view")
                         )
                     )
                 )
        ),
        tabPanel("Distributions of Employee Monthly Income/ Attrition", 
                 selectInput("select", label = h3("Attrition/Monthly Income"), 
                             choices = list("Attrition" = "Attrition", "MonthlyIncome" = "MonthlyIncome"), 
                             selected = 1),
                 selectInput("select1", 
                             label = h3("Employee Features"),
                             choices = list("Age"="Age",
                                            "Attrition"="Attrition",
                                            "BusinessTravel"="BusinessTravel",
                                            "DailyRate"="DailyRate",
                                            "Department"="Department",
                                            "DistanceFromHome"="DistanceFromHome",
                                            "Education"="Education",
                                            "EducationField"="EducationField",
                                            "EmployeeNumber"="EmployeeNumber",
                                            "EnvironmentSatisfaction"="EnvironmentSatisfaction",
                                            "Gender"="Gender",
                                            "HourlyRate"="HourlyRate",
                                            "JobInvolvement"="JobInvolvement",
                                            "JobLevel"="JobLevel",
                                            "JobRole"="JobRole",
                                            "JobSatisfaction"="JobSatisfaction",
                                            "MaritalStatus"="MaritalStatus",
                                            "MonthlyIncome"= "MonthlyIncome",
                                            "MonthlyRate"="MonthlyRate",
                                            "NumCompaniesWorked"="NumCompaniesWorked",
                                            "OverTime"="OverTime",
                                            "PercentSalaryHike"="PercentSalaryHike",
                                            "PerformanceRating"="PerformanceRating",
                                            "RelationshipSatisfaction"="RelationshipSatisfaction",
                                            "StandardHours"="StandardHours",
                                            "StockOptionLevel"="StockOptionLevel",
                                            "TotalWorkingYears"="TotalWorkingYears",
                                            "TrainingTimesLastYear"="TrainingTimesLastYear",
                                            "WorkLifeBalance"=
                                              "WorkLifeBalance",
                                            "YearsAtCompany"="YearsAtCompany",
                                            "YearsInCurrentRole"="YearsInCurrentRole",
                                            "YearsSinceLastPromotion"="YearsSinceLastPromotion",
                                            "YearsWithCurrManager"="YearsWithCurrManager"),selected = 1),
                 radioButtons('ChartType', 'Document format', c('Box_Plot', 'Histogram'),inline = TRUE)
                 ,mainPanel(plotOutput(outputId= "Plot")),
        # tabPanel("ABV vs IBU ", 
        #          selectInput("stateinput",
        #                      "State :",
        #                      c(unique(Beers_and_Breweries_merged$State))),
        #          radioButtons('Line', 'Linear Regression Line', c('Yes', 'No'),
        #                       inline = TRUE)
        #          ,mainPanel(plotOutput(outputId= "scatter")) 
        # ),
        
        # tabPanel("Distributions of Style of beer ", 
        #          selectInput("style",
        #                      "Style",
        #                      c(unique(Beers_and_Breweries_merged$Style))),mainPanel(plotOutput(outputId= "styleinput")) 
        )))
  # Define server logic required to draw a histogram
server <- function(input, output) {
    datasetInput <- eventReactive(input$update, {
        switch(input$dataset,
               
               "Employee_Attrition"=employeedata)
    }, ignoreNULL = FALSE)
    
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    output$view <- renderTable({
        head(datasetInput(), n = isolate(input$obs))
    })

    
  
    output$Plot<-renderPlot({
        #if(input$select=="Attrition"){
          
        
            if(input$ChartType=="Histogram"){
               
       
                ggplot(employeedata, aes(x=employeedata[,input$select1],fill=employeedata[,input$select])) +
                    geom_bar() + 
                    theme_bw() +
                    xlab(input$select1) +ylab("count")+
                    ggtitle("Histogram")
                
                
                #ggplot(Beers_and_Breweries_merged)+geom_boxplot(mapping=aes(x=Beers_and_Breweries_merged1$ABV))
            }
            else if(input$ChartType=="Box_Plot"){
                
                ggplot(employee, aes(x=employee[,input$select1],y=employeedata[,input$select],fill=employeedata[,input$select])) +
                    geom_boxplot() + 
                    theme_bw() +
                    xlab(input$select1) +ylab(input$select)+
                    ggtitle("Box Plot")
            }
       # }
    
        
        
    })
 
    # output$scatter<-renderPlot({
    #     if(input$Line=="Yes"){
    #         ggplot(m(), aes(x=ABV,y=IBU)) +
    #             geom_point(color="red") + 
    #             theme_bw() +
    #             
    #             xlab("ABV") +ylab("IBU")+
    #             ggtitle("ABV vs IBU")+ geom_smooth(method="lm")
    #     }
    #     else if(input$Line=="No"){
    #         ggplot(m(), aes(x=ABV,y=IBU)) +
    #             geom_point(color="red") + 
    #             theme_bw() +
    #             xlab("ABV") +ylab("IBU")+
    #             ggtitle("ABV vs IBU")
    #     }
    # })
    # 
    # 
    # output$styleinput<-renderPlot({
    #     
    #         ggplot(v(), aes(x=Region)) +
    #             geom_bar(fill="blue") + 
    #            
    #             xlab("Region")+
    #             ggtitle("Region vs Count of Beer")
    #     })
    
}
# Run the application 
shinyApp(ui = ui, server = server)

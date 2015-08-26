

```{r,message=FALSE,echo=FALSE}
library("googleVis")
library("pxR")
library("reshape")

#Read in the following px file: JCA02 Job Churn Components by Nace Code, Year and Statistic, e.g. Jobs Created, Jobs Destroyed, etc. 
#This is available from the CSO Statbank: http://www.cso.ie/px/pxeirestat/database/eirestat/Job%20Churn/Job%20Churn_statbank.asp?sp=Job%20Churn&Planguage=0
JCA02.px<- read.px("http://www.cso.ie/px/pxeirestat/Database/eirestat/Job%20Churn/JCA02.px")
#Convert this .px file into a data frame
JCA02df<-as.data.frame(JCA02.px)
#We need to reshape the data frame so as we can change the statistic categories into variables which have 
#values for Year and Nace Code
JCA02 = dcast(JCA02df, Year + Activity.NACE.Rev.2 ~ Statistic, value.var="value", fun.aggregate=sum)
#We are going to prepare graphs for Nace Codes B to N,-642, so we can create a smaller dataset which just this category
ourdata<-subset(JCA02, Activity.NACE.Rev.2== "Business economy excluding activities of holding companies (B to N,-642)")
names(ourdata)[14]="Hirings"
names(ourdata)[15]="Separations"
names(ourdata)[16]="Job creation"
names(ourdata)[17]="Job destruction"
```

```{r, echo=FALSE,message=FALSE}
library(shiny)
shinyApp(
  ui=pageWithSidebar(headerPanel("job creation, job destruction, hirings and separations by Nace Code sector"),
                                        
                                        
                                        sidebarPanel( 
                                                      radioButtons("Nace", "Choose a Nace Code:",
                                                                   choices = c(levels(ourdata$Activity.NACE.Rev.2)[1:15],
                                                                               levels(JCA02$Activity.NACE.Rev.2)[65])),
                                                      checkboxInput(inputId = "pageable", label = "Pageable"),
                                                      conditionalPanel("input.pageable==true",
                                                                       numericInput(inputId = "pagesize",
                                                                                    label = "values per page",5
                                        

                                        ))),
                                        
                                        mainPanel(     #includeHTML("svgfiller.js"),
                                          htmlOutput("churn"),
                        
                                          htmlOutput("view"))
  ),
  
  server=(function(input,output) {

  output$churn <- renderGvis({ 
    
    ourdata<-subset(JCA02, Activity.NACE.Rev.2== input$Nace)
    names(ourdata)[14]="Hirings"
    names(ourdata)[15]="Separations"
    names(ourdata)[16]="Job creation"
    names(ourdata)[17]="Job destruction"
gvisLineChart(ourdata, "Year", c("Job creation","Job destruction", "Hirings","Separations"), 
                        options=list(
                          #set the size of the chart
                          height=500,width=800,
                          #add a title and set the font type and size
                          title=input$Nace,
                          titleTextStyle="{fontSize:16}",
                          #set the colour of the lines and include dashed lines for Hirings and Separations
                          series="[{color:'green', targetAxisIndex: 0}, 
                                    {color:'red',targetAxisIndex:0},
                           {color:'#b9c246',targetAxisIndex:0, lineDashStyle: [4, 4]},{color:'#e7711b',targetAxisIndex:0, lineDashStyle: [4, 4] }]",               
                          #set the line width/thickness
                          lineWidth=5,
                          gvis.editor="Edit me!"
                        ))
  
  
  })

myOptions <- reactive({
  list(
    page=ifelse(input$pageable==TRUE,'enable','disable'),
    pageSize=input$pagesize,
    height=500,width="100%"
  )
})
  
  output$view <- renderGvis({
gvisTable(ourdata,options=myOptions())
})
  
})

    
    )
```

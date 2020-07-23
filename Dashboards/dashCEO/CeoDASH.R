library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)

gg.gauge <- function(pos,breaks=c(0,70,100)) {
  get.poly <- function(a,b,r1=0.5,r2=1.0) {
    th.start <- pi*(1-a/100)
    th.end   <- pi*(1-b/100)
    th       <- seq(th.start,th.end,length=100)
    x        <- c(r1*cos(th),rev(r2*cos(th)))
    y        <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  ggplot()+ 
    geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="grey")+
    geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="forestgreen")+
    geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
    geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
              aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
    annotate("text",x=0,y=0,label=pos,vjust=0,size=8,fontface="bold")+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank()) 
}

subscribers = data.frame(Month = seq(as.Date("2015/1/1"), as.Date("2020/1/1"), "months"), Subs = rnorm(61, mean = 18, sd = 2))
subscribers <- subscribers %>% mutate(Year = format(Month, "%Y"))

ui <- dashboardPage(
  dashboardHeader(title = "Financial Performance Dashboard", titleWidth = 350, dropdownMenuOutput("messageMenu")),
  dashboardSidebar(width = 350, collapsed = TRUE),
  dashboardBody(
    fluidRow(
      splitLayout(cellWidths = c("25%", "25%","25%","25%"),
                  plotOutput("gauge1", height = 100),
                  plotOutput("gauge2", height = 100),
                  plotOutput("gauge3", height = 100),
                  plotOutput("gauge4", height = 100))
    ),
    fluidRow(titlePanel("Subscriptions"),
      box(plotOutput("subsPlot", height = 200)),
      box(dateRangeInput("daterange", "Date Range",
                         start = min(subscribers$Month),
                         end = max(subscribers$Month),
                         min = min(subscribers$Month),
                         max = max(subscribers$Month),
                         separator = " - ", format = "dd/mm/yy",
                         startview = 'Month', language = 'en', weekstart = 1),
          selectInput(inputId = 'Time_unit',
                      label='Time_unit',
                      choices=c('Month','Year'),
                      selected='Month'))
      
    ),
    fluidRow(titlePanel("Marketing")),
    fluidRow(titlePanel("Finance")),
    fluidRow(titlePanel("Stocks"))
 
  )
)

server <- function(input, output) {
  dateRangeInput<-reactive({
    dataset <- subset(subscribers, Month >= input$daterange[1] & Month <= input$daterange[2])
    dataset
  })
  selectInput= reactive({
    dataset <- dateRangeInput() %>% group_by_(input$Time_unit) %>% summarise(Sum = sum(Subs))
    print(head(dataset))
    dataset
  })
  
  output$subsPlot <-renderPlot({
    ggplot(data=selectInput(), aes_string(x=input$Time_unit,y="Sum"))  + geom_bar(stat="identity") + 
      labs(title="Subscriptions", y ="Number of subscribers") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$gauge1 <- renderPlot({
    gg.gauge(64, breaks = c(0,75,100))+labs(title = "Total Accounts YTD")
  
  })
  output$gauge2 <- renderPlot({
    gg.gauge(82, breaks = c(0,75,100))+labs(title = "Trial Account Conv Rate")
    
  })
  output$gauge3 <- renderPlot({
    gg.gauge(82, breaks = c(0,75,100))+labs(title = "Monthly MRR Retention")
    
  })
  output$gauge4 <- renderPlot({
    gg.gauge(82, breaks = c(0,75,100))+labs(title = "Average MRR YTD")
    
  })
}

shinyApp(ui,server)
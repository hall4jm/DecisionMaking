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

marketing = data.frame(Month = seq(as.Date("2010/7/1"), as.Date("2020/1/1"), "quarter"), Revenue = freeny.y)
marketing <- marketing %>% mutate(Year = format(Month, "%Y"))

ui <- dashboardPage(
  dashboardHeader(title = "Financial Performance Dashboard", titleWidth = 350),
  dashboardSidebar(width = 350,
                   dateRangeInput("daterange", "Date Range",
                                      start = min(subscribers$Month),
                                      end = max(subscribers$Month),
                                      min = min(subscribers$Month),
                                      max = max(subscribers$Month),
                                      separator = " - ", format = "dd/mm/yy",
                                      startview = 'Month', language = 'en', weekstart = 1),
                   selectInput(inputId = 'Time_unit',
                                   label='Unit',
                                   choices=c('Month','Year'),
                                   selected='Month'),
                   selectInput(inputId = 'country_unit',
                               label='Country',
                               choices=c('Global','United States', 'Canada', 'Mexico', 'Brazil', 'England'
                               , 'Ireland', 'Greenland', 'Iceland', 'Russia', 'South Korea', 'Vietnam'),
                               selected='Global')
                   ),
  dashboardBody(
    fluidRow(
      splitLayout(cellWidths = c("25%", "25%","25%","25%"),
                  plotOutput("gauge1", height = 100),
                  plotOutput("gauge2", height = 100),
                  plotOutput("gauge3", height = 100),
                  plotOutput("gauge4", height = 100))
    ),
    fluidRow(titlePanel("Subscriptions"),
      box(plotOutput("subsPlot", height = 150), width = '100%')
    ),
    fluidRow(titlePanel("Expenses"),
             box(plotOutput("markPlot",height = 150), width = '100%')
             ),
    fluidRow(titlePanel("Finance"),
             box(plotOutput("", height = 150))),
 
  )
)

server <- function(input, output) {
  dateRangeInput<-reactive({
    dataset <- subset(subscribers, Month >= input$daterange[1] & Month <= input$daterange[2])
    print(head(dataset))
    dataset
  })
  selectInput= reactive({
    dataset <- dateRangeInput() %>% group_by_(input$Time_unit) %>% summarise(Sum = sum(Subs))
    print(head(dataset))
    dataset
  })
  
  output$subsPlot <- renderPlot({
    print(input$Time_unit)
    ggplot(data=selectInput(), aes_string(x = input$Time_unit, y = "Sum", group = 1)) + geom_line()+
      labs(title="Subscribers", y ="Number of subscribers") +
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
  output$markPlot <- renderPlot({
    ggplot(data= marketing, aes(x = Month, y = Revenue))  + geom_line(stat="identity") + 
      labs(title="Revenue", y ="Number of subscribers") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui,server)
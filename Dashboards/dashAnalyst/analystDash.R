library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

s <- function(input, output){
  
}

shinyApp(ui, s)

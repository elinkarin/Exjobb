# setwd("//ad.liu.se/home/eliha214/Documents/Exjobb/exjobb/")
setwd("~/Documents/Exjobb/git/exjobb/")
source("main1.R")
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(4,
           dateInput(inputId = "startDate",
                     label = h3("Start date"),
                     weekstart=1)),
    column(4,
           dateInput(inputId = "endDate",
                     label = h3("End date"),
                     weekstart=1)),
    column(4,
           selectInput(inputId = "objective",
                        label = h3("Objective function"),
                        choices = list("Variance" = "variance", "Expected Shortfall" = "cvar",
                                       "Utility" = "utility")
                        ,selected = 1))
  ),
  fluidRow(
    column(4, 
           numericInput(inputId = "commission", 
                        label = h3("Commission rate, percent"), 
                        value = 2)),
    column(4, 
           numericInput(inputId = "lambda", 
                        label = h3("Lambda, [0,1]"), 
                        value = 0.5)),
    column(4, 
           numericInput(inputId = "cash", 
                        label = h3("Cash, MNOK"), 
                        value = 15011))
  ),
  fluidRow(
    column(4,
           checkboxGroupInput(inputId ="instrument",
                       label = h3("Instruments"), 
                       choices = list("Aluminium" = "aluminium",
                                      "Standard ingot premium" = "ingot",
                                      "Alumina" = "alumina",
                                      "Coke" = "coke",
                                      "Fuel oil" = "oil",
                                      "Caustic soda" = "caustic",
                                      "Pitch" = "pitch",
                                      "Coal" = "coal",
                                      "USD/NOK" = "usdnok",
                                      "BRL/NOK" = "brlnok",
                                      "EUR/NOK" = "eurnok"),
                       selected = list("aluminium","ingot","alumina","coke","oil","caustic","pitch","coal","usdnok","brlnok","eurnok"))),
    column(4, 
           numericInput(inputId = "months", 
                        label = h3("Hedge length, months"), 
                        value = 48)),
    column(4,
           fileInput(inputId = "data",
                     label = h3("Data file"))),
  ),
  fluidRow(
    column(4, 
           numericInput(inputId = "kappa", 
                        label = h3("Kappa"), 
                        value = 5000)),
    column(4,
           numericInput(inputId = "saveCash",
                        label = h3("Percentage of income to use in portfolio"),
                        value = 100)),
    column(4,
           numericInput(inputId = "percentExposure",
                        label = h3("Percentage of exposure to hedge"),
                        value = 100))
  ),
  fluidRow(
    column(4,
           actionButton(inputId = "go",
                        label = h3("Optimize!")))
  ),
  fluidRow(
    column(4,
           h3("Put Options"),
           tableOutput("put")),
    column(4,
           h3("Long Forwards"),
           tableOutput("long")),
    column(4,
           h3("Short Forwards"),
           tableOutput("short"))
  ),
  fluidRow(
    column(4,
           h3("StatTest Value"),
           tableOutput("value")),
    column(4,
           h3("StatTest Objective"),
           tableOutput("obj"))
  )
  

)

server <- function(input, output) {
  
  optTable <- eventReactive(input$go, {
    optimera(input$startDate, input$endDate, input$objective, input$commission,
             input$lambda, input$cash, input$instrument, input$months, input$data,
             input$kappa, input$saveCash, input$percentExposure)
  })
  
  output$put <- renderTable({optTable()[1]})
  output$long <- renderTable({optTable()[2]})
  output$short <- renderTable({optTable()[3]})
  output$value <- renderTable({optTable()[4]})
  output$obj <- renderTable({optTable()[5]})
}

test <- function(vect) {
  return(sum(vect))
}

shinyApp(ui = ui, server = server)
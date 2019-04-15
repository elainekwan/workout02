# author: Elaine Kwan
# date: 4/7/19
# assignment: workout2 

library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)

#functions used to generate plot
#' @title future value
#' @description computes the future value of an investment assuming compound interest
#' @param amount: initial invested amount (numeric)
#' @param rate: annual rate of return (numeric)
#' @param years: number of years (numeric)
#' @return FV, the future value of an initial investment
future_value = function(amount, rate, years){
  FV = amount*(1+rate)^years
  return(FV)
}

#' @title future value of an annuity
#' @description computes the future value of an annuity 
#' @param contrib: how much you deposit at the end of each year (numeric)
#' @param rate: annual rate of return (numeric)
#' @param years: number of years (numeric)
#' @return FVA, the future value of the annuity
annuity = function(contrib,rate, years){
  FVA = contrib*(((1+rate)^years-1)/rate)
  return(FVA)
}

#' @title future value of growing annuity
#' @description computes the future value of a growing annuity  
#' @param contrib: first contribution (numeric)
#' @param rate: annual rate of return (numeric)
#' @param growth: annual rate of return (numeric)
#' @param years: number of years (numeric)
#' @return future value of growing annuity
growing_annuity = function(contrib,rate,growth,years){
  FVGA = contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
  return(FVGA)
}

#Define a UI 
ui <- fluidPage(
  titlePanel("Investment Scenarios"),
  
  fluidRow(
    column(4,
           sliderInput("initAmount",
                       label = "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       value = 1000,
                       pre = "$")),
    
    column(4,
           sliderInput("returnRate",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 5)),
    
    column(4, 
           sliderInput("years",
                       label = "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 20))
  ),
  
  fluidRow(
    
    column(4,
           sliderInput("annualContr",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       value = 2000,
                       pre = "$")),
    
    column(4,
           sliderInput("growthRate",
                       label = "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)),
    
    column(4, 
           selectInput("facet", "Facet?",
                       choices = c("No", "Yes")))
  ),
  verticalLayout(
    h4("Timelines"),
    plotOutput("distPlot"),
    wellPanel(
      h4("Balance"),
      verbatimTextOutput('balances')
    )
    
  )
)

# Define server logic required to draw timeline graph
server <- function(input, output) {
  
  modalities <- reactive({
    invest_period = input$years
    init_investment = input$initAmount
    rate = input$returnRate/100
    growth = input$growthRate/100
    contribution = input$annualContr
    
    mode1 = c()
    mode2 = c()
    mode3 = c()
    
    for (i in 0:(invest_period)){
      mode1[i+1] = future_value(init_investment,rate,i)
      mode2[i+1] = mode1[i+1]+ annuity(contribution, rate, i)
      mode3[i+1] = mode1[i+1] + growing_annuity(contribution,rate, growth,i)
    }
    
    #create data table for non-facetted ggplot
    year = 0:input$years
    modalities = data.frame(year,mode1,mode2,mode3)
    names(modalities)[names(modalities)=="mode1"] <- "no_contrib"
    names(modalities)[names(modalities)=="mode2"] <- "fixed_contrib"
    names(modalities)[names(modalities)=="mode3"] <- "growing_contrib"
    
    modalities
  })
    
   output$distPlot <- renderPlot({
     
     #create data table for facetted ggplot
     facet_modalities = melt(modalities(), id.vars = "year")
     #head(facet_modalities)
     
     if (input$facet == 'No'){
     
     ggplot(facet_modalities, aes(x = year, y = value)) + 
         geom_point(aes(color = variable)) + 
         geom_line(aes(color = variable)) +
         labs(x = "Year") +  labs(y = "Dollars ($)") +  
         labs(title = "Three Modes of Investing") 
       
     } else if (input$facet == 'Yes'){
     ggplot(facet_modalities, aes(x = year, y = value)) + 
         geom_point(aes(color = variable)) + 
         geom_line(aes(color = variable)) +
         geom_area(aes(fill = variable),alpha = 0.5) + 
         labs(x = "Year") +  labs(y = "Dollars ($)") +  
         labs(title = "Three Modes of Investing") + facet_wrap(~variable)
     }
    
   })
   
   output$balances = renderPrint({
     print(modalities(), print.gap = 2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


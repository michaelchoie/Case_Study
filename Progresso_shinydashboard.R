# Consolidate data visualizations into a shinydashboard app

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(shiny)
library(shinydashboard)

setwd("~/Desktop/Data Science Interview Prep/Projects/Progresso_EDA")
data <- read_csv("soup_new.csv")

data
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data[, c("Winter", "Region")] <- lapply(data[, c("Winter", "Region")], function(x) as.factor(x))
sum(is.na(data))

# Create tidy data to compare volume and prices better
tidy_data <- data %>% 
  gather("Company", "Volume", Volume.Campbell:Volume.Progresso) %>%
  select(-c(Price.Campbell:Total_Volume)) %>%
  bind_cols(data %>% gather("Company2", "Price", Price.Campbell:Price.Progresso) %>% select(Price))

tidy_data$Company <- as.factor(str_extract(tidy_data$Company, "(?<=\\.).*"))
tidy_data <- tidy_data %>% mutate(month = month(Date), year = year(Date))

###################################################################################################################
# Shinydashboard
###################################################################################################################

# shinydashbaord has 3 componenets: header, sidebar, body
header <- dashboardHeader(title = "Progresso Pricing Analytics")
sidebar <- dashboardSidebar(disable = TRUE)

# Combine fluid rows to make the body

# Key performance indicators using dynamic value boxes
frow1 <- fluidRow(
  valueBoxOutput("volumeSold"),
  valueBoxOutput("marketShare"),
  valueBoxOutput("revenueGenerated")
)

# Price and sales by brand
frow2 <- fluidRow(
  
  # first box for sales by quarter and region bar
  box(
    title = "Price by Brand",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("priceByBrand", height = "300px")
  ),
  
  # second box for sales by year and region bar
  box(
    title = "Volume by Brand",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("volumeByBrand", height = "300px")
  )
  
)

# Time series analysis
frow3 <- fluidRow(
  
  # First box for market share time series
  box(
    title = "Market Share Time Series",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("marketShareTimeSeries", height = "300px")
  ),
  
  # Second box for price time series
  box(
    title = "Revenue Time Series",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("revenueTimeSeries", height = "300px")
  )
  
)

# Fourth row will contain a table with a drop down selector to see the data
frow4 <- fluidRow(
  
  tabBox(
    title = "Data Viewer",
    width = 12,
    id = "dataTabBox",
    
    tabPanel(
      title = "By Region",
      dataTableOutput("byRegion")
    ),
    
    tabPanel(
      title = "By Month",
      dataTableOutput("byMonth")
    ),
    
    tabPanel(
      title = "By Year",
      dataTableOutput("byYear")
    )
  )
  
)

# Output for the source info
frow5 <- fluidRow(
  
  infoBoxOutput("sourceBox", width = 8),
  infoBoxOutput("nameBox", width = 4)
  
)

body <- dashboardBody(frow1, frow2, frow3, frow4, frow5)

# Combine all 3 componenets to form the UI
ui <- dashboardPage(skin = "red", header, sidebar, body)

# Create the server functions for the dashboard
server <- function(input, output) {
  
  # fluid row 1, kpi 1: volume sold
  output$volumeSold <- renderValueBox({
    
    valueBox(
      formatC(sum(tidy_data$Volume[tidy_data$Company == "Progresso"]), format = "d", big.mark = ","),
      "Volume Sold",
      icon = icon("shopping-cart"),
      color = "blue"
    )
    
  })
  
  # fluid row 1, kpi 2: market share
  output$marketShare <- renderValueBox({
    
    valueBox(
      paste0(round(sum(tidy_data$Volume[tidy_data$Company == "Progresso"]) / sum(tidy_data$Volume) * 100, 2), "%"),
      "Market Share",
      icon = icon("pie-chart"),
      color = "blue"
    )
    
  })
  
  # fluid row 1, kpi 3: revenue generated in latest year
  output$revenueGenerated <- renderValueBox({
    
    valueBox(
      paste0("$", formatC(round(tidy_data %>% filter(year(Date) == max(year(Date)), Company == "Progresso") %>% summarize(sum(Volume * Price)), 2), format = "d", big.mark = ",")),
      "Revenue Generated in latest year",
      icon = icon("eur"),
      color = "green"
    )
    
  })
  
  # fluid row 2, graph 1: price by company bar graph
  output$priceByBrand <- renderPlot({
    
    ggplot(tidy_data %>% group_by(Company) %>% summarize(mean_price = mean(Price)), aes(x = Company, y = mean_price, fill = Company)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = round(mean_price, 2), vjust = -1))
    
  })
  
  # fluid row 2, graph 2: volume by company circle graph
  output$volumeByBrand <- renderPlot({
    
    ggplot(tidy_data %>% group_by(Company) %>% summarize(brand_volume = sum(Volume)) %>% mutate(market_share = brand_volume/sum(brand_volume)), 
           aes(x = "", y = market_share, fill = Company)) +
      geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
      geom_text(aes(label = Company), position = position_stack(vjust = 0.5))
    
  })
  
  # fluid row 3, graph 1: Time series of market share
  output$marketShareTimeSeries <- renderPlot({
    
    ggplot(date_data %>% group_by(month, Company) %>% summarize(brand_volume = sum(Volume)) %>% mutate(market_share = brand_volume/sum(brand_volume)),
           aes(x = month, y = market_share, color = Company)) + 
      geom_point() + geom_line() + scale_x_continuous(breaks = 1:12)
    
  })
  
  # fluid row 3, graph 2: Time series of price 
  output$revenueTimeSeries <- renderPlot({
    
    ggplot(date_data %>% group_by(Company, month) %>% mutate(revenue = Price * Volume) %>% summarize(mean_revenue_by_month = mean(revenue)), 
           aes(x = month, y = mean_revenue_by_month, color = Company)) + 
      geom_point() + geom_line() + scale_x_continuous(breaks = 1:12)
    
  })
  
  # fluid row 4: data table "Sales by Model","Sales by Quarter","Prior Year Sales"
  output$byRegion <- renderDataTable(tidy_data %>% group_by(Region) %>% summarize(mean(Volume)))
  output$byMonth <- renderDataTable(tidy_data %>% group_by(month) %>% summarize(mean(Volume)))
  output$byYear <- renderDataTable(tidy_data %>% group_by(year) %>% summarize(mean(Volume)))
  
  # fluid row 5: source of the data
  output$sourceBox <- renderInfoBox({
    
    infoBox(
      title = "Data Source",
      value = "IRI",
      color = "purple",
      icon = icon("tachometer")
    )
    
  })
  
  # fluid row 5: source of the data
  output$nameBox <- renderInfoBox({
    
    infoBox(
      title = "Michael Choie",
      value = "Progresso Pricing Strategy Analysis",
      color = "purple",
      icon = icon("code")
    )
    
  })
  
}

# Render the dashboard as a shiny app
shinyApp(ui, server)

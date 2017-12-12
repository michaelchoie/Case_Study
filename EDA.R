# Conduct EDA and analysis on Progresso data

rm(list=ls())

library(data.table)
library(ggplot2)
library(tidyr)
library(forecast)

setwd("~/Desktop/Data Science/Capstone/Progresso")
soup <- fread("soup_new.csv")

# Data wrangling
sum(is.na(soup))
soup[, Date := as.Date(Date, format = "%m/%d/%Y")]
soup[, m := month(Date)][, y := year(Date)][, winter := (m < 4 | m > 9)]
soup[, Progresso.Share := Volume.Progresso/Total_Volume][, Campbell.Share := Volume.Campbell/Total_Volume]

# Make the wide table longer
# Use setDT() to convert dataframe to data.table without creating a copy in memory (does by reference)
b <- setDT(soup
           %>% gather(brand, price, Price.Campbell:Price.Progresso)
           %>% gather(brand2, volume, Volume.Campbell:Volume.Progresso))
c <- b[brand == "Price.Campbell" | brand == "Price.Progresso",]
c[, brand := factor(brand, levels = c("Price.Progresso", "Price.Campbell"))]

d <- c[, .(Price.ByBrand = mean(price)), by = brand]

# Plot price by brand (Progresso vs. Campbell)
ggplot(d, aes(x = brand, y = Price.ByBrand, fill = brand)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Price.ByBrand, 1)), vjust = -1)

e <- b[brand2=="Volume.Campbell" | brand2=="Volume.Progresso", ]
e[, brand2 := factor(brand2, levels=c("Volume.Progresso", "Volume.Campbell"))]

f <- e[, .(Volume.ByBrand = mean(volume)), by = brand2]

# Plot volume by brand (Progresso vs. Campbell)
ggplot(f, aes(x = brand2, y = Volume.ByBrand, fill = brand2)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Volume.ByBrand, 1)), vjust = -1)

# Get seperate graphs for each Region
brand1region <- c[, .(Price.ByBrandRegion = mean(price)), by = .(brand, Region)]
PriceByRegion <- ggplot(brand1region, aes(x = brand, y = Price.ByBrandRegion, fill = brand)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Price.ByBrandRegion, 1)), vjust = -0.5) +
    facet_wrap(~Region, nrow = 1)

brand2region <- e[, .(Volume.ByBrandRegion = mean(volume)), by = .(brand2, Region)]
VolumeByRegion <- ggplot(brand2region, aes(x = brand2, y = Volume.ByBrandRegion, fill = brand2)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Volume.ByBrandRegion, 1)), vjust = -0.5) +
    facet_wrap(~Region, nrow = 1)

# Combine two plots (volume by region & price by region) on one page
library('cowplot')
plot_grid(VolumeByRegion, PriceByRegion, nrow=2)

# Time Series of Sales
timeSeries <- e[, .(Volume.ByTime = mean(volume)), .(brand2, Date)]
ggplot(timeSeries, aes(x = Date, y = Volume.ByTime, fill = brand2)) + geom_area()

# Category Sales by Month
j <- soup[, .(Volume.ByYearMonthRegion = sum(Total_Volume)), by = .(y, m, Region)]
k <- j[, .(MeanVolume.ByMonthRegion =  mean(Volume.ByYearMonthRegion), StdVolume.ByMonthRegion = sd(Volume.ByYearMonthRegion)),
       by = .(m, Region)]

ggplot(k[, winter := m < 4 | m > 9], aes(x = m, y = MeanVolume.ByMonthRegion, fill = winter)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymax = MeanVolume.ByMonthRegion + StdVolume.ByMonthRegion,
                      ymin = MeanVolume.ByMonthRegion - StdVolume.ByMonthRegion)) +
    facet_wrap(~Region, nrow=2)

# Time Series of Price
l <- c[, .(Price.ByMonthRegion = mean(price)), by = .(brand, m, Region)]
ggplot(l, aes(x = m, y = Price.ByMonthRegion, color = brand)) +
    geom_line() + geom_point() + facet_wrap(~Region, nrow = 1)

n <- soup[, .(IRI_KEY,m,y,Region,Date,Progresso.Share,Campbell.Share)]
o <- melt(data = n, id.vars = c("IRI_KEY", "m", "y", "Region", "Date"), measure.vars = c("Progresso.Share", "Campbell.Share"),
          variable.name = "brand", value.name = "Share")
p <- o[, .(Share.ByYearMonth = mean(Share)), by = .(brand, y, m)]
p[, brand := factor(brand, levels = c("Campbell.Share", "Progresso.Share"))]

ggplot(p, aes(x = m, y = Share.ByYearMonth, group = brand, color = brand)) +
    geom_line() + geom_point() + facet_wrap(~y, nrow = 1)

# Filter on regions
lPlots <- replicate(4, list(NA))

regionFilter <- function(area) {
    o1 <- o[Region == area]
    p1 <- o1[, .(Share.ByYearMonth = mean(Share)), by = .(brand, y, m)]
    p1[, brand := factor(brand, levels = c("Campbell.Share", "Progresso.Share"))]
    return(ggplot(p1, aes(x = m, y = Share.ByYearMonth, group = brand,color=brand)) +
               geom_line() + geom_point() + facet_wrap(~y, nrow=1))
}

lPlots <- lapply(unique(o[,Region]), function(x) regionFilter(x))
plot_grid(lPlots[[1]], lPlots[[2]], lPlots[[3]], lPlots[[4]], nrow = 4)

# Create simple linear regression
linearModel1 <- lm(Volume.Progresso ~ Price.Progresso, data = soup)
summary(linearModel1)

# Create semi-log model
soup[, c("logVolume.Campbell", "logVolume.Other", "logVolume.PL", "logVolume.Progresso") := lapply(.SD, log), .SDcols = 5:8]
semiLogModel1 <- lm(logVolume.Progresso ~ Price.Progresso, data = soup)
summary(semiLogModel1)

# Create log-log model
soup[, c("logPrice.Campbell", "logPrice.Other", "logPrice.PL", "logPrice.Progresso") := lapply(.SD, log), .SDcols = 9:12]
logModel1 <- lm(logVolume.Progresso ~ logPrice.Progresso, data = soup)
summary(logModel1)
logModel2 <- lm(logVolume.Progresso ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other, data = soup)
summary(logModel2)

# Create Price Elasticity Matrix, Vulnerability & Clout Brand Mapping using log-log models

progresso_pe <- logModel2
campbell_pe <- lm(logVolume.Campbell ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other, data = soup)
PL_pe <- lm(logVolume.PL ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other, data = soup)
other_pe <- lm(logVolume.Other ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other, data = soup)

PriceElasticityMatrix <- lapply(c("progresso_pe", "campbell_pe", "PL_pe", "other_pe"), function(models) get(models)$coefficients[-1])
PriceElasticityMatrix <- t(matrix(unlist(PriceElasticityMatrix), nrow = 4))
Vulnerability <- round(apply(PriceElasticityMatrix, 1, sum),2)
Clout <- round(apply(PriceElasticityMatrix, 2, sum),2)
brandMap <- as.data.frame(cbind(c("Progresso", "Campbell", "PL", "Other"), Vulnerability, Clout))

ggplot(brandMap, aes(x= Vulnerability, y= Clout, label = V1)) + geom_point() +
    geom_text(aes(label=V1),hjust=0, vjust=0)

# Control for seasonality; add in the winter dummy variable
logModel3 <- lm(logVolume.Progresso ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other + winter,
                data = soup)
summary(logModel3)

# Add in month dummy that is ordered
soup[, month_dummy := factor(months(Date), levels = month.name)]

# Set December as the reference level
soup[, month_dummy := relevel(month_dummy, ref = "December")]
logModel4 <- lm(logVolume.Progresso ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other + month_dummy,
                data = soup)
summary(logModel4)

# Control for region differences
logModel5 <- lm(logVolume.Progresso ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other + month_dummy
                + Region, data = soup)
summary(logModel5)

# Analyze for each region separately
regionalModels <- replicate(4, list(NA))
unique(soup[,Region])
regionalModels <- lapply(unique(soup[,Region]),
                         function(x) lm(logVolume.Progresso ~ logPrice.Progresso + logPrice.Campbell + logPrice.PL + logPrice.Other + month_dummy, data = soup[,, by = x]))
lapply(regionalModels, summary)

# Compare models using AIC/BIC (the lower the better) - penalize for size of models [avoid overfitting]

# Linear vs semi-log vs log-log
AIC(linearModel1, semiLogModel1, logModel1)
BIC(linearModel1, semiLogModel1, logModel1)

# Models with vs without control variables
AIC(logModel1, logModel2, logModel3, logModel4, logModel5)
BIC(logModel1, logModel2, logModel3, logModel4, logModel5)

# See All 4 Models in same Table
library(texreg)
screenreg(list(logModel1, logModel2, logModel3, logModel4, logModel5))

# Create dashboard for strategy/marketing team

library(shiny)
library(shinydashboard)

# All shiny dashboards have the same three basic layout elements: header, sidebar, body
header <- dashboardHeader(title = "Progresso Pricing Analytics")

# For our simple dashboard we won't have a side bar since it is usually reserved for menu items of interactive selectors.
# Possible area of improvement after this MVP:
sidebar <- dashboardSidebar(disable = TRUE)

# The body will contain four fluid rows:

# Key performance indicators using dynamic value boxes
frow1 <- fluidRow(
    valueBoxOutput("volumeSold"),
    valueBoxOutput("marketShare"),
    valueBoxOutput("revenueGenerated")
)

# Sales by quarter, year
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
        title = "Volume by Brand"
        ,status = "primary"
        ,solidHeader = TRUE
        ,collapsible = TRUE
        ,plotOutput("volumeByBrand", height = "300px")
    )

)

# share of sales by model
frow3 <- fluidRow(

    # first box for a line graph showing the share per model
    box(
        title = "Mean Volume each Month by Region"
        ,status = "primary"
        ,solidHeader = TRUE
        ,collapsible = TRUE
        ,plotOutput("meanVolumeByMonthRegion", height = "300px")
    ),

    # second box for the sales per model bar
    box(
        title = "Price each Month by Brand",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("priceByMonthRegion", height = "300px")
        #radioButtons("radio", "Sales Type Selection", choices = as.character(unique(sales.model.tdy$Sales)))
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

# combine the three fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3, frow4, frow5)

ui <- dashboardPage(skin = "red", header, sidebar, body)

# create the server functions for the dashboard
server <- function(input, output) {

    # fluid row 1, kpi 1: volume sold
    output$volumeSold <- renderValueBox({
        valueBox(
        formatC(sum(Volume.Progresso), format = "d", big.mark = ","),
        "Volume Sold",
        icon = icon("shopping-cart"),
        color = "blue"
        )
    })

    # fluid row 1, kpi 2: market share
    output$marketShare <- renderValueBox({
        valueBox(
        paste0(round(soup[, sum(Volume.Progresso)/sum(c(Volume.Progresso + Volume.Campbell + Volume.PL + Volume.Other))] * 100,2), "%"),
        "Market Share",
        icon = icon("pie-chart"),
        color = "blue"
        )
    })

    # fluid row 1, kpi 3: revenue generated in latest year
    output$revenueGenerated <- renderValueBox({
        valueBox(
        paste0("$", formatC(round(soup[, sum(Volume.Progresso * Price.Progresso, by = max(year(soup[, Date])))], 2), format = "d", big.mark = ",")),
        "Revenue Generated in latest year",
        icon = icon("eur"),
        color = "green"
        )
    })

    # fluid row 2, graph 1: sales by region quarter bar graph
    output$priceByBrand <- renderPlot({
        ggplot(d, aes(x = brand, y = Price.ByBrand, fill = brand)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(Price.ByBrand, 1)), vjust = -1)
    })

    # fluid row 2, graph 2: sales be region current/prior year
    output$volumeByBrand <- renderPlot({
        ggplot(f, aes(x = brand2, y = Volume.ByBrand, fill = brand2)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(Volume.ByBrand, 1)), vjust = -1)
    })

    # fluid row 3, graph 1: Bar chart with confidence intervals faceted by region
    output$meanVolumeByMonthRegion <- renderPlot({
        ggplot(k[, winter := m < 4 | m > 9],
        aes(x = m, y = MeanVolume.ByMonthRegion, fill = winter)) +
        geom_bar(stat="identity") +
        geom_errorbar(aes(ymax = MeanVolume.ByMonthRegion + StdVolume.ByMonthRegion,
        ymin = MeanVolume.ByMonthRegion - StdVolume.ByMonthRegion)) +
        facet_wrap(~Region, nrow=2)
    })

    # fluid row 3, graph 2: Time series of price faceted by region
    output$priceByMonthRegion <- renderPlot({
        ggplot(l, aes(x = m, y = Price.ByMonthRegion, color = brand)) +
        geom_line() + geom_point() + facet_wrap(~Region, nrow = 1)
    })

    # fluid row 4: data table "Sales by Model","Sales by Quarter","Prior Year Sales"
    output$byRegion <- renderDataTable(soup[,, by = Region])
    output$byMonth <- renderDataTable(soup[,, by = month_dummy])
    output$byYear <- renderDataTable(soup[,, by = year(Date)])

    # fluid row 5: source of the data
    output$sourceBox <- renderInfoBox({
        infoBox(
        title = "Source",
        value = "Data Driven Decision Making - NYU Stern",
        color = "purple",
        icon = icon("tachometer")
        )
    })

    # fluid row 5: source of the data
    output$nameBox <- renderInfoBox({
        infoBox(
        title = "Michael Y Choie",
        value = "R Code of Pricing Strategy Analysis",
        color = "purple",
        icon = icon("code")
        )
    })

}

# Render the dashboard as a shiny app
shinyApp(ui, server)

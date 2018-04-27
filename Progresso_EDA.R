# Evaluate current pricing strategy of Progresso
# Evaluate performance across geographies
# Develop a regression based demand model to analyze price elastiity
# Suggest alternative strategies

# Data Source: IRI

rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot) 
library(stringr)
library(lubridate)
library(caret)

setwd("~/Desktop/Data Science Interview Prep/Projects/Progresso_EDA")
data <- read_csv("soup_new.csv")

###################################################################################################################
# EXPLORATORY DATA ANALYSIS
###################################################################################################################

###################################################################################################################
# 1. Clean data
###################################################################################################################

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

###################################################################################################################
# 2. Analyze data 
###################################################################################################################

summary(tidy_data)

# Investigate outliers (IQR * 1.5)
# For outliers, see if the Price to Volume ratio adheres to a reasonable slope
boxplot(tidy_data$Price)
ggplot(tidy_data, aes(x = Date, y = Price)) + geom_point(alpha = 0.5) + geom_smooth() 

boxplot(tidy_data$Volume)
ggplot(tidy_data, aes(x = Date, y = Volume)) + geom_point(alpha = 0.5) + geom_smooth()

ggplot(tidy_data, aes(x = Volume, y = Price)) + geom_point(alpha = 0.5) + geom_smooth() + geom_hline(aes(yintercept = IQR(tidy_data$Price) * 1.5), color = "red")

tidy_data[tidy_data$Price >= 10, ]

# Highly likely that prices above 10 were a data entry error - make change
tidy_data[tidy_data$Price >= 10, "Price"] <- tidy_data[tidy_data$Price >= 10, "Price"] * .1

###################################################################################################################
# 3. Analyze differences between groups - for understanding data & knowing what to add into regression models
###################################################################################################################

# Check for normality - tests are robust but to stay on the safe side
hist(tidy_data$Price, breaks = seq(0, 10 ,0.1))
hist(tidy_data$Volume, breaks = seq())

# Is the difference of sales/price between companies statistically significant?
fit <- aov(Volume ~ Company, data = tidy_data)
p1 <- summary(fit)[[1]]$"Pr(>F)"[[1]]
summary(fit)

fit <- aov(Price ~ Company, data = tidy_data)
p2 <- summary(fit)[[1]]$"Pr(>F)"[[1]]
summary(fit)

# Are sales/price changes from winter to nonwinter statistically significant?
test <- t.test(tidy_data$Volume[tidy_data$Winter == "NonWinter"], tidy_data$Volume[tidy_data$Winter == "Winter"])
p3 <- test$p.value

test <- t.test(tidy_data$Price[tidy_data$Winter == "NonWinter"], tidy_data$Price[tidy_data$Winter == "Winter"])
p4 <- test$p.value

# Are sales/price changes between regions statistically significant?
fit <- aov(Price ~ Region, data = tidy_data)
p5 <- summary(fit)[[1]]$"Pr(>F)"[[1]]
summary(fit)

fit <- aov(Price ~ Volume, data = tidy_data)
p6 <- summary(fit)[[1]]$"Pr(>F)"[[1]]
summary(fit)

# Account for multiple comparisons problem - use Holm-Bonferroni correction method
# Simple, doesn't assume independence, but less conservative than Bonferroni
p.adjust(p = c(p1, p2, p3, p4, p5, p6), method = "holm", n = 6)

###################################################################################################################
# 4. Analyze market share
###################################################################################################################

market_share <- tidy_data %>% 
  group_by(Company, Region) %>% 
  summarize(brand_volume = sum(Volume)) 

ggplot(market_share %>% group_by(Company) %>% summarize(brand_volume = sum(brand_volume)) %>% mutate(market_share = brand_volume/sum(brand_volume)), 
        aes(x = "", y = market_share, fill = Company)) +
    geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
    geom_text(aes(label = Company), position = position_stack(vjust = 0.5))

# Analyze market share by region
ggplot(market_share %>% group_by(Region, Company) %>% summarize(brand_volume = sum(brand_volume)) %>% mutate(market_share = brand_volume/sum(brand_volume)), 
         aes(x = "", y = market_share, fill = Company)) +
    geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
    geom_text(aes(label = Company), position = position_stack(vjust = 0.5)) + 
    facet_wrap(~Region)

# Market share time series
date_data <- tidy_data %>% mutate(month = month(Date), year = year(Date))
date
market_share_time_series <- ggplot(date_data %>% group_by(month, Company) %>% summarize(brand_volume = sum(Volume)) %>% mutate(market_share = brand_volume/sum(brand_volume)),
                                     aes(x = month, y = market_share, color = Company)) + 
                                geom_point() + geom_line() + scale_x_continuous(breaks = 1:12)

###################################################################################################################
# 5. View pricing models
###################################################################################################################

# Summary stats on mean Company prices
ggplot(tidy_data %>% group_by(Company) %>% summarize(mean_price = mean(Price)), aes(x = Company, y = mean_price, fill = Company)) + 
  geom_bar(stat = "identity") + geom_text(aes(label = round(mean_price, 2), vjust = -1))

# Compare pricing strategies by company (seasonality)
pricing_time_series <- ggplot(date_data %>% group_by(Company, month) %>% summarize(mean_price_by_month = mean(Price)), 
                              aes(x = month, y = mean_price_by_month, color = Company)) + 
                            geom_point() + geom_line() + scale_x_continuous(breaks = 1:12)

# Price strategy by region
ggplot(date_data %>% group_by(Region, Company, month) %>% summarize(mean_price_by_month = mean(Price)), 
       aes(x = month, y = mean_price_by_month, color = Company)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = 1:12) + 
  facet_wrap(~Region)

# Analyze price trends
ggplot(date_data %>% group_by(Company, year) %>% summarize(mean_price_by_year = mean(Price)), aes(x = year, y = mean_price_by_year, color = Company)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = 1:length(year))

# Price trends by region
ggplot(date_data %>% group_by(Region, Company, year) %>% summarize(mean_price_by_year = mean(Price)), aes(x = year, y = mean_price_by_year, color = Company)) + 
  geom_point() + geom_line() + scale_x_continuous(breaks = 1:length(year)) + facet_wrap(~Region)

# Plot total revenue time series
revenue_time_series <- ggplot(date_data %>% group_by(Company, month) %>% mutate(revenue = Price * Volume) %>% summarize(mean_revenue_by_month = mean(revenue)), 
                              aes(x = month, y = mean_revenue_by_month, color = Company)) + 
                        geom_point() + geom_line() + scale_x_continuous(breaks = 1:12)

# Compare market share to pricing strategy
cowplot::plot_grid(pricing_time_series, market_share_time_series, revenue_time_series)

###################################################################################################################
# LOG-LOG REGRESSION MODEL
###################################################################################################################

# Construct log-log model to ascertain price elasticity of demand; control for season & region
# Log transformations turn geometric relationships into arithmetic

# Factorize month; set December as reference level
data$month_dummy <- factor(months(data$Date), levels = month.name)
data <- within(data, month_dummy <- relevel(data$month_dummy, ref = "December"))

# Log transform prices
data$LogPrice.Progresso <- log(data$Price.Progresso)
data$LogPrice.Campbell <- log(data$Price.Campbell)
data$LogPrice.PL <- log(data$Price.PL)
data$LogPrice.Other <- log(data$Price.Other)

log_model <- lm(LogPrice.Progresso ~ LogPrice.Campbell + LogPrice.PL + LogPrice.Other + month_dummy + Region, data = data)
summary(log_model)

# Evaluate residuals - passes normality test
qqnorm(log_model$residuals)

# Evaluate generalizability via k-fold cross validation across multiple regression models
train_index <- createDataPartition(data$LogPrice.Progresso, p = 0.8, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]
cross_validation <- trainControl("cv", 10)

# Create a function that can create different regression models with same parameters
regression_model <- function(method) {
  model <- train(LogPrice.Progresso ~ LogPrice.Campbell + LogPrice.PL + LogPrice.Other + month_dummy + Region,
               data = train,
               method = method,
               trControl = cross_validation)
  
  predictions <- predict(model, test[, !(colnames(data) %in% "LogPrice.Progresso")])
  
  rmse <- RMSE(test$LogPrice.Progresso, predictions)
  
  return(list(model,predictions, rmse))
}

log_model <- regression_model("lm")
ridge_model <- regression_model("ridge")
lasso_model <- regression_model("lasso")
enet_model <- regression_model("enet")

model_performance <- data.frame("performance" = c(log_model[[3]], ridge_model[[3]], lasso_model[[3]], enet_model[[3]]),
                                "type" = c("log", "ridge", "losso", "elastic net"))

# Plot results - not much difference between models, but ridge/elastic net performed best
model_performance
ggplot(model_performance, aes(x = type, y = performance, fill = type)) + geom_bar(stat = "identity") 

###################################################################################################################
# CLOUT, VULNERABILITY, BRAND MAP
###################################################################################################################

# Construct cross-price elasticity matrix to analyze competition
# TODO

###################################################################################################################
# Shinydashboard
###################################################################################################################
# Packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(dplyr)

# I have Already Set my Working Directory

# Modeling a Time Series Trend using Regression

# Loading in the File
df <- read.csv("annual_perf_s&p500.csv")
View(df)

# Engineering a Better Column for Time Period
df$Year <- str_sub(df$Date, -4)

# Filtered DataFrame for recent Time Period
df2 <- df |> filter(Year > 1975)

# Serial Number Column for each Year's Recording
df2$Time <- 1:nrow(df2)

# Creating a Bar Chart plot showing Annual S&P500 Stock Market Index at the End of Year's Recording, for the Past 50 Years
ggplot(data = df2, mapping = aes(x = Year, y = Value)) +
  geom_bar(stat = "identity") +
  labs(title = "S&P500 End of Year Price, 
       1928 to 2025", x = "Year", y = "Price")

# Creating a Time Series plot showing Annual S&P500 Stock Market Index at End of Year's Recording, for the Past 50 Years
ggplot(data = df2, mapping = aes(x = Year, y = Value, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "S&P500 End of Year Price, 
       1928 to 2025", x = "Year", y = "Price")

# Assigning Each of the Past 50 Years to Certain Time Period Categories
df2 <- df2 |> mutate("Yearcat" = case_when(
    Year >= 1975 & Year <= 1988 ~ "Very Early",
    Year > 1988 & Year <= 2000 ~ "Earlier",
    Year > 2000 & Year <= 2012 ~ "Previous Time",
    Year > 2012 & Year <= 2025 ~ "Modern Era"
))

# View(df2)

# Creating a Time Series Plot showing Year Category-Based Stock Prices
mean_data <- df2 |>
  group_by(Yearcat) |>
  summarise(avg_price = mean(Value))

# View(mean_data)

# Bar Plot Visualizing Average S&P500 Price at the End of Each Year Range Category
ggplot(data = mean_data, mapping = aes(x = Yearcat, y = avg_price)) +
  geom_bar(stat = "identity") +
  labs(title = "S&P 500 Price by Year Category", 
     x = "Year Category", y = "S&P500 Price")
  
# Creating Dummy variables Corresponding to each Quarter 
df2$Q1 <- ifelse(grepl("Very Early", df2$Yearcat), 1, 0)
df2$Q2 <- ifelse(grepl("Earlier", df2$Yearcat), 1, 0)
df2$Q3 <- ifelse(grepl("Previous Time", df2$Yearcat), 1, 0)
df2$Q4 <- ifelse(grepl("Modern Era", df2$Yearcat), 1, 0)
  
# Using Regression with the Time variable to Generate a Regression equation for Forecasting
spreg <- lm(Value ~ Time, data = df2)
summary(spreg)
  
# Creating a Vector of Predicted values Generated from the Regression above
spregv = predict(spreg)

# Creating Functions for Accuracy Measures of the Regression Model

mae <- function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse <- function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse <- function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

# Calculating Accuracy Measures with vector of Actual values and vector of Predicted values as Inputs
mae(df2$Value, spregv)
mse(df2$Value, spregv)
rmse(df2$Value, spregv)
mape(df2$Value, spregv)
  
# Using Multiple Regression with the Time and Quarters variables to Generate a Regression equation for Forecasting
spreg2 <- lm(Value ~ Time + Q1 + Q2 + Q3 + Q4, data = df2)
summary(spreg2)
  
# Creating a Vector of Predicted values generated from the Multiple Regression above
spreg2_v = predict(spreg2)
  
# Calculating Accuracy Measures with a Vector of Actual values and a vector of Predicted values as Inputs
mae(df2$Value, spreg2_v)
mse(df2$Value, spreg2_v)
rmse(df2$Value, spreg2_v)
mape(df2$Value, spreg2_v) # Says "+ infinity" # the MAPE is Lower than before; this indicates that when we Model both Seasonality and Trend, 
# ============================================
# STA 137 Final Project
# The Popularity of Drake (2010–2024)
# Roy Ho
# ============================================

# ------------------ PACKAGES ------------------
library(forecast)
library(ggplot2)
library(tidyverse)
library(tseries)

# ------------------ DATA PROCESSING ------------------

# Read CSV (adjust path if needed)
drake_data <- read.csv("data/Drake.csv", sep = ",", header = FALSE, skip = 1)

# Rename columns
colnames(drake_data) <- c("Month", "Trend")

# Convert Month column to Date
drake_data$Month <- as.Date(paste0(drake_data$Month, "-01"), format="%Y-%m-%d")

# Remove unwanted text and convert Trend to numeric
drake_data$Trend <- gsub("Drake: \\(United States\\)", NA, drake_data$Trend)
drake_data$Trend <- as.numeric(drake_data$Trend)

# Remove NA rows
drake_data <- drake_data[!is.na(drake_data$Month), ]

# ------------------ SUMMARY ------------------
summary(drake_data)

# ------------------ TIME SERIES OBJECT ------------------
drake_ts <- ts(
  drake_data$Trend,
  start = c(2010, 1),
  frequency = 12
)

# ------------------ TIME SERIES PLOT ------------------
ggplot(drake_data, aes(x = Month, y = Trend)) +
  geom_line(color = "black") +
  labs(title = "Drake's Trend/Popularity Over Time",
       x = "Month",
       y = "Trend") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# ------------------ BOX PLOTS ------------------

drake_data$Year <- format(drake_data$Month, "%Y")

# Boxplot by Year
ggplot(drake_data, aes(x = Year, y = Trend)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Drake's Box Plot by Year",
       x = "Year",
       y = "Trend") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Overall Boxplot
ggplot(drake_data, aes(y = Trend)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs(title = "Drake's Overall Trend Boxplot",
       x = "",
       y = "Trend")

# ------------------ HISTOGRAM ------------------

ggplot(drake_data, aes(x = Trend)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +
  labs(title = "Histogram of Drake's Trend/Popularity",
       x = "Trend",
       y = "Frequency")

# ------------------ DECOMPOSITION ------------------

drake_ts <- ts(na.omit(drake_data$Trend), frequency = 12, start = c(2010,1))

break_down <- decompose(drake_ts, type = "additive")
plot(break_down)

# ------------------ STATIONARITY TEST ------------------

adf_test <- adf.test(drake_ts)
print(adf_test)

# ------------------ ACF & PACF ------------------

Acf(drake_ts, main = "ACF of Drake's Trend")
Pacf(drake_ts, main = "PACF of Drake's Trend")

# ------------------ ARMA MODEL ------------------

drake_arma <- Arima(drake_ts, order = c(1, 0, 1))
summary(drake_arma)

# Auto ARIMA
drake_auto_arima <- auto.arima(drake_ts)
summary(drake_auto_arima)

# ------------------ NORMALITY TEST ------------------

shapiro_test_residuals <- shapiro.test(residuals(drake_arma))
print(shapiro_test_residuals)

# ------------------ RESIDUAL CHECK ------------------

checkresiduals(drake_arma)

# ------------------ FORECAST (NEXT 12 MONTHS) ------------------

future_pred <- forecast(drake_arma, h = 12)

autoplot(future_pred) +
  labs(title = "Prediction of Drake's Trend for the Next Year",
       x = "Year",
       y = "Trend")

# ------------------ SPECTRAL ANALYSIS ------------------

spectral_analy <- spectrum(drake_data$Trend, plot = FALSE)

plot(spectral_analy$freq,
     spectral_analy$spec,
     type = "l",
     col = "black",
     main = "Drake Spectral Analysis",
     xlab = "Frequency",
     ylab = "Spectrum")

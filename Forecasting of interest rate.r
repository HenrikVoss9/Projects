# Load required libraries
library("dplyr")
library("forecast")
library("tseries")
library("lmtest")
library("sandwich")
library("readxl")
library("urca")
library("car")

########
# Data #
########
#Load data
unem_data <- read.csv("/Users/janickrommers/Desktop/Kausal inferens/Kausal projekt/UNRATE.csv") # https://fred.stlouisfed.org/series/UNRATE
effective_rate <- read.csv("/Users/janickrommers/Desktop/Kausal inferens/Kausal projekt/FEDFUNDS.csv") #https://fred.stlouisfed.org/series/FEDFUNDS?fbclid=IwAR0PlIvSfL2r8D6v1a9TZ6kcO7cZq2N-96VbJ26qZK0uU348epQnJzKpNKU
fred_inflation <- read_xlsx("/Users/janickrommers/Desktop/Kausal inferens/Kausal projekt/Inflation1.xlsx") # https://ycharts.com/indicators/us_core_inflation_rate
durable_goods <- read.csv("/Users/janickrommers/Desktop/Kausal inferens/Kausal projekt/Personal durable goods.csv") # https://fred.stlouisfed.org/series/PCEDG 
leading_indicators <- read.csv("/Users/janickrommers/Desktop/Kausal inferens/Kausal projekt/True US Leading Indicators.csv") #https://data.oecd.org/leadind/composite-leading-indicator-cli.htm 

#Differencing the effective rate
effective_rate$diff_eff_rate <- c(0, diff(effective_rate$FEDFUNDS))

#Making sure the dates match
data <- effective_rate[55:832, ]
data$inflation <- as.numeric(fred_inflation$inflation)
unem_data1 <- unem_data[133:910, ]
data$unem <- unem_data1$UNRATE
leading_indicators1 <- leading_indicators[49:826, ]
data$leading_indicators <- leading_indicators1$Value
data$durable_goods <- durable_goods$PCEDG
colnames(data)[colnames(data) == "FEDFUNDS"] <- "effective_rate"

#######################
# Evaluating the data #
#######################
summary(lm(data$effective_rate ~ data$inflation + data$durable_goods + data$unem + data$leading_indicators))

#Checking data for multicollinearity by correlation matrix
selected_data <- data[, c("inflation", "durable_goods", "leading_indicators", "unem")]
cor(selected_data) # Does not suggest multicollinearity between independent variables

#adf test to test for unit root and stationarity
acf(data$effective_rate) #I(1), because p_1 > 0.9/0.8
adf.test(data$effective_rate) #Dickey fuller confirms non-stationarity
acf(data$diff_eff_rate) #I(0), because p_1 = 0.4 --> acf indicates it is stationary
adf.test(data$diff_eff_rate) #Dickey fuller confirms stationarity

acf(data$inflation) #I(1), because p_1 > 0.9/0.8
acf(diff(data$inflation)) #I(0), because p_1 < 0.9/0.8
adf.test(diff(data$inflation)) 
data$diff_inflation <- c(NA, diff(data$inflation))

acf(data$unem) #I(1), because p_1 > 0.9/0.8
adf.test(data$unem)
acf(diff(data$unem)) # p_1 insignificant
adf.test(diff(data$unem))
data$diff_unem <- c(NA, diff(data$unem))

acf(data$leading_indicators) #I(1), because p_1 > 0.9/0.8
acf(diff(data$leading_indicators)) #I(0), because p_1 < 0.9/0.8
adf.test(diff(data$leading_indicators))
data$diff_leading_indicators <- c(NA, diff(data$leading_indicators))

acf(data$durable_goods) #I(1), da p_1 > 0.9/0.8
acf(diff(data$durable_goods))
adf.test(diff(data$durable_goods))
data$diff_durable_goods <- c(NA, diff(data$durable_goods))

#Test with diff:
summary(lm(data$diff_eff_rate ~ data$diff_inflation + data$diff_durable_goods + data$diff_unem + data$diff_leading_indicators)) #diff_durable_goods and diff_inflation are insignificant

# Checking diff data for multicollinearity by correlation matrix
selected_data <- data[, c("diff_inflation", "diff_leading_indicators", "diff_unem", "diff_durable_goods")]
selected_data <- na.omit(selected_data)
cor(selected_data)
# Correlation between the variables gets smaller after diff except between leading indicators and unemployment (from -0.31841527 to -0.66713159) 
# still may not indicate problematic multicollinearity (threshold at 0.8)

###############################################
# plot.ts to check for trends and seasonality #
###############################################
data$t <- seq(1, nrow(data))
plot.ts(data$diff_eff_rate)
plot.ts(data$diff_inflation)
plot.ts(data$diff_durable_goods) #Looks like there could be a small trend
plot.ts(data$diff_unem)
plot.ts(data$diff_leading_indicators)

plot.ts(data$effective_rate) #No trend or seasonality as the spikes looks like economic crises
plot.ts(data$inflation) #No trend or seasonality as the spikes looks like economic crises
plot.ts(data$durable_goods) #Upward trend
plot.ts(data$unem) #No trend or seasonality as the spikes looks like economic crises
plot.ts(data$leading_indicators) #No trend, but maybe seasonality

#Check for trends by regressing on time variable for differenced data
time_model_diff_eff_rate <- lm(data$diff_eff_rate ~ data$t)
summary(time_model_diff_eff_rate) #No significant trend

time_model_inflation <- lm(data$diff_inflation ~ data$t)
summary(time_model_inflation) #No significant trend

time_model_durable_goods <- lm(data$diff_durable_goods ~ data$t)
summary(time_model_durable_goods) #Significant trend

time_model_unemployement <- lm(data$diff_unem ~ data$t)
summary(time_model_unemployement) #No significant trend

time_model_leading_indicators <- lm(data$diff_leading_indicators ~ data$t)
summary(time_model_leading_indicators) #No significant trend

##Check for trends by regressing on time variable for non-differenced data
time_model_effective_rate <- lm(data$effective_rate ~ data$t)
summary(time_model_effective_rate) # Significant trend

time_model_inflation <- lm(data$inflation ~ data$t)
summary(time_model_inflation) # Significant trend

time_model_durable_goods <- lm(data$durable_goods ~ data$t)
summary(time_model_durable_goods) #Significant trend

time_model_unemployement <- lm(data$unem ~ data$t)
summary(time_model_unemployement) #No significant trend

time_model_leading_indicators <- lm(data$leading_indicators ~ data$t)
summary(time_model_leading_indicators) # Significant trend

# Plot of each month every year for seasonality on differenced data
eff_rate_ts = ts(data = data$diff_eff_rate, start = c(1959, 1), frequency = 12)
ggseasonplot(eff_rate_ts)

diff_inflation_ts = ts(data = data$diff_inflation, start = c(1959, 1), frequency = 12)
ggseasonplot(diff_inflation_ts)

unem_ts = ts(data = data$unem, start = c(1959, 1), frequency = 12)
ggseasonplot(unem_ts)

diff_leading_indicators_ts = ts(data = data$diff_leading_indicators, start = c(1959, 1), frequency = 12)
ggseasonplot(diff_leading_indicators_ts)

diff_durable_goods_ts = ts(data = data$diff_durable_goods, start = c(1959, 1), frequency = 12)
ggseasonplot(diff_durable_goods_ts)

# Plot of each month every year for seasonality on non-differenced data
eff_rate_ts = ts(data = data$effective_rate, start = c(1959, 1), frequency = 12)
ggseasonplot(eff_rate_ts)

inflation_ts = ts(data = data$inflation, start = c(1959, 1), frequency = 12)
ggseasonplot(inflation_ts)

unem_ts = ts(data = data$unem, start = c(1959, 1), frequency = 12)
ggseasonplot(unem_ts)

leading_indicators_ts = ts(data = data$leading_indicators, start = c(1959, 1), frequency = 12)
ggseasonplot(leading_indicators_ts)

durable_goods_ts = ts(data = data$durable_goods, start = c(1959, 1), frequency = 12)
ggseasonplot(durable_goods_ts)

##########
# Models #
##########
#Defining the lags
for (i in 1:24) {
  lag_column_name <- paste("effective_rate_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$effective_rate, i)
}
for (i in 1:21) {
  lag_column_name <- paste("diff_eff_rate_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$diff_eff_rate, i)
}

for (i in 1:24) {
  lag_column_name <- paste("inflation_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$inflation, i)
}
for (i in 1:12) {
  lag_column_name <- paste("diff_inflation_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$diff_inflation, i)
}

for (i in 1:24) {
  lag_column_name <- paste("unem_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$unem, i)
}
for (i in 1:12) {
  lag_column_name <- paste("diff_unem_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$diff_unem, i)
}

for (i in 1:24) {
  lag_column_name <- paste("leading_indicators_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$leading_indicators, i)
}
for (i in 1:12) {
  lag_column_name <- paste("diff_leading_indicators_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$diff_leading_indicators, i)
}

for (i in 1:24) {
  lag_column_name <- paste("durable_goods_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$durable_goods, i)
}
for (i in 1:12) {
  lag_column_name <- paste("diff_durable_goods_lag", i, sep = "")
  data[[lag_column_name]] <- dplyr::lag(data$diff_durable_goods, i)
}

#Choosing period for estimation of model and period for forecasting 
dataTrain = data[19:766, ]
dataTest = data[767:778, ]

dataTrain2 = data[19:754, ]
dataTest2 = data[755:778, ]
############################
# 12 month forecast models #
############################
# AR(1)
acf(data$diff_eff_rate)
pacf(data$diff_eff_rate)
AR1 = lm(data=dataTrain, diff_eff_rate ~ diff_eff_rate_lag1)
summary(AR1)

# Finding optimal number of lags for AR(1)+X(p)
acf(dataTrain$diff_inflation) #Max 10 lags
acf(dataTrain$diff_unem) 
acf(dataTrain$diff_leading_indicators) #Max 24 lags (bit too many)
acf(dataTrain$diff_durable_goods) #Max 12 lags
max_lags <- 12
bic_values <- array(Inf, dim = c(max_lags, max_lags, max_lags, max_lags))
optimal_lags <- c(1, 1, 1, 1)

for (i in 1:max_lags) {
  for (j in 1:max_lags) {
    for (k in 1:max_lags) {
      for (l in 1:max_lags) { 
        lag_terms_diff_leading_indicators <- paste0("diff_leading_indicators_lag", 1:i, collapse = " + ")
        lag_terms_inflation <- paste0("diff_inflation_lag", 1:j, collapse = " + ")
        lag_terms_unem <- paste0("diff_unem_lag", 1:k, collapse = " + ")
        lag_terms_durable_goods <- paste0("diff_durable_goods_lag", 1:l, collapse = " + ")
        
        formula_str <- paste("diff_eff_rate ~ diff_eff_rate_lag1 + ", lag_terms_diff_leading_indicators, " + ", lag_terms_inflation, " + ", lag_terms_unem, " + ", lag_terms_durable_goods)
        formula <- as.formula(formula_str)
        model <- lm(formula, data = dataTrain)
        bic_values[i, j, k, l] <- BIC(model)
        
        if (bic_values[i, j, k, l] < bic_values[optimal_lags[1], optimal_lags[2], optimal_lags[3], optimal_lags[4]]) {
          optimal_lags <- c(i, j, k, l)
        }
      }
    }
  }
}
print(optimal_lags)
# According to BIC the optimal number of lags of leading indicators is 2, for inflation 6, unemployment 2 and 1 durable goods

# AR(1) + X(2,6,2,1)
AR1X = lm(data=dataTrain, diff_eff_rate ~ diff_eff_rate_lag1 + diff_leading_indicators_lag1 + diff_leading_indicators_lag2
                                                            + diff_inflation_lag1 + diff_inflation_lag2 + diff_inflation_lag3 + diff_inflation_lag4 + diff_inflation_lag5 + diff_inflation_lag6   
                                                            + diff_unem_lag1 + diff_unem_lag2
                                                            + diff_durable_goods_lag1
                                                            + t)
summary(AR1X)

# AR(p)
# Finding optimal p for AR(p) using BIC 
acf(dataTrain$diff_eff_rate)
bic_values <- numeric()
max_lags <- 21

for (p in 1:max_lags) {
  lag_terms <- paste0("diff_eff_rate_lag", 1:p, collapse = " + ")
  formula <- as.formula(paste("diff_eff_rate ~", lag_terms))
  model <- lm(formula, data = dataTrain)
  bic_values[p] <- BIC(model)
}
optimal_lags <- which.min(bic_values)
print(optimal_lags)
# According to BIC the optimal number of lags is 13

# AR(13)
AR13 = lm(data=dataTrain, diff_eff_rate ~ diff_eff_rate_lag1 + diff_eff_rate_lag2 + diff_eff_rate_lag3 + diff_eff_rate_lag4 +diff_eff_rate_lag5 +diff_eff_rate_lag6 +diff_eff_rate_lag7 + diff_eff_rate_lag8 + diff_eff_rate_lag9 + diff_eff_rate_lag10 + diff_eff_rate_lag11 + diff_eff_rate_lag12 + diff_eff_rate_lag13)
summary(AR13)

# Finding optimal number of lags for AR(13)+X(p)
max_lags <- 12
bic_values <- array(Inf, dim = c(max_lags, max_lags, max_lags, max_lags))
optimal_lags <- c(1, 1, 1, 1)

for (i in 1:max_lags) {
  for (j in 1:max_lags) {
    for (k in 1:max_lags) {
      for (l in 1:max_lags) { 
        lag_terms_diff_leading_indicators <- paste0("diff_leading_indicators_lag", 1:i, collapse = " + ")
        lag_terms_inflation <- paste0("diff_inflation_lag", 1:j, collapse = " + ")
        lag_terms_unem <- paste0("diff_unem_lag", 1:k, collapse = " + ")
        lag_terms_durable_goods <- paste0("diff_durable_goods_lag", 1:l, collapse = " + ")
        
        formula_str <- paste("diff_eff_rate ~ diff_eff_rate_lag1 + diff_eff_rate_lag2 + diff_eff_rate_lag3 + diff_eff_rate_lag4 +diff_eff_rate_lag5 +diff_eff_rate_lag6 +diff_eff_rate_lag7 + diff_eff_rate_lag8 + diff_eff_rate_lag9 + diff_eff_rate_lag10 + diff_eff_rate_lag11 + diff_eff_rate_lag12 + diff_eff_rate_lag13 + ", lag_terms_diff_leading_indicators, " + ", lag_terms_inflation, " + ", lag_terms_unem, " + ", lag_terms_durable_goods)
        formula <- as.formula(formula_str)
        model <- lm(formula, data = dataTrain)
        bic_values[i, j, k, l] <- BIC(model)
        
        if (bic_values[i, j, k, l] < bic_values[optimal_lags[1], optimal_lags[2], optimal_lags[3], optimal_lags[4]]) {
          optimal_lags <- c(i, j, k, l)
        }
      }
    }
  }
}
print(optimal_lags)
# According to BIC the optimal number of lags of leading indicators is 2, for inflation 3, unemployment 2 and 1 durable goods

# AR(13) + X(2,3,2,1)
AR13X = lm(data=dataTrain, diff_eff_rate ~ diff_eff_rate_lag1 + diff_eff_rate_lag2 + diff_eff_rate_lag3 + diff_eff_rate_lag4 +diff_eff_rate_lag5 +diff_eff_rate_lag6 +diff_eff_rate_lag7 + diff_eff_rate_lag8 + diff_eff_rate_lag9 + diff_eff_rate_lag10 + diff_eff_rate_lag11 + diff_eff_rate_lag12 + diff_eff_rate_lag13
                              + diff_leading_indicators_lag1 + diff_leading_indicators_lag2 
                              + diff_inflation_lag1 + diff_inflation_lag2 + diff_inflation_lag3
                              + diff_unem_lag1 + diff_unem_lag2
                              + diff_durable_goods_lag1
                              + t)
summary(AR13X)

# Auto ARIMA model
auto_model <- auto.arima(dataTrain$effective_rate)
summary(auto_model)

# Random Walk with differenced data
variance <- var(diff(dataTrain$diff_eff_rate))
n_simulations <- 100
random_walks <- matrix(nrow = 12, ncol = n_simulations)
rmse_values1 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  random_steps <- rnorm(12, mean = 0, sd = sqrt(variance))
  random_walk <- cumsum(c(dataTrain$diff_eff_rate[748], random_steps))
  random_walks[, i] <- random_walk[-1]
  rmse_values1[i] <- sqrt(mean((dataTest$diff_eff_rate - random_walks[, i])^2))
}
lowest_rmse_index <- which.min(rmse_values1)

plot.ts(random_walks[, lowest_rmse_index], col = "blue", type = "l")
lines(dataTest$diff_eff_rate, col = "red", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Random Walk"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")
print(min(rmse_values1))

# Random Walk with non-differenced data
variance <- var(dataTrain$effective_rate)
n_simulations <- 100
random_walks <- matrix(nrow = 12, ncol = n_simulations)
rmse_values2 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  random_steps <- rnorm(12, mean = 0, sd = sqrt(variance))
  random_walk <- c(dataTrain$effective_rate[748], cumsum(random_steps))
  random_walks[, i] <- random_walk[-1]
  rmse_values2[i] <- sqrt(mean((dataTest$effective_rate - random_walks[, i])^2))
}
lowest_rmse_index <- which.min(rmse_values2)
combined_range <- range(c(dataTest$effective_rate, random_walks[, lowest_rmse_index]), na.rm = TRUE)
plot.ts(random_walks[, lowest_rmse_index], col = "blue", ylim = combined_range, type = "l")
lines(dataTest$effective_rate, col = "red")
legend("topright", inset=c(0, 0), legend = c("Actual", "Random Walk"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")
print(min(rmse_values2))

############################
# 24 month forecast models #
############################
# AR(1)
AR1_ = lm(data=dataTrain2, diff_eff_rate ~ diff_eff_rate_lag1)
summary(AR1_)

# AR(1) + X(2,6,2,1)
AR1X_ = lm(data=dataTrain2, diff_eff_rate ~ diff_eff_rate_lag1 + diff_leading_indicators_lag1 + diff_leading_indicators_lag2
          + diff_inflation_lag1 + diff_inflation_lag2 + diff_inflation_lag3 + diff_inflation_lag4 + diff_inflation_lag5 + diff_inflation_lag6   
          + diff_unem_lag1 + diff_unem_lag2
          + diff_durable_goods_lag1
          + t)
summary(AR1X_)

# AR(13)
AR13_ = lm(data=dataTrain2, diff_eff_rate ~ diff_eff_rate_lag1 + diff_eff_rate_lag2 + diff_eff_rate_lag3 + diff_eff_rate_lag4 +diff_eff_rate_lag5 +diff_eff_rate_lag6 +diff_eff_rate_lag7 + diff_eff_rate_lag8 + diff_eff_rate_lag9 + diff_eff_rate_lag10 + diff_eff_rate_lag11 + diff_eff_rate_lag12 + diff_eff_rate_lag13)
summary(AR13_)

# AR(13) + X(2,3,2,1)
AR13X_ = lm(data=dataTrain2, diff_eff_rate ~ diff_eff_rate_lag1 + diff_eff_rate_lag2 + diff_eff_rate_lag3 + diff_eff_rate_lag4 +diff_eff_rate_lag5 +diff_eff_rate_lag6 +diff_eff_rate_lag7 + diff_eff_rate_lag8 + diff_eff_rate_lag9 + diff_eff_rate_lag10 + diff_eff_rate_lag11 + diff_eff_rate_lag12 + diff_eff_rate_lag13
           + diff_leading_indicators_lag1 + diff_leading_indicators_lag2 
           + diff_inflation_lag1 + diff_inflation_lag2 + diff_inflation_lag3
           + diff_unem_lag1 + diff_unem_lag2
           + diff_durable_goods_lag1
           + t)
summary(AR13X_)

# Auto ARIMA model
auto_model_ <- auto.arima(dataTrain2$effective_rate)
print(auto_model_) #ARIMA(2,1,1)
summary(auto_model_)

# Random Walk with differenced data
variance <- var(diff(dataTrain2$diff_eff_rate))
n_simulations <- 100
random_walks <- matrix(nrow = 24, ncol = n_simulations)
rmse_values3 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  random_steps <- rnorm(24, mean = 0, sd = sqrt(variance))
  random_walk <- c(dataTrain2$diff_eff_rate[736], cumsum(random_steps))
  random_walks[, i] <- random_walk[-1]
  rmse_values3[i] <- sqrt(mean((dataTest2$diff_eff_rate - random_walks[, i])^2))
}
lowest_rmse_index <- which.min(rmse_values3)

plot.ts(random_walks[, lowest_rmse_index], col = "blue", type = "l")
lines(dataTest2$diff_eff_rate, col = "red", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Random Walk"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")
print(min(rmse_values3))

# Random Walk with non-differenced data
variance <- var(dataTrain2$effective_rate)
n_simulations <- 100
random_walks <- matrix(nrow = 24, ncol = n_simulations)
rmse_values4 <- numeric(n_simulations)

for (i in 1:n_simulations) {
  random_steps <- rnorm(24, mean = 0, sd = sqrt(variance))
  random_walk <- c(dataTrain2$diff_eff_rate[736], cumsum(random_steps))
  random_walks[, i] <- random_walk[-1]
  rmse_values4[i] <- sqrt(mean((dataTest2$effective_rate - random_walks[, i])^2))
}
lowest_rmse_index <- which.min(rmse_values4)
combined_range <- range(c(dataTest2$effective_rate, random_walks[, lowest_rmse_index]), na.rm = TRUE)
plot.ts(random_walks[, lowest_rmse_index], col = "blue", ylim = combined_range, type = "l")
lines(dataTest2$effective_rate, col = "red")
legend("topright", inset=c(0, 0), legend = c("Actual", "Random Walk"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")
print(min(rmse_values4))

##################
# Co-integration #
##################
# Johansen Test for cointegration
data1 = data[c("effective_rate","inflation","unem","leading_indicators","durable_goods")]
summary(ca.jo(data1, type = "trace", K = 4, ecdet = "const")) #r=3

# Finding optimal p for AR(p) using BIC
bic_values <- numeric()
max_lags <- 24

for (p in 1:max_lags) {
  lag_terms <- paste0("effective_rate_lag", 1:p, collapse = " + ")
  formula <- as.formula(paste("effective_rate ~", lag_terms))
  model <- lm(formula, data = dataTrain)
  bic_values[p] <- BIC(model)
}
optimal_lags <- which.min(bic_values)
print(optimal_lags)
# According to BIC the optimal number of lags is 14 

# Finding optimal number of lags to do Granger test on
max_lags <- 24
bic_values <- array(Inf, dim = c(max_lags, max_lags, max_lags, max_lags))
optimal_lags <- c(1, 1, 1, 1)

for (i in 1:max_lags) {
  for (j in 1:max_lags) {
    for (k in 1:max_lags) {
      for (l in 1:max_lags) {  
        lag_terms_leading_indicators <- paste0("leading_indicators_lag", 1:i, collapse = " + ")
        lag_terms_inflation <- paste0("inflation_lag", 1:j, collapse = " + ")
        lag_terms_unem <- paste0("unem_lag", 1:k, collapse = " + ")
        lag_terms_durable_goods <- paste0("durable_goods_lag", 1:l, collapse = " + ")  # New lag terms for the fourth variable
        
        formula_str <- paste("effective_rate ~ effective_rate_lag1 + effective_rate_lag2 + effective_rate_lag3 + effective_rate_lag4 + effective_rate_lag5 + effective_rate_lag6 + effective_rate_lag7 + effective_rate_lag8 + effective_rate_lag9 + effective_rate_lag10 + effective_rate_lag11 + effective_rate_lag12 + effective_rate_lag13 + effective_rate_lag14 + ", lag_terms_leading_indicators, " + ", lag_terms_inflation, " + ", lag_terms_unem, " + ", lag_terms_durable_goods)
        formula <- as.formula(formula_str)
        model <- lm(formula, data = dataTrain)
        bic_values[i, j, k, l] <- BIC(model)
        
        if (bic_values[i, j, k, l] < bic_values[optimal_lags[1], optimal_lags[2], optimal_lags[3], optimal_lags[4]]) {
          optimal_lags <- c(i, j, k, l)
        }
      }
    }
  }
}
print(optimal_lags)
# According to BIC the number of lags of leading indicators is 1, for inflation 4, durable goods 1 and unemployment 1

# Granger causality test to see which variable to exclude
grangertest(data$effective_rate ~ data$leading_indicators, order = 1) # Significant
grangertest(data$effective_rate ~ data$inflation, order = 4) # Significant
grangertest(data$effective_rate ~ data$durable_goods, order = 1) # Not significant
grangertest(data$effective_rate ~ data$unem, order = 1) # Significant
# We decide to exclude durable goods

# Finding optimal number of lags for AR(14)+X(p) model
max_lags <- 24
bic_values <- array(Inf, dim = c(max_lags, max_lags, max_lags))
optimal_lags <- c(1, 1, 1)

for (i in 1:max_lags) {
  for (j in 1:max_lags) {
    for (k in 1:max_lags) {  
      lag_terms_leading_indicators <- paste0("leading_indicators_lag", 1:i, collapse = " + ")
      lag_terms_inflation <- paste0("inflation_lag", 1:j, collapse = " + ")
      lag_terms_unem <- paste0("unem_lag", 1:k, collapse = " + ")
      
      formula_str <- paste("effective_rate ~ effective_rate_lag1 + effective_rate_lag2 + effective_rate_lag3 + effective_rate_lag4 + effective_rate_lag5 + effective_rate_lag6 + effective_rate_lag7 + effective_rate_lag8 + effective_rate_lag9 + effective_rate_lag10 + effective_rate_lag11 + effective_rate_lag12 + effective_rate_lag13 + effective_rate_lag14 + ", lag_terms_leading_indicators, " + ", lag_terms_inflation, " + ", lag_terms_unem)
      formula <- as.formula(formula_str)
      model <- lm(formula, data = dataTrain)
      bic_values[i, j, k] <- BIC(model)
      
      if (bic_values[i, j, k] < bic_values[optimal_lags[1], optimal_lags[2], optimal_lags[3]]) {
        optimal_lags <- c(i, j, k)
      }
    }
  }
}
print(optimal_lags)
# According to BIC the optimal number of lags of leading indicators is 1, for inflation 4, and unemployment 1

#We define two co-integration models with trends
# AR(14)+X(1,4,1) for 12 months
AR14X_coint = lm(data=dataTrain, effective_rate ~ effective_rate_lag1 + effective_rate_lag2 + effective_rate_lag3 + effective_rate_lag4 + effective_rate_lag5 + effective_rate_lag6 + effective_rate_lag7 + effective_rate_lag8 + effective_rate_lag9 + effective_rate_lag10 + effective_rate_lag11 + effective_rate_lag12 + effective_rate_lag13 + effective_rate_lag14  
               + leading_indicators_lag1 
               + inflation_lag1 + inflation_lag2 + inflation_lag3 + inflation_lag4
               + unem_lag1
               + t)
summary(AR14X_coint)

# AR(14)+X(1,4,1) for 24 months
AR14X_coint_ = lm(data=dataTrain2, effective_rate ~ effective_rate_lag1 + effective_rate_lag2 + effective_rate_lag3 + effective_rate_lag4 + effective_rate_lag5 + effective_rate_lag6 + effective_rate_lag7 + effective_rate_lag8 + effective_rate_lag9 + effective_rate_lag10 + effective_rate_lag11 + effective_rate_lag12 + effective_rate_lag13 + effective_rate_lag14  
                 + leading_indicators_lag1 
                 + inflation_lag1 + inflation_lag2 + inflation_lag3 + inflation_lag4
                 + unem_lag1
                 + t)
summary(AR14X_coint_)

#############
# Forecasts #
#############
###################################
# Out of sample 12 month forecast #
###################################
# For AR1
combined_range_ar1 <- range(dataTest$diff_eff_rate, forecast(AR1, h = 12, newdata = dataTest)$mean, na.rm = TRUE)
plot(dataTest$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar1)
lines(forecast(AR1, h = 12, newdata = dataTest)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR1"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR1X
combined_range_ar1x <- range(dataTest$diff_eff_rate, forecast(AR1X, h = 12, newdata = dataTest)$mean, na.rm = TRUE)
plot(dataTest$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar1x)
lines(forecast(AR1X, h = 12, newdata = dataTest)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR1X"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR13
combined_range_ar13 <- range(dataTest$diff_eff_rate, forecast(AR13, h = 12, newdata = dataTest)$mean, na.rm = TRUE)
plot(dataTest$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar13)
lines(forecast(AR13, h = 12, newdata = dataTest)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR13"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR13X
combined_range_ar13X <- range(dataTest$diff_eff_rate, forecast(AR13X, h = 12, newdata = dataTest)$mean, na.rm = TRUE)
plot(dataTest$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar13X)
lines(forecast(AR13X, h = 12, newdata = dataTest)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR13X"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# ARIMA(0,1,1)
combined_range <- range(dataTest$effective_rate, forecast(auto_model, h = 12)$mean, na.rm = TRUE)
plot(dataTest$effective_rate, col = "red", type = "l", ylim = combined_range, xlab = "Time", ylab = "effective_rate")
lines(as.numeric(forecast(auto_model, h = 12)$mean), col = "blue", type = "l", ylim = combined_range, xlab = "Time", ylab = "diff_eff_rate", main = "Actual vs Forecast")
legend("right", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR14X_coint
combined_range <- range(dataTest$effective_rate, forecast(AR14X_coint, h = 12, newdata = dataTest)$mean, na.rm = TRUE)
plot(dataTest$effective_rate, col = "red", type = "l", ylim = combined_range, xlab = "Time", ylab = "effective_rate")
lines(forecast(AR14X_coint, h = 12, newdata = dataTest)$mean, col = "blue", type = "l")
legend("right", inset=c(0, 0), legend = c("Actual", "Forecast AR14X_coint"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

###################################
# Out of sample 24 month forecast #
###################################
# For AR1_
combined_range_ar1_ <- range(dataTest2$diff_eff_rate, forecast(AR1_, h = 24, newdata = dataTest2)$mean, na.rm = TRUE)
plot(dataTest2$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar1_)
lines(forecast(AR1_, h = 24, newdata = dataTest2)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR1_"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR1X_
combined_range_ar1x_ <- range(dataTest2$diff_eff_rate, forecast(AR1X_, h = 24, newdata = dataTest2)$mean, na.rm = TRUE)
plot(dataTest2$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar1x_)
lines(forecast(AR1X_, h = 24, newdata = dataTest2)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR1X_"), col = c("red", "blue"), lty = 1, cex = 0.7, bty = "n")

# For AR13_
combined_range_ar13_ <- range(dataTest2$diff_eff_rate, forecast(AR13_, h = 12, newdata = dataTest2)$mean, na.rm = TRUE)
plot(dataTest2$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar13_)
lines(forecast(AR13_, h = 12, newdata = dataTest2)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR13_"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR13X_
combined_range_ar13X_ <- range(dataTest2$diff_eff_rate, forecast(AR13X_, h = 12, newdata = dataTest2)$mean, na.rm = TRUE)
plot(dataTest2$diff_eff_rate, col = "red", type = "l", ylim = combined_range_ar13X_)
lines(forecast(AR13X_, h = 12, newdata = dataTest2)$mean, col = "blue", type = "l")
legend("topright", inset=c(0, 0), legend = c("Actual", "Forecast AR13X_"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# ARIMA(2,1,1) - Flat forecast, because of low interest rates in the end of the training set
combined_range <- range(dataTest2$effective_rate, forecast(auto_model_, h = 24)$mean, na.rm = TRUE)
plot(dataTest2$effective_rate, col = "red", type = "l", ylim = combined_range, xlab = "Time", ylab = "effective_rate")
lines(as.numeric(forecast(auto_model_, h = 24)$mean), col = "blue", type = "l", ylim = combined_range, xlab = "Time", ylab = "effective_rate", main = "Actual vs Forecast")
legend("topright", inset=c(0, 0),  legend = c("Actual", "Forecast ARIMA"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")

# For AR14X_coint_
combined_range <- range(dataTest2$effective_rate, forecast(AR14X_coint_, h = 24, newdata = dataTest2)$mean, na.rm = TRUE)
plot(dataTest2$effective_rate, col = "red", type = "l", ylim = combined_range, xlab = "Time", ylab = "effective_rate")
lines(forecast(AR14X_coint_, h = 24, newdata = dataTest2)$mean, col = "blue", type = "l")
legend("right", inset=c(0, 0), legend = c("Actual", "Forecast AR14X_coint_"), col = c("red", "blue"), lty = 1, cex = 0.8, bty = "n")


###########################
# Evaluating the forecast #    
###########################
###################################
# Out of sample 12 month forecast #
###################################
residuals_AR1 <- residuals(forecast(AR1, h = 12, newdata = dataTest))
RMSE_AR1 <- sqrt(mean(residuals_AR1^2))
RMSE_AR1 # 0.4667068

residuals_AR1X <- residuals(forecast(AR1X, h = 12, newdata = dataTest))
RMSE_AR1X <- sqrt(mean(residuals_AR1X^2))
RMSE_AR1X # 0.4469902

residuals_AR13 <- residuals(forecast(AR13, h = 12, newdata = dataTest))
RMSE_AR13 <- sqrt(mean(residuals_AR13^2))
RMSE_AR13 # 0.4330909

residuals_AR13X <- residuals(forecast(AR13X, h = 12, newdata = dataTest))
RMSE_AR13X <- sqrt(mean(residuals_AR13X^2))
RMSE_AR13X # 0.4197813

print(min(rmse_values1)) #Random walk with RMSE of 0.4

# According to RMSE the AR13X model does the best 12 month forecast of diff_eff_rate
# But out of 100 random walks there at least one of them will beat the AR13X model


residuals_AR14X_coint <- residuals(forecast(AR14X_coint, h = 12, newdata = dataTest))
RMSE_AR14X_coint <- sqrt(mean(residuals_AR14X_coint^2))
RMSE_AR14X_coint # 0.4108601

print(min(rmse_values2)) #Random walk with RMSE of 2.3
# According to RMSE the AR14X_coint model does the best 12 month forecast of effective_rate


###################################
# Out of sample 24 month forecast #
###################################
residuals_AR1_ <- residuals(forecast(AR1_, h = 24, newdata = dataTest2))
sqrt(mean(residuals_AR1_^2)) # 0.4694202 

residuals_AR1X_ <- residuals(forecast(AR1X_, h = 24, newdata = dataTest2))
sqrt(mean(residuals_AR1X_^2)) # 0.4490956

residuals_AR13_ <- residuals(forecast(AR13_, h = 24, newdata = dataTest2))
sqrt(mean(residuals_AR13_^2)) # 0.4351859

residuals_AR13X_ <- residuals(forecast(AR13X_, h = 24, newdata = dataTest2))
sqrt(mean(residuals_AR13X_^2)) # 0.4214133

print(min(rmse_values3)) #Random walk with RMSE of 0.48

# According to RMSE the AR13X model does the best 24 month forecast of diff_eff_rate


residuals_AR14X_coint_ <- residuals(forecast(AR14X_coint_, h = 24, newdata = dataTest2))
sqrt(mean(residuals_AR14X_coint_^2)) # 0.4130163

print(min(rmse_values4)) #Random walk with RMSE of 3.3
# According to RMSE the AR14X_coint model does the best 24 month forecast of effective_rate


###################
# Tests of models #
###################
#Breusch-Godfrey test - Serial correlation
bgtest(AR1, order = 1) # p-value = 6.108e-06
dwtest(AR1) # p-value = 0.03579 --> suggest serial correlation as well
bgtest(AR1X, order = 1) # p-value = 0.007434
bgtest(AR13, order = 13) # p-value = 0.003394
bgtest(AR13X, order = 13) # p-value = 0.01413
bgtest(AR14X_coint, order = 14) # p-value = 0.06355
#Conclusion is suggestion of serial/auto correlation in all models except for AR14X_coint

#Breusch-Pagan test only for AR14X_coint model, because of serial correlation in the other models
bptest(AR14X_coint) #Suggest heteroschedasticity
ncvTest(AR14X_coint) #Suggest heteroschedasticity

#########################################
# Residual plot for heteroschedasticity #
#########################################
plot(dataTrain$t, residuals(AR1), xlab = "Time", ylab = "Residuals", main = "Residuals AR(1)")
abline(h = 0, col = "red")

plot(dataTrain$t, residuals(AR1X), xlab = "Time", ylab = "Residuals", main = "Residuals AR(1) + X")
abline(h = 0, col = "red")

plot(dataTrain$t, residuals(AR13), xlab = "Time", ylab = "Residuals", main = "Residuals AR(13)")
abline(h = 0, col = "red")

plot(dataTrain$t, residuals(AR13X), xlab = "Time", ylab = "Residuals", main = "Residuals AR(13) + X")
abline(h = 0, col = "red")

plot(dataTrain$t, residuals(AR14X_coint), xlab = "Time", ylab = "Residuals", main = "Residuals AR(14) + X")
abline(h = 0, col = "red")

plot(dataTrain$t, residuals(auto_model), xlab = "Time", ylab = "Residuals", main = "Residuals ARIMA(0,1,1)")
abline(h = 0, col = "red")
# Plots does not show clear evidence of heteroschedasticity, but all plot show variance decreasing with time

#Hac standard errors
coeftest(AR1, vcov = vcovHAC(AR1))
summary(AR1)
coeftest(AR1X, vcov = vcovHAC(AR1X))
summary(AR1X)

coeftest(AR13, vcov = vcovHAC(AR13))
summary(AR13)
coeftest(AR13X, vcov = vcovHAC(AR13X))
summary(AR13X)

coeftest(AR14X_coint, vcov = vcovHAC(AR14X_coint))
summary(AR14X_coint)

# Wald test for significance of independent variables
waldtest(AR1)
waldtest(AR1X)
waldtest(AR13)
waldtest(AR13X)
waldtest(AR14X_coint)
# Low p-values, the independent variables in the model jointly have a statistically significant effect on the dependent variable

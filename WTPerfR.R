setwd("C:\\Projects\\WTPerf\\RProj_WTPerf")

# load the dataset
# source: https://www.kaggle.com/datasets/bhavikjikadara/wind-power-generated-data
data <- read.csv("T1.csv")



# DATA CLEANING

# show summary of the dataset
summary(data)

# change the column names
colnames(data) <- c("DateTime", "Power", "WindSpeed", 
                    "TheoreticalPowerCurve", "WindDirection")

# change DateTime to proper date/time format
data$DateTime <- as.POSIXct(data$DateTime, format = "%d %m %Y %H:%M", 
                            tz = "Europe/Istanbul")

# replace negative Power values with zero
data$Power <- ifelse(data$Power < 0, 0, data$Power)

# check for missing values
colSums(is.na(data))

# view updated summary of the dataset
summary(data)



# DATA EXPLORATION

library(ggplot2)

# plot Power (target variable) histogram
ggplot(data, aes(x = Power)) +
  geom_histogram(binwidth = 100, fill="steelblue2", color="gray35") + 
  ggtitle("Power Distribution")
# observations: high count of zero values

# plot WindSpeed histogram
ggplot(data, aes(x = WindSpeed)) +
  geom_histogram(binwidth = 1, fill="steelblue2", color="gray35") + 
  ggtitle("Wind Speed Distribution")
# observations: distribution similar to Weibull distribution

# plot WindDirection histogram
ggplot(data, aes(x = WindDirection)) +
  geom_histogram(binwidth = 10, fill="steelblue2", color="gray35") + 
  ggtitle("Wind Direction Distribution")
# observations: concentrated at around 60 degrees and 200 degrees

# plot Power vs WindSpeed scatterplot
ggplot(data, aes(x=WindSpeed, y=Power)) +
  geom_point(color="steelblue2") +
  ggtitle("Power vs Wind Speed")
# observations: follows an S-shaped curve, many outliers, many zeros

# plot Power vs WindDirection scatterplot
ggplot(data, aes(x=WindDirection, y=Power)) +
  geom_point(color="steelblue2") +
  ggtitle("Power vs Wind Direction")
# observations: no pattern, just more points in the same values observed in the histogram

# plot Power vs WindSpeed and Power vs TheoreticalPowerCurve scatterplot
ggplot() +
  geom_point(data=data, aes(x=WindSpeed, y=Power, color="Actual"), alpha=0.1) +
  geom_point(data=data, aes(x=WindSpeed, y=TheoreticalPowerCurve, color="Theoretical")) +
  scale_color_manual(values = c("Actual" = "steelblue1", "Theoretical" = "darkorange")) +
  ggtitle("Actual vs Theoretical Power Curves") +
  theme(legend.position = c(0.9, 0.5))
# observations: actual has wide deviation from theoretical
# These outliers are mostly due to lower output during, before, and after turbine stoppages
# There may also be output curtailment due to grid limitations
# To model the actual generating performance of the turbine, outliers must be removed



# OUTLIER REMOVAL

# remove rows with zero Power and/or WindSpeed values
data_noZero <- data[data$Power > 0,]
data_noZero <- data_noZero[data_noZero$WindSpeed > 0,]

# Outliers will be identified as those outside Q1-1.5IQR and Q3-1.5IQR.
# However, power values are expected to range from zero to max power normally.
# Quartiles to be calculated should have a condition, which is the WindSpeed value
# Instead of checking outliers for the entire data, it needs to be split to WindSpeed bins

# setup data for looping through the bins
binwidth <- 0.1
# max WindSpeed value for bins
max_WindSpeed <- 25
# number of bins
bin_count <- max_WindSpeed/binwidth
# minimum number datapoints to do filtering
bin_min_points <- 4
# max Power value based on turbine capacity
max_Power <- max(data_noZero$Power)
# initialize vectors
bounds_WindSpeed <- c()
bounds_Power_low <- c()
bounds_Power_high <- c()

# loop through the bins
for (i in seq(from = binwidth, to = max_WindSpeed, by = binwidth)) {
  # get the boolean vector for the rows of data that are part of the current bin
  data_subset <- data_noZero$WindSpeed > i-binwidth & data_noZero$WindSpeed <= i
  
  if (sum(data_subset) >= bin_min_points) {
    # if enough datapoints, calculate bounds for filtering
    Q1 <- quantile(data_noZero[data_subset, "Power"], 0.25)
    Q3 <- quantile(data_noZero[data_subset, "Power"], 0.75)
    IQR <- Q3 - Q1
    bounds_Power_low <- c(bounds_Power_low, Q1-1.5*IQR)
    bounds_Power_high <- c(bounds_Power_high, Q3+1.5*IQR)
    
  } else if (sum(data_subset) > 0) {
    # if there are datapoints but not enough datapoints, just use all
    bounds_Power_low <- c(bounds_Power_low, min(data_noZero[data_subset, "Power"]))
    bounds_Power_high <- c(bounds_Power_high, max(data_noZero[data_subset, "Power"]))
    
  } else {
    # if no datapoints, skip this bin
    next
  }
  # if there are datapoints, append the bin value as WindSpeed
  bounds_WindSpeed <- c(bounds_WindSpeed, i)
}

# compile the calculated bin bounds in a dataframe
bounds <- data.frame(WindSpeed = bounds_WindSpeed, 
                     Power_low = bounds_Power_low,
                     Power_high = bounds_Power_high)

# define function to match the lower bound from the bounds dataframe to the main dataframe
get_bounds_low <- function(ws) {
  bounds_low <- bounds[which(ws > bounds$WindSpeed - binwidth & 
                               ws <= bounds$WindSpeed), "Power_low"]
  # check if no match
  if (length(bounds_low) == 0) { return(NA) }
  else { return(bounds_low) }
}

# define function to match the upper bound from the bounds dataframe to the main dataframe
get_bounds_high <- function(ws) {
  bounds_high <- bounds[which(ws > bounds$WindSpeed - binwidth & 
                                ws <= bounds$WindSpeed), "Power_high"]
  # check if no match
  if (length(bounds_high) == 0) {
    return(NA)
  } else {
    return(bounds_high)
  }
}

# add columns in the main dataframe containing corresponding the upper and lower bounds
data_noZero$bounds_low <- vapply(data_noZero$WindSpeed, get_bounds_low, numeric(1))
data_noZero$bounds_high <- vapply(data_noZero$WindSpeed, get_bounds_high, numeric(1))

# check if there are null data in the new columns
colSums(is.na(data_noZero))

# remove rows with null data
data_noZero <- data_noZero[!rowSums(is.na(data_noZero)),]

# check updated null count
colSums(is.na(data_noZero))

# Remove outliers. Create a new dataframe with the filtered data.
data_cleaned <- data_noZero[data_noZero$Power >= data_noZero$bounds_low & 
                              data_noZero$Power <= data_noZero$bounds_high, 
                            c("Power", "WindSpeed", "WindDirection")]

# plot original vs cleaned Power vs WindSpeed
ggplot() + 
  geom_point(data = data, aes(x = WindSpeed, y = Power, color = "Outliers")) +
  geom_point(data = data_cleaned, aes(x = WindSpeed, y = Power, color = "Cleaned")) +
  scale_color_manual(values = c("Outliers" = "lightsteelblue2", "Cleaned" = "steelblue2")) +
  ggtitle("Outliers vs Cleaned Dataset") +
  theme(legend.position = c(0.9, 0.5))

# check correlation between Power, WindSpeed, and WindDirection
cor(data_cleaned[,c("Power", "WindSpeed", "WindDirection")])
# WindSpeed has very high correlation with Power, while WindDirection does not
# Still, try models with both to see if there will be improvement in having more variables



# DATA PREPARATION FOR REGRESSION

# identify target and independent variables
y <- c("Power")
x1 <- c("WindSpeed") # "1" = wind speed only
x2 <- c("WindSpeed", "WindDirection") # "2" = both wind speed and direction

# split data into training and test sets
set.seed(42)
library(dplyr)
library(caret)
training_obs <- data_cleaned[,y] %>% createDataPartition(p = 0.8, list = FALSE)
# for "1", use only WindSpeed, and no need for normalization
train1 <- data_cleaned[training_obs, c(y, x1)]
test1 <- data_cleaned[-training_obs, c(y, x1)]

# "2" (both wind speed and direction) needs normalization
process <- preProcess(data_cleaned, method=c("range"))
data_cleaned_norm <- predict(process, data_cleaned)
# add column of original (pre-normalization) Power values for later use
data_cleaned_norm$Power_orig <- data_cleaned$Power
# split
train2 <- data_cleaned_norm[training_obs,]
test2 <- data_cleaned_norm[-training_obs,]

# create formula for regressors
library(wrapr)
fmla1 <- mk_formula(y, x1)
fmla2 <- mk_formula(y, x2)

# define functions for calcuating R-squared and Root Mean Squared Error (RMSE)
rsq <- function(y, f) { 1 - sum((y - f)^2) / sum((y - mean(y))^2) }
rmse <- function(y, f) { sqrt(mean((y - f)^2)) }

# load libraries
library(rpart) # Decision Tree
library(randomForest) # Random Forest
library("gbm") # Generalized Boosted Regression
library("xgboost") # XGBoost
library("lightgbm") # LightGBM



# REGRESSION

# 1. Non-Linear Regression using Sigmoid Function with 1 Variable
# formula and starting values were determined by visual approximation
#   by trial and error using a graphing calculator)
nls_model <- nls(Power ~ max_Power * tanh((a * WindSpeed + b)^3),
                 data = train1, start = c(a=0.1, b=0.1))
train1$pred <- predict(nls_model, newdata = train1)
test1$pred <- predict(nls_model, newdata = test1)
# append results
method <- "Non-Linear Regression (1 var)"
rsq_train <- rsq(train1$Power, train1$pred)
rsq_test <- rsq(test1$Power, test1$pred)
rmse_train <- rmse(train1$Power, train1$pred)
rmse_test <- rmse(test1$Power, test1$pred)
# this model was based on the Power vs WindSpeed scatter
#   so this will not be done with 2 variables

# 2. Decision Tree Regression with 1 Variable
tree_model <- rpart(fmla1, data = train1, method = "anova")
train1$pred <- predict(tree_model, newdata = train1)
test1$pred <- predict(tree_model, newdata = test1)
# append results
method <- c(method, "Decision Tree (1 var)")
rsq_train <- c(rsq_train, rsq(train1$Power, train1$pred))
rsq_test <- c(rsq_test, rsq(test1$Power, test1$pred))
rmse_train <- c(rmse_train, rmse(train1$Power, train1$pred))
rmse_test <- c(rmse_test, rmse(test1$Power, test1$pred))

# 3. Decision Tree Regression with 2 Variables
tree_model <- rpart(fmla2, data = train2, method = "anova")
train2$pred <- predict(tree_model, newdata = train2)
test2$pred <- predict(tree_model, newdata = test2)
# scale predictions back to original magnitude (before normalization)
train2$pred_orig <- train2$pred * max_Power
test2$pred_orig <- test2$pred * max_Power
# append results
method <- c(method, "Decision Tree (2 var)")
rsq_train <- c(rsq_train, rsq(train2$Power_orig, train2$pred_orig))
rsq_test <- c(rsq_test, rsq(test2$Power_orig, test2$pred_orig))
rmse_train <- c(rmse_train, rmse(train2$Power_orig, train2$pred_orig))
rmse_test <- c(rmse_test, rmse(test2$Power_orig, test2$pred_orig))

# 4. Random Forest Regression with 1 Variable
rf_model <- randomForest(fmla1, data = train1, ntree = 100, maxnodes = 100)
train1$pred <- predict(rf_model, newdata = train1)
test1$pred <- predict(rf_model, newdata = test1)
# append results
method <- c(method, "Random Forest (1 var)")
rsq_train <- c(rsq_train, rsq(train1$Power, train1$pred))
rsq_test <- c(rsq_test, rsq(test1$Power, test1$pred))
rmse_train <- c(rmse_train, rmse(train1$Power, train1$pred))
rmse_test <- c(rmse_test, rmse(test1$Power, test1$pred))

# 5. Random Forest Regression with 2 Variables
rf_model <- randomForest(fmla2, data = train2, ntree = 100, maxnodes = 100)
train2$pred <- predict(rf_model, newdata = train2)
test2$pred <- predict(rf_model, newdata = test2)
# scale predictions back to original magnitude (before normalization)
train2$pred_orig <- train2$pred * max_Power
test2$pred_orig <- test2$pred * max_Power
# append results
method <- c(method, "Random Forest (2 var)")
rsq_train <- c(rsq_train, rsq(train2$Power_orig, train2$pred_orig))
rsq_test <- c(rsq_test, rsq(test2$Power_orig, test2$pred_orig))
rmse_train <- c(rmse_train, rmse(train2$Power_orig, train2$pred_orig))
rmse_test <- c(rmse_test, rmse(test2$Power_orig, test2$pred_orig))

# 6. Generalized Boosted Regression with 1 Variable
gbm_model <- gbm(fmla1, data = train1, distribution = "gaussian", n.trees = 100)
train1$pred <- predict(gbm_model, newdata = train1)
test1$pred <- predict(gbm_model, newdata = test1)
# append results
method <- c(method, "Generalized Boosted Reg (1 var)")
rsq_train <- c(rsq_train, rsq(train1$Power, train1$pred))
rsq_test <- c(rsq_test, rsq(test1$Power, test1$pred))
rmse_train <- c(rmse_train, rmse(train1$Power, train1$pred))
rmse_test <- c(rmse_test, rmse(test1$Power, test1$pred))

# 7. Generalized Boosted Regression with 2 Variables
gbm_model <- gbm(fmla2, data = train2, distribution = "gaussian", n.trees = 100)
train2$pred <- predict(gbm_model, newdata = train2)
test2$pred <- predict(gbm_model, newdata = test2)
# scale predictions back to original magnitude (before normalization)
train2$pred_orig <- train2$pred * max_Power
test2$pred_orig <- test2$pred * max_Power
# append results
method <- c(method, "Generalized Boosted Reg (2 var)")
rsq_train <- c(rsq_train, rsq(train2$Power_orig, train2$pred_orig))
rsq_test <- c(rsq_test, rsq(test2$Power_orig, test2$pred_orig))
rmse_train <- c(rmse_train, rmse(train2$Power_orig, train2$pred_orig))
rmse_test <- c(rmse_test, rmse(test2$Power_orig, test2$pred_orig))

# 8. XGBoost with 1 Variable
dtrain <- xgb.DMatrix(data = as.matrix(train1[, x1]), label = train1[, y])
dtest <- xgb.DMatrix(data = as.matrix(test1[, x1]), label = test1[, y])
xgb_model <- xgboost(data = dtrain, objective = "reg:squarederror", 
                     nrounds = 100, max_depth = 3, eta = 0.1, verbose = FALSE)
train1$pred <- predict(xgb_model, newdata = dtrain)
test1$pred <- predict(xgb_model, newdata = dtest)
# append results
method <- c(method, "XGBoost (1 var)")
rsq_train <- c(rsq_train, rsq(train1$Power, train1$pred))
rsq_test <- c(rsq_test, rsq(test1$Power, test1$pred))
rmse_train <- c(rmse_train, rmse(train1$Power, train1$pred))
rmse_test <- c(rmse_test, rmse(test1$Power, test1$pred))

# 9. XGBoost with 2 Variables
dtrain <- xgb.DMatrix(data = as.matrix(train2[, x2]), label = train2[, y])
dtest <- xgb.DMatrix(data = as.matrix(test2[, x2]), label = test2[, y])
xgb_model <- xgboost(data = dtrain, objective = "reg:squarederror", 
                     nrounds = 100, max_depth = 3, eta = 0.1, verbose = FALSE)
train1$pred <- predict(xgb_model, newdata = dtrain)
test1$pred <- predict(xgb_model, newdata = dtest)
# scale predictions back to original magnitude (before normalization)
train2$pred_orig <- train2$pred * max_Power
test2$pred_orig <- test2$pred * max_Power
# append results
method <- c(method, "XGBoost (2 var)")
rsq_train <- c(rsq_train, rsq(train2$Power_orig, train2$pred_orig))
rsq_test <- c(rsq_test, rsq(test2$Power_orig, test2$pred_orig))
rmse_train <- c(rmse_train, rmse(train2$Power_orig, train2$pred_orig))
rmse_test <- c(rmse_test, rmse(test2$Power_orig, test2$pred_orig))

# 10. LightGBM with 1 Variable
lgb_train <- lgb.Dataset(data = as.matrix(train1[, x1]), label = train1[, y])
lgb_test <- lgb.Dataset(data = as.matrix(test1[, x1]), label = test1[, y])
params <- list(objective = "regression", nrounds = 100, learning_rate = 0.1)
lgb_model <- lgb.train(params = params, data = lgb_train)
train1$pred <- predict(lgb_model, as.matrix(train1[, x1]))
test1$pred <- predict(lgb_model, as.matrix(test1[, x1]))
# append results
method <- c(method, "LightGBM (1 var)")
rsq_train <- c(rsq_train, rsq(train1$Power, train1$pred))
rsq_test <- c(rsq_test, rsq(test1$Power, test1$pred))
rmse_train <- c(rmse_train, rmse(train1$Power, train1$pred))
rmse_test <- c(rmse_test, rmse(test1$Power, test1$pred))

# 11. LightGBM with 2 Variables
lgb_train <- lgb.Dataset(data = as.matrix(train2[, x2]), label = train2[, y])
lgb_test <- lgb.Dataset(data = as.matrix(test2[, x2]), label = test2[, y])
params <- list(objective = "regression", nrounds = 100, learning_rate = 0.1)
lgb_model <- lgb.train(params = params, data = lgb_train)
train2$pred <- predict(lgb_model, as.matrix(train2[, x2]))
test2$pred <- predict(lgb_model, as.matrix(test2[, x2]))
# scale predictions back to original magnitude (before normalization)
train2$pred_orig <- train2$pred * max_Power
test2$pred_orig <- test2$pred * max_Power
# append results
method <- c(method, "LightGBM (2 var)")
rsq_train <- c(rsq_train, rsq(train2$Power_orig, train2$pred_orig))
rsq_test <- c(rsq_test, rsq(test2$Power_orig, test2$pred_orig))
rmse_train <- c(rmse_train, rmse(train2$Power_orig, train2$pred_orig))
rmse_test <- c(rmse_test, rmse(test2$Power_orig, test2$pred_orig))



# REGRESSION RESULTS
results <- data.frame(method, rsq_train, rsq_test, rmse_train, rmse_test)
print(results)

# The best model in terms of lowest test RMSE is LightGBM (2 var)



# EVALUATION OF TURBINE PERFORMANCE

# refit best model
lgb_train <- lgb.Dataset(data = as.matrix(train2[, x2]), label = train2[, y])
params <- list(objective = "regression", nrounds = 100, learning_rate = 0.1)
lgb_model <- lgb.train(params = params, data = lgb_train)

# normalize data (before outlier removal) for predictions
data_noZero_norm <- predict(process, data_noZero)
# add column of original (pre-normalization) WindSpeed and TheoreticalPowerCurve values
data_noZero_norm$ws_orig <- data_noZero$WindSpeed
data_noZero_norm$theo_orig <- data_noZero$TheoreticalPowerCurve

# predict Power using best model
data_noZero_norm$pred <- predict(lgb_model, as.matrix(data_noZero_norm[, x2]))
data_noZero_norm$pred_orig <- data_noZero_norm$pred * max_Power

# plot power curve comparison
ggplot() +
  geom_point(data=data_noZero_norm, aes(x=ws_orig, y=pred_orig, color="Predicted")) +
  geom_point(data=data_noZero_norm, aes(x=ws_orig, y=theo_orig, color="Theoretical")) +
  scale_color_manual(values = c("Predicted" = "steelblue1", "Theoretical" = "darkorange")) +
  ggtitle("Predicted vs Theoretical Power Curve") +
  theme(legend.position = c(0.9, 0.5))

# calculate % difference
pdiff <- sum(data_noZero_norm$pred_orig) / sum(data_noZero_norm$theo_orig) - 1
pdiff
cat("The actual generating performance of the turbine is ", 
    sprintf("%.2f%%", 100 * abs(pdiff)),
    " below the theoretical power curve.", sep="")
#==================================================================
# DALEX: Descriptive mAchine Learning EXplanations
#==================================================================

# LIBRARIES

library(moderndive) # For data 
library(ranger)
library(xgboost)
library(DALEX) # For model interpretation
library(ceterisParibus)
library(tidyverse) # For data prep
library(caret) # For data split

# FUNCTIONS
rmse <- function(y, pred) {
  sqrt(mean((y - pred)^2))
}


#==================================================================
# DATA PREPARATION
#==================================================================

dim(house_prices)
View(house_prices)

# Apply necessary data preparation steps
prep <- transform(house_prices, 
                  lPrice = log(price),
                  d2015 = lubridate::year(date) == 2015,
                  age = lubridate::year(date) - yr_built,
                  bedrooms = pmin(bedrooms, 10),
                  bathrooms = pmin(bathrooms, 4),
                  lLotSize = log(sqft_lot),
                  lLivingSize = log(sqft_living),
                  dRenovated = yr_renovated > 0,
                  condition = as.integer(condition),
                  grade = as.integer(grade))

# Some column names
x <- c("d2015", "age", "bedrooms", "bathrooms", "lLivingSize", "lLotSize", 
            "dRenovated", "view", "condition", "grade")
y <- "lPrice"

#==================================================================
# Train/valid split
#==================================================================

set.seed(3928272)
ind <- caret::createDataPartition(prep[[y]], p = 0.80, list = FALSE) %>% c

trainDF <- prep[ind, c(x, y)]
validDF <- prep[-ind, c(x, y)]

trainMat <- as.matrix(trainDF)
validMat <- as.matrix(validDF)

trainXgb <- xgb.DMatrix(trainMat[, x], label = trainMat[, y])

#==================================================================
# MODELS: We fit three count regressions: 
# OLS, Random Forest, Tree Booster
#==================================================================

form <- lPrice ~ d2015 + age + bedrooms + bathrooms + 
  lLivingSize + lLotSize + dRenovated + view + condition + grade

# Linear model
summary(fit_ols <- lm(form, data = trainDF))

# Random forest
(fit_rf <- ranger(reformulate(x, y), data = trainDF))

# Tree booster: XGBoost
param_xgb <- list(max_depth = 5, 
                  learning_rate = 0.05, 
                  nthread = 4, 
                  objective = "reg:linear")

(fit_xgb <- xgb.train(param_xgb,
                      data = trainXgb,
                      nrounds = 500))

#==================================================================
# EXPLAINING THE MODELS
#==================================================================

# We are almost ready to start interpreting the fitted models. 
# First, we need to collect all infos per model required to make predictions 
# and put them into an object called "explainer". It contains
# - The fitted model
# - A data set to be used to evaluate predictions
# - The response for above observations
# - A predict function

## Initializing the "explainer"
explainer_ols <- explain(fit_ols, 
                         data = validDF[, x], 
                         y = validDF[[y]], 
                         label = "ols")

explainer_rf <- explain(fit_rf, 
                         data = validDF[, x], 
                         y = validDF[[y]], 
                         label = "rf")

explainer_xgb <- explain(fit_xgb, 
                         data = validDF[, x], 
                         y = validDF[[y]], 
                         label = "xgb",
                         predict_function = function(model, X) {X <- as.matrix(X); 
                            predict(model, X[, x, drop = FALSE])})

explainers <- list(explainer_ols, explainer_rf, explainer_xgb)

## Model performance
# A simple way to visualize the results of a model is to look at distributions of residuals 
# and to compare them across models.

mp <- lapply(explainers, model_performance)
do.call(plot, mp)

## Variable importance on permuting validation data
# A model agnostic way to study variable importance is to study the worsening 
# of the total loss if the values in variable $X$ are randomly permuted. 
# unimportant variable would lead to a small increase in loss if permuted, 
# while an important one would lead to completely wrong predictions and, 
# consequently to a large gain in loss.

vd <- lapply(explainers, variable_importance, n_sample = -1) # loss can be specified
do.call(plot, vd)

## Variable importance ("prediction breakdown") for one single observation
# The permutation approach provides a "global" variable importance. Additionally, 
# it might be interesting to see which variable is responsible by how much for 
# the prediction of an individual observation. For a linear model, this is just 
# the value of the (centered) regressor times the model coefficient. There are 
# different algorithms to do it in a model agnostic way: LIME, LIVE, Shapely 
# and also prediction breakdown. Here, we use the latter. It works by iteratively 
# evaluating how much the average prediction on the evaluation data changes if all 
# values of variable $X$ are replaced by the corresponding value of the one observation of interest. 

head(validDF, 1)
sp <- lapply(explainers, single_prediction, observation = validDF[1, ])
do.call(plot, sp)

## Effect of grade: ceteris paribus ("everything else being fixed") profiles across models

# Here, we pick one or more individual observations and check how their predictions 
# change if the value of a selected variable is systematically being changed. 
# For an additive linear model, the movement is identical for all observations. 
# These plots make sense as long as ceteris paribus interpretations make sense 
# (i.e. if no strong causal relationships to other regressors exist.)

x_selected <- "grade"
table(house_prices[[x_selected]])

picks <- 1
cp <- lapply(explainers, 
             ceteris_paribus, 
             observations = validDF[picks, x])

do.call(plot, c(cp, color = "_label_", selected_variables = x_selected))

## Effect of driver age: multiple ceteris paribus profiles for the XGBoost model
picks <- 1:20
cp <- ceteris_paribus(explainer_xgb, observations = validDF[picks, x])
plot(cp, selected_variables = x_selected)

## Effect of driver age: average of above profiles aka "partial dependence plots"
# If multiple ceteris paribus profiles are being aggregated, we end up with 
# Friedman's famous partial dependence plots. 

plot(cp, selected_variables = x_selected, aggregate_profiles = mean)


## Variant: Look at profiles of close neighbours

validDF[1, x_selected]
neigh <- select_neighbours(validDF, validDF[1, ])
cp <- ceteris_paribus(explainer_xgb, observations = neigh)
plot(cp, selected_variables = x_selected)

# Fully flexible partial dependence plots for nice reports
source("r/partialDependence.R")

dat <- explainers %>% 
  lapply(partialDependence, list(bedrooms = 1:5, dRenovated = c(FALSE, TRUE))) %>% 
  bind_rows()

ggplot(dat, aes(x = bedrooms, y = predicted, group = label, color = label)) +
  geom_line(alpha = 0.7) +
  facet_wrap(~dRenovated)

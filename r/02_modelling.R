#==================================================================
# Modelling
#==================================================================

# install.packages("https://github.com/mayer79/lightgbm_r_binaries/raw/master/R_3_5_0/lightgbm_2_2_3/lightgbm.zip", repos = NULL) 

library(tidyverse)
library(lightgbm) 
library(ranger)
library(caret)
library(DALEX)

load("rdata/prep.RData", verbose = TRUE)
source("r/functions.R", encoding = "UTF-8")

#==================================================================
# Train / valid / test split (70% / 20% / 10%)
#==================================================================

set.seed(56745)
ind <- caret::createFolds(prep[[y]], k = 10, list = FALSE)

train <- prep[ind >= 4, ]
valid <- prep[ind %in% 2:3, ]
test <- prep[ind == 1, ]

#======================================================================
# Linear regression
#======================================================================

(form <- reformulate(x, y))

fit_lm <- lm(form, data = prep_lm(train))
summary(fit_lm)

#======================================================================
# Random forest
#======================================================================

fit_rf <- ranger(form, data = train, importance = "impurity", seed = 8373)
cat("R-squared OOB:", fit_rf$r.squared)

# object.size(fit_rf)

# In-built variable importance
# par(mar = c(0, 10, 0, 0))
# barplot(fit_rf %>% importance %>% sort, horiz = TRUE, las = 2)

#======================================================================
# Tree booster
#======================================================================

# Note that the column order has to be identical when predicting on new data!
dtrain_lgb <- lgb.Dataset(prep_lgb(train, x), label = train[[y]], 
                          categorical_feature = c("year")) # Not necessary, just as example

# GRID SEARCH PART - CAN BE SKIPPED IF ALREADY TUNED
# paramGrid <- expand.grid(iteration = NA_integer_, # filled by algorithm
#                          score = NA_real_,     # "
#                          learning_rate = c(0.05), # c(1, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01),
#                          num_leaves = c(7, 15, 32), #c(7, 15, 31, 63), # c(31, 63, 127, 255),
#                          # max_depth = 14,
#                          min_data_in_leaf = c(10, 50),
#                          lambda_l1 = 0:2, #0:5, #, c(0, 0.5, 1),
#                          lambda_l2 = 0:2, #0:4, # c(0, 0.1, 0.5, 1:6),
#                          min_sum_hessian_in_leaf = 0, #c(0, 0.01, 0.02), #c(0, 0.001, 0.1),
#                          feature_fraction = c(0.6, 0.8, 1), #c(0.6, 0.8), ## 0.8, #seq(0.4, 1, by = 0.2),
#                          bagging_fraction = 1, #c(0.8, 1), # seq(0.4, 1, by = 0.2),
#                          bagging_freq = 1,
#                          nthread = 4,
#                          mc = "1,0,0,1,0,0,1,1") # monotonicity constraints in the order of x
# (n <- nrow(paramGrid)) # 2160
# set.seed(34234)
# paramGrid <- paramGrid[sample(n, 40), ]
# (n <- nrow(paramGrid)) # 100
# 
# pb <- txtProgressBar(0, n, style = 3)
# 
# for (i in seq_len(n)) {
#   cvm <- lgb.cv(as.list(paramGrid[i, -(1:2)]),
#                 dtrain_lgb,
#                 nrounds = 5000, # we use early stopping
#                 nfold = 5,
#                 objective = "regression",
#                 showsd = FALSE,
#                 early_stopping_rounds = 20,
#                 verbose = -1,
#                 metric = "rmse")
#   paramGrid[i, 1:2] <- as.list(cvm)[c("best_iter", "best_score")]
#   setTxtProgressBar(pb, i)
#   save(paramGrid, file = "rdata/paramGrid_lgb.RData")
# }

load("rdata/paramGrid_lgb.RData", verbose = TRUE)
head(paramGrid <- paramGrid[order(-paramGrid$score), ])

# Use best only
cat("Best rmse (CV):", -paramGrid[1, "score"])

system.time(fit_lgb <- lgb.train(paramGrid[1, -(1:2)], 
                                 data = dtrain_lgb, 
                                 nrounds = paramGrid[1, "iteration"],
                                 objective = "regression",
                                 seed = 2698)) 

# In-built variable importance
# imp_lgb <- lgb.importance(fit_lgb)
# print(imp_lgb)
# lgb.plot.importance(imp_lgb, top_n = length(x), cex = 0.8)

#======================================================================
# Model interpretation via DALEX explainers
#======================================================================

## Initializing the "explainers"
explainer_lm <- explain(fit_lm, data = valid[, x], y = valid[[y]], 
                        predict_function = function(model, X) predict(model, prep_lm(X)),
                        label = "Linear regression", link = exp)

explainer_rf <- explain(fit_rf, data = valid[, x], y = valid[[y]], 
                        label = "Random forest", link = exp)

explainer_lgb <- explain(fit_lgb, data = valid[, x], y = valid[[y]], label = "Tree booster", 
                         link = exp,
                         predict_function = function(model, X) predict(model, prep_lgb(X, x)))

explainers <- list(explainer_lm, explainer_rf, explainer_lgb)

# Model performance
perfTable <- lapply(explainers, performance, 
                    metrics = c(`root-mean-squared error` = rmse, `mean absolute deviation` = mae, `R-squared` = r2)) %>% 
  bind_rows()

# Variable importance
set.seed(3452)
varImp <- lapply(explainers, variable_importance, n_sample = -1, 
                 loss = mse, type = "difference") %>% 
  bind_rows %>% 
  filter(substring(variable, 1, 1) != "_")

most_relevant <- varImp %>% 
  group_by(variable) %>% 
  summarize(loss = sum(dropout_loss, na.rm = TRUE)) %>% 
  arrange(desc(loss)) %>% 
  pull(variable) %>% 
  as.character()

# Partial dependence enriched by descriptive info
eff <- lapply(most_relevant, effects, data = prep, explainers = explainers, response = "price") %>% 
  setNames(most_relevant)

# Save results (no models)
save(perfTable, varImp, most_relevant, eff, file = "rdata/models.RData")

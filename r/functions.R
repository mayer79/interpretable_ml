#==================================================================
# Functions
#==================================================================

#==================================================================
# 1) General functions
#==================================================================

# Partial dependency plot data for DALEX explainer.
#'
#' @import dplyr
#' @importFrom tidyr crossing
#' 
#' @description Generates partial dependence plot data based on a DALEX explainer. Note that there is an unkeyed outer join of the explainer data and all combinations of the grid variables. Thus, don't use too many grid points or too large explainer data. Without aggregation, the function would return ceteris paribus profiles.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param explainer DALEX explainer.
#' @param grid A named list of grid points.
#' @param by A vector of column names used to additionally group the results. Like this, it would also be possible to generate ceteris paribus profiles.
#' @param w An column name with case weights.
#' 
#' @return Data frame with partial dependence data.
#' 
#' @example
#' \dontrun{ 
#' library(DALEX)
#' library(tidyverse)
#' fit <- lm(log(Sepal.Length) ~ ., data = iris)
#' explainer <- explain(fit, data = iris, label = "log_ols", link = exp)
#' dat <- partialDependence(explainer, grid = list(Species = levels(iris$Species)))
#' ggplot(dat, aes(x = Species, y = predicted)) + geom_point()
#' 
#' dat <- partialDependence(explainer, grid = list(Species = levels(iris$Species), Petal.Length = 1:6), 
#'                          w = "Sepal.Width")
#' ggplot(dat, aes(x = Petal.Length, y = predicted, group = Species, color = Species)) + geom_point()
#' }
partialDependence <- function(explainer, grid, by = NULL, w = NULL) {
  stopifnot(inherits(explainer, "explainer"), 
            is.list(grid), length(grid) >= 1, 
            c(by, w, (v <- names(grid))) %in% colnames(explainer$data), 
            !any(c("label", "predicted") %in% v))
  
  explainer$data %>% 
    select(-one_of(v)) %>% 
    crossing(expand.grid(grid)) %>% 
    mutate(predicted = with(explainer, link(predict_function(model, .)))) %>% 
    group_by_at(c(v, by)) %>% 
    {if (is.null(w)) summarize(., predicted = mean(predicted, na.rm = TRUE)) else .} %>% 
    {if (!is.null(w)) summarize(., predicted = weighted.mean(predicted, w = !!sym(w), na.rm = TRUE)) else .} %>% 
    ungroup() %>% 
    mutate(label = explainer$label)
}

# Model performance evaluation for DALEX explainer.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' 
#' @description Returns model performance metrics of a DALEX explainer for a named list or vector of performance metric functions of the form f(y, pred) = value.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param explainer DALEX explainer.
#' @param metrics A named list or vector of metric functions.
#' 
#' @return Data frame with performance values in "long" form.
#' 
#' @example
#' \dontrun{ 
#' library(DALEX)
#' library(ModelMetrics)
#' library(tidyverse)
#' fit <- lm(log(Sepal.Length) ~ ., data = iris)
#' explainer <- explain(fit, data = iris, y = log(iris$Sepal.Length), 
#'                      label = "log_ols", link = exp)
#' dat <- performance(explainer, c(rmse = rmse, mae = mae))
#' ggplot(dat, aes(x = type, y = value, fill = type)) + 
#'   geom_bar(stat = "identity")
#' }
performance <- function(explainer, metrics) {
  stopifnot(inherits(explainer, "explainer"))

  with(explainer,
       vapply(metrics, function(fun, a, b) fun(a, b), FUN.VALUE = 1, 
         a = y, b = predict_function(model, data))) %>% 
    t() %>% 
    data.frame(label = explainer$label, ., check.names = FALSE) %>% 
    gather(key = "type", value = "value", -label) %>% 
    mutate(type = factor(type, names(metrics)))
}

# performance <- function(explainer, metrics, by = NULL, w = NULL) {
#   stopifnot(inherits(explainer, "explainer"))
#   data <- with(explainer, 
#     cbind(data[, c(by, w), drop = FALSE], y = y, 
#           pred = with(explainer, predict_function(model, data))))
#   apply(metrics, 
#   data %>% 
#     group_by_at(by) %>% 
#     summarize(function(fun, a, b) fun(a, b), FUN.VALUE = 1, 
#                       a = y, b = pred))
#   with(explainer,
#        vapply(metrics, function(fun, a, b) fun(a, b), FUN.VALUE = 1, 
#               a = y, b = predict_function(model, data))) %>% 
#     t() %>% 
#     data.frame(label = explainer$label, ., check.names = FALSE) %>% 
#     gather(key = "type", value = "value", -label) %>% 
#     mutate(type = factor(type, names(metrics)))
# }

# R-squared
#'
#' @importFrom stats weighted.mean
#' 
#' @description Returns (weighted) R-squared of predicted values.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param y Observed values.
#' @param pred Predicted values.
#' @param w Optional case weights.

#' @return The R-squared.
#' 
#' @example
#' r2(1:10, c(1, 1:9))
#' r2(1:10, c(1, 1:9), w = rep(1, 10))
#' r2(1:10, c(1, 1:9), w = 1:10)
r2 <- function(y, pred, w) {
  1 - weighted.mean((y - pred)^2, w) / weighted.mean((y - mean(y))^2, w)
}

# Root-mean-squared error
#'
#' @importFrom stats weighted.mean
#'
#' @description Returns (weighted) root-mean-squared error of predicted values.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param y Observed values.
#' @param pred Predicted values.
#' @param w Optional case weights.

#' @return The root-mean-squared error.
#' 
#' @example
#' rmse(1:10, (1:10)^2)
#' rmse(1:10, (1:10)^2, w = rep(1, 10))
#' rmse(1:10, (1:10)^2, w = 1:10)
rmse <- function(y, pred, w) {
  sqrt(weighted.mean((y - pred)^2, w))
}

# Mean absolute error
#'
#' @importFrom stats weighted.mean
#'
#' @description Returns (weighted) mean absolute error of predicted values.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param y Observed values.
#' @param pred Predicted values.
#' @param w Optional case weights.

#' @return The mean absolute value.
#' 
#' @example
#' mae(1:10, (1:10)^2)
#' mae(1:10, (1:10)^2, w = rep(1, 10))
#' mae(1:10, (1:10)^2, w = 1:10)
mae <- function(y, pred, w) {
  weighted.mean(abs(y - pred), w)
}

# Values describing the box of a boxplot. Can be used together with \code{geom_stats} of \code{ggplot}.
#'
#' @importFrom stats quantile
#' 
#' @description Returns median and the two other quartiles.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param x Numeric vector to be described.

#' @return A \code{data.frame} with one row containing the median and the two quarties.
#' 
#' @example
#' box_stats(1:10)
box_stats <- function(x) {
  out <- data.frame(t(quantile(x, probs = c(0.5, 0.25, 0.75), na.rm = TRUE)))
  names(out) <- c("y", "ymin", "ymax")
  out
}

# # Formats large numbers like 1'000'000.
#' 
#' @description # # Formats large numbers as 1'000'000.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param x Numeric vector to be formatted.

#' @return A character vector of the same length as \code{x}.
#' 
#' @example
#' format_large(1000000)
format_large <- function(x) {
  format(x, big.mark = "'", scientific = FALSE)
}

#==================================================================
# 2) Specific functions for this project
#==================================================================

# Interface to lm
prep_lm <- function(data) {
  data %>% 
    mutate(sqrt_living = log(sqft_living),
           sqrt_lot = log(sqft_lot),
           zipcode = factor(zipcode %/% 10))
}

# Interface to lgb
prep_lgb <- function(data, x = colnames(data)) {
  data %>% 
    select_at(x) %>% 
    mutate_if(is.factor, as.integer) %>% 
    data.matrix()
}

# Calculates both descriptive effects and effects from partial dependence plot
effects <- function(data, v, explainers, response, breaks = NULL) {
  stopifnot(c(v, response) %in% colnames(data))
  
  # Deal with x breaks... (ugly)
  if (is.numeric(vv <- data[[v]]) && length(unique(vv)) > 15) {
    if (!is.null(breaks)) {
      data$temp_ <- cut(vv, breaks = breaks, include.lowest = TRUE)
    } else {
      data$temp_ <- Hmisc::cut2(vv, g = 11, trim = TRUE)
    }
    grid <- data %>% 
      group_by(temp_) %>% 
      summarize_at(v, mean, na.rm = TRUE)
  } else {
    data$temp_ <- data[[v]]
    grid <- if (is.factor(vv)) factor(levels(vv), levels(vv)) else unique(vv) 
    grid <- setNames(data.frame(grid, grid), c("temp_", v))
  }
  
  # Data for partial dependence plot
  modelled <- lapply(explainers, partialDependence, grid = grid[, v, drop = FALSE]) %>% 
    bind_rows() %>% 
    left_join(grid, by = v) %>% 
    select(-one_of(v)) %>% 
    rename_at("predicted", function(nm) response)

  # Descriptive data joined with partial dependence data and scaling data
  list(modelled = modelled, data = select_at(data, c(response, "temp_")))
}




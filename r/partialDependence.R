# Partial dependency plot data for DALEX explainer.
#'
#' @import dplyr
#' @importFrom tidyr crossing
#' 
#' @description Generates partial dependence plot data based on a DALEX explainer. Note that there is an unkeyed outer join of the explainer data and all combinations of the grid variables. Thus, don't use too many grid points or too large explainer data.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param explainer DALEX explainer.
#' @param grid A named list of grid points.
#' @param by A vector of column names used to additionally group the results. Like this, it would also be possible to generate ceteris paribus profiles.

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
#' dat <- partialDependence(explainer, grid = list(Species = levels(iris$Species), Petal.Length = 1:6))
#' ggplot(dat, aes(x = Petal.Length, y = predicted, group = Species, color = Species)) + geom_point()
#' 
#' dat <- partialDependence(explainer, grid = list(Petal.Length = 1:6), by = "Species")
#' ggplot(dat, aes(x = Petal.Length, y = predicted, group = Species, color = Species)) + geom_point()
#' }
partialDependence <- function(explainer, grid, by = NULL) {
  stopifnot(inherits(explainer, "explainer"), 
            is.list(grid), length(grid) >= 1, 
            c(by, (v <- names(grid))) %in% colnames(explainer$data), 
            !any(c("label", "predicted") %in% v))
  
  explainer$data %>% 
    select(-one_of(v)) %>% 
    crossing(expand.grid(grid)) %>% 
    mutate(predicted = with(explainer, link(predict_function(model, .)))) %>% 
    group_by_at(c(v, by)) %>% 
    summarize(predicted = mean(predicted, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(label = explainer$label)
}

# Partial dependency plot data for DALEX explainer.
#'
#' @import dplyr
#' @importFrom tidyr crossing
#' 
#' @description Generates partial dependence plot data based on a DALEX explainer.
#' @author Michael Mayer, \email{mayermichael79@gmail.com}
#' @param explainer DALEX explainer.
#' @param grid A named list of grid points.

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
#' }
partialDependence <- function(explainer, grid) {
  stopifnot(inherits(explainer, "explainer"), 
            is.list(grid), length(grid) >= 1, 
            (v <- names(grid)) %in% colnames(explainer$data), 
            !any(c("label", "predicted") %in% v))
  
  explainer$data %>% 
    select(-one_of(v)) %>% 
    crossing(expand.grid(grid)) %>% 
    mutate(predicted = with(explainer, link(predict_function(model, .)))) %>% 
    group_by_at(v) %>% 
    summarize(predicted = mean(predicted, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(label = explainer$label)
}

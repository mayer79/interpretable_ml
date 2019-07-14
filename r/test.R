eff <- effects(v, data = prep, explainers = explainers, response = "price")

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
  
  list(modelled = modelled, 
       data = data %>% select_at(c(response, "temp_")), 
       counts = count(data, temp_))
}

box_stats <- function(x) {
  quantile(x, probs = c(0.5, 0.25, 0.75), na.rm = TRUE) %>%
    t() %>% 
    data.frame() %>% 
    setNames(c("y", "ymin", "ymax"))
}

resp <- ggplot(mapping = aes(x = factor(temp_), y = !!sym(response))) +
  stat_summary(data = eff$data, fun.data = box_stats, geom = "crossbar",
               width = 0.5, fill = "lightblue", colour = "#414487FF") +
  geom_line(data = eff$modelled, aes(group = label, color = label), size = 1.5, alpha = 0.7) +
  geom_point(data = eff$modelled, aes(group = label, color = label), size = 1.5, alpha = 0.7) +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.2, 0.8), legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  xlab(v) +
  scale_y_continuous(name = "Price", labels = scales::comma) +
  scale_color_viridis_d(begin = 0.2, end = 0.9)

counts <- ggplot(eff$counts, aes(x = temp_, y = n,
                                      label = format(n, big.mark = "'", scientific = FALSE))) +
  geom_bar(stat = "identity", fill = "#414487FF", alpha = 0.3) +
  geom_text(aes(y = 0), angle = 90, hjust = -0.1, size = 6) +
  theme_void() +
  theme(strip.text.x = element_blank(), panel.grid = element_blank())

print(ggarrange(counts, resp, heights = c(0.2, 1), ncol = 1, nrow = 2, align = "v"))

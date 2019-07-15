#==========================================================================
# Data preparation
#==========================================================================

library(moderndive) # "house_prices" data

dim(house_prices)
head(house_prices)

# Apply necessary data preparation steps
prep <- transform(house_prices, 
                  log_price = log(price),
                  grade = as.integer(as.character(grade)),
                  year = factor(lubridate::year(date)),
                  age = lubridate::year(date) - yr_built,
                  zipcode = as.numeric(as.character(zipcode)),
                  waterfront = factor(waterfront, levels = c(FALSE, TRUE), labels = c("no", "yes")))

# Special columns
y <- "log_price"

x <- c("grade", "year", "age", "sqft_living", "sqft_lot", "zipcode", 
       "condition", "waterfront")

save(prep, y, x, file = "rdata/prep.RData")


# Exploratory Data Analysis on NFL Combine Data
library(nflreadr)
combine_data <- load_combine()

sum(is.na(combine_data$draft_ovr)) # 3,194 rows with no dependent variable

# Get rid of NA dependent variable rows
combine_data_clean <- combine_data[!is.na(combine_data$draft_ovr), ]

sum(is.na(combine_data_clean$forty)) # 330 rows
sum(is.na(combine_data_clean$bench)) # 1714 rows
sum(is.na(combine_data_clean$vertical)) # 1112 rows
sum(is.na(combine_data_clean$broad_jump)) # 1172 rows
sum(is.na(combine_data_clean$cone)) # 1965 rows
sum(is.na(combine_data_clean$shuttle)) # 1896 rows

# Define columns to remove NA values
cols_to_remove_na <- c("forty", "bench", "vertical", "broad_jump", "cone", "shuttle")

# Remove rows with NA values in specified columns
combine_data_super_clean <- combine_data_clean[complete.cases(combine_data_clean[, ..cols_to_remove_na]), ]

# Should we set NA rows to 0? Must decide this
# Can't do this for forty, cone, shuttle as lower values are better, might have to remove NA vals
# Replace NA values with 0 in multiple columns
cols_to_replace <- c("bench", "broad_jump", "vertical")
combine_data_clean_replaced[, (cols_to_replace) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = cols_to_replace]

# Scatter matrix
cols <- c("draft_ovr", "forty", "bench", "vertical", "broad_jump", "cone", "shuttle")
pairs(combine_data_super_clean[, ..cols])

# Plot individual feature against dependent variable
plot(combine_data_super_clean$shuttle, combine_data_super_clean$draft_ovr)

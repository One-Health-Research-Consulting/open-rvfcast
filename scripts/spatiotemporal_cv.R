install.packages("spatialsample")
library(spatialsample)
library(rsample)
library(tidyroll)


# Create sample data
set.seed(123)
populations <- paste("Pop", 1:5, sep = "_")
dates <- seq.Date(from = as.Date("2025-01-01"), by = "days", length.out = 25)
temperatures <- runif(125, min = -10, max = 35)  # Temperatures between -10 and 35

# Expand grid for combinations of populations and dates
fake_data <- expand.grid(population = populations, date = dates)

# Add temperatures to the dataframe
fake_data$temp <- temperatures

# View first few rows of the data
head(fake_data)

# Key here is to first sort by date and figure out how many we should be assessing and skipping
# For this to work each date must have the same number of entries. Must  Or maybe I can use 
# This does spatio-temporal cross-validation.
skip_rows = unique(table(fake_data$date))
assertthat::are_equal(length(skip_rows), 1)


# This code performs nested cross-validation using the `rsample` package, combining 
# a rolling origin resampling strategy for the outer cross-validation loop and 
# a spatial leave-location-out strategy for the inner cross-validation loop.
folds <- fake_data |> rsample::nested_cv(
    # Outer cross-validation: Rolling origin resampling
    outside = rolling_origin(
      initial = skip_rows,  # The size of the initial training set (number of rows).
      assess = skip_rows,   # The size of the assessment set (test set) for each split.
      skip = skip_rows      # The number of rows skipped between successive splits.
    ),
    # Inner cross-validation: Spatial leave-location-out
    inside = spatial_leave_location_out_cv(
      group = "population"  # The column in the data that defines spatial groups (here, populations).
      # Each "location" (population) will be left out in turn for validation.
    )
  )


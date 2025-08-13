library(terra)
library(tidyverse)
library(targets)

tar_load(africa_full_predictor_data_sources)

first_elements_list <- lapply(africa_full_predictor_data_sources, `[[`, 1) |> map(~arrow::read_parquet(.))
# Assume 'first_elements_list' is named
df_names <- names(first_elements_list)

# If it doesn't have names, make some
if (is.null(df_names)) {
  df_names <- paste0("df", seq_along(first_elements_list))
}

# Extract unique x,y from each dataframe
xy_sets <- lapply(first_elements_list, function(df) {
  unique(df[c("x", "y")])
})
names(xy_sets) <- df_names

# Progressive intersection tracking
common_xy <- xy_sets[[1]]
drop_points <- data.frame(
  dataframe = character(),
  common_rows = integer(),
  stringsAsFactors = FALSE
)

for (i in 2:length(xy_sets)) {
  common_xy <- merge(common_xy, xy_sets[[i]], by = c("x", "y"))
  drop_points <- rbind(drop_points, data.frame(
    dataframe = df_names[i],
    common_rows = nrow(common_xy)
  ))
}

drop_points



dat_out <- arrow::read_parquet(tar_read(ndvi_anomalies)[[1]])
dat_out1 <- arrow::read_parquet(tar_read(forecasts_anomalies)[[1]])
dat_out2 <- arrow::read_parquet(tar_read(weather_anomalies)[[1]])
dat_out2
dat_out2

dat_out %>% dplyr::filter(doy == 14) %>% {
  ggplot(., aes(x, y, z = anomaly_temperature)) +
    geom_tile(aes(fill = anomaly_temperature)) #+
  #scale_x_continuous(limits = c(19, 22)) +
  # scale_y_continuous(limits = c(19, 22))
}

dat_out %>% {
  ggplot(., aes(x, y, z = ndvi)) +
    geom_point(aes(fill = ndvi)) +
    scale_x_continuous(limits = c(19, 22)) +
    scale_y_continuous(limits = c(19, 22))
}

library(terra)
library(gstat)
library(dplyr)
library(ggplot2)

template <- tar_read(continent_raster_template) |> unwrap()
values(template) <- 0
centroids <- crds(template, df=TRUE)

library(terra)
library(gstat)
library(dplyr)
library(ggplot2)

template <- tar_read(continent_raster_template) |> unwrap()
values(template) <- 0
centroids <- crds(template, df=TRUE)

# Convert to SpatialPoints for gstat
library(sp)
pts_sp <- SpatialPoints(centroids[, c("x", "y")])

# Define variogram model (adjust range/sill as needed)
vgm_model <- vgm(psill=1, model="Exp", range=5)

# Set up gstat for simulation
gs <- gstat(formula = z ~ 1, locations = pts_sp,
            dummy = TRUE, beta=0, model = vgm_model, nmax = 30)

# Simulate one spatially correlated realization
set.seed(42)
simulated <- predict(gs, newdata = pts_sp, nsim = 1)

# Add simulated values to centroids df
centroids$z <- simulated$sim1

# Plot
ggplot(centroids, aes(x, y, fill = z)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c() +
  theme_minimal()

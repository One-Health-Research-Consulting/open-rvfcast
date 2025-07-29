library(terra)
library(tidyverse)
library(targets)

dat_out <- tar_read(nasa_weather_transformed)[[1]] |> 
  arrow::read_parquet() |>
  filter(day == 1)

dat_out %>% {
  ggplot(., aes(x, y, z = relative_humidity)) +
    geom_tile(aes(fill = relative_humidity)) #+
  #scale_x_continuous(limits = c(19, 22)) +
  # scale_y_continuous(limits = c(19, 22))
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

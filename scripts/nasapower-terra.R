# Load packages (in packages.R) and load project-specific functions in R folder
suppressPackageStartupMessages(source("packages.R"))

# https://github.com/adamhsparks/nasapower-example

tar_load(continent_polygon)

# Now that we have objects for the states we can create a raster grid to represent the 0.5 x 0.5 
# degree grid that is the NASA-POWER data and select only cells that fall within the two states of interest.

# new target for raster setup
# from HERE
r <- rast(
  nrows = 360,
  ncols = 720,
  xmin = -180,
  xmax = 180,
  ymin = -90,
  ymax = 90,
  resolution = 0.1
)

values(r) <- 1:ncell(r)

plot(r, main = "Full global raster at 0.5 x 0.5 degrees")

# Extract continent_polygon, first crop by bounding box, then mask the raster
# Since terra doesn't play nice with `sf` yet we need to convert the objects
# to spatial data frames, which we do in-operation using `as()`
coords <- crop(r, as(continent_polygon, "Spatial"))
coords <- mask(coords, continent_polygon)
plot(coords, main = "Africa")
plot(continent_polygon, col = NA, add = TRUE)

# extract the centroid values of the cells to use querying the POWER data
coords <- as.data.frame(xyFromCell(coords, 1:ncell(coords)))
names(coords) <- c("lon", "lat")
# to HERE

# Using nested for loops, query the NASA-POWER database to gather precipitation data for the states where rust was reported and save a CSV file of the rainfall.

power <- vector(mode = "list", 4) # hold four growing seasons
precip <- vector(mode = "list", nrow(coords)) # hold the cells

seasons <- list(c("2014-11-01", "2015-01-31"),
                c("2015-11-01", "2016-01-31"),
                c("2016-11-01", "2017-01-31"),
                c("2017-11-01", "2018-01-31"))

#TODO refactor this so that you can use dynamic branching by year 



for (i in seq_along(seasons)) { # four seasons (outer loop 4x)
  season <- seasons[[i]]
  
  for (j in seq_along(nrow(coords))) { # 312 coordinate pairs (inner loop 312x)
    site <- as.numeric(coords[j, ])
    power_precip <- get_power(community = "AG",
                              lonlat = site,
                              pars = "PRECTOT",
                              dates = season,
                              temporal_average = "DAILY"
    )
    precip[[j]] <- power_precip
    Sys.sleep(5) # wait 5 seconds between requests so we don't hammer the server
  }
  precip <- bind_rows(power_precip)
  power[[i]] <- precip
}

power <- bind_rows(power)

#TODO save end result as the raster object based on coords
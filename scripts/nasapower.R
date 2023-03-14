library("nasapower")
daily_ag <- get_power(community = "ag",
                      lonlat = c(15, -35, 20, -30), # xmin (W), ymin (S), xmax (E), ymax (N)
                      pars = c("RH2M", "T2M", "PRECTOTCORR"),
                      dates = c("1993-01-01", as.character(ymd(Sys.Date()))), #
                      temporal_api = "daily"
)
daily_ag

# TODO figure out spatial and temporal blocking for the download
# by year
# by 5 x 5 region of 1 degree values, (i.e., 100 points total)
lonlat = c(xmin = 15, ymin = -35,xmax= 37, ymax = -21) 
seq(lonlat["xmin"], lonlat["xmax"], by = 5)

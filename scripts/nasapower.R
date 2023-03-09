library("nasapower")
daily_ag <- get_power(community = "ag",
                      lonlat = c(112.5, -55.5, 115.5, -50.5)
                      pars = c("RH2M", "T2M", "PRECTOTCORR"),
                      dates = "1985-01-01",
                      temporal_api = "daily"
)
daily_ag

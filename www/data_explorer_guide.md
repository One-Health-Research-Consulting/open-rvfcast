# OpenRVFcast Data Explorer — User Guide

## What this app shows

The Data Explorer lets you browse the covariate stack used to drive the RVF forecast 
model — without any model predictions. Each spatial unit is an H3 hex (fine-resolution
comparable to the **"Child hexes"** in the prediction explorer -- the scale 
at which the predictions are made natively), and variables can be explored as 
spatial maps, scatter plots, time series, or distributions. Use this app to understand 
the input data landscape and to contextualize any particular hex within the broader covariate space.

For model output (predicted outbreak probabilities), see the **Prediction Explorer** app.

---

## Layout

- **Left panel**: Interactive map. Select a covariate from the dropdown; the map 
updates to show that variable's value for each hex. Click any hex to select it for 
the right-panel analyses.
- **Right panel**: Three tabs — Correlation, Time Series, and Density — each offering 
a different analytical view of the covariates.

---

## Covariate categories

Variables in the dropdown are drawn from four groups:

| Category | Description |
|---|---|
| **Static** | BioClim climate normals 
(1970–2000 averages: annual mean temperature, temperature/precipitation seasonality, 
wettest/driest quarter means, etc.), elevation, land cover fractions (trees, shrubs, 
grass, cropland, bare ground, water, wetlands), soil texture and drainage class, livestock 
density (cattle, sheep, goats). |
| **Lagged** | Anomalies of weather (NASA MERRA-2/POWER) and NDVI, averaged over three 30-day 
windows looking *backward* from the prediction date (0–30, 31–60, 61–90 days prior). The interval
is given as a suffix (e.g., _60) after the name of the variable. Each value is the deviation 
from the historical day-of-year mean, divided by the historical day-of-year standard 
deviation. These variables are the temporal mirror of the forecast covariates, capturing 
how recent environmental conditions have deviated from normal. |
| **Forecast** | ECMWF seasonal forecasts of precipitation, temperature, and 
relative humidity — the mean of a 51-member ensemble (see 
https://www.ecmwf.int/en/forecasts/documentation-and-support/seasonal) — aggregated over 
each of the five 30-day forecast windows. Like the anomaly lagged variables, anomaly forecast
scaled values were calculated by subtracting the forecast from the historical mean and then
dividing by the historical standard deviation. |
| **Temporal** | Time-varying covariates encoding seasonal or temporal structure (e.g., month, 
time index). Currently, the only time-varying covariate that is not lagged or forecast 
is modeled host seroprevalence (`pred_sero`) -- see the section "Notes on seroprevalence"
at the bottom of this guide for more info on this covariate. |

All continuous variables were standardized for model fitting. The values displayed 
here are those standardized values, so units are standard deviations from the 
Africa-wide mean rather than native units.

---

## Left panel — Map

Shows all hexes across Africa colored by the selected covariate (viridis scale). 
Values displayed are:
- **Static, lagged, and temporal covariates**: time-averaged mean across the full 
study period for each hex.
- **Forecast covariates**: averaged over the study period for the selected forecast 
window. (A forecast-window selector appears automatically when a forecast covariate is chosen.)

Hover over any hex for its ID and covariate value. Click to select it for use in 
the right-panel plots; the selected hex is highlighted with a yellow outline and its 
value is shown in a pop-up in the lower-left corner.

---

## Right panel — Correlation tab

Scatter plot of two selected covariates, with each point representing one hex 
(values averaged across all time). Use this to explore relationships between 
predictors — e.g., livestock density vs. vegetation index, or precipitation 
seasonality vs. temperature range.

Select the X and Y variables from the two dropdowns. Values shown are hex-level 
time-averages, so this view captures the spatial covariance structure across 
Africa rather than temporal dynamics.

---

## Right panel — Time Series tab

After clicking a hex on the map, shows the temporal trajectory of up to four 
selected variables for that hex over the study period. Only non-static covariates 
(lagged, forecast, temporal) vary through time and will produce meaningful time 
series. Select variables from the dropdown (maximum 4 at once).

**Note on temporal resolution**: the covariate stack is built from two randomly 
sampled dates per calendar month, not daily data. Time series will therefore appear 
as irregularly spaced points at roughly half-month intervals rather than a continuous daily record.

This view is particularly useful for examining how climate anomalies and lagged 
weather signals co-vary through time at a specific location, and for understanding 
the temporal drivers that would have fed into predictions at different points in time.

---

## Right panel — Density tab

After clicking a hex on the map, shows two overlapping density curves for a selected variable:

- **Grey fill**: distribution across **all hexes and dates** — the full Africa-wide 
range of values seen for that covariate.
- **Blue outline**: distribution for **the clicked hex only** across all dates.

This situates the selected hex within the overall covariate space. A hex whose blue 
curve sits far in the tail of the grey distribution occupies an unusual part of 
the predictor space — potentially relevant for understanding where model 
extrapolation may be a concern, or for identifying ecologically extreme locations.

---

## Notes on seroprevalence (`pred_sero`)

`pred_sero` is the model-estimated seroprevalence (fraction of the livestock 
population with prior exposure to RVF) for each hex at each date. It is derived 
from a Bayesian spatio-temporal model fit to a pan-African animal seroprevalence 
dataset compiled from the published literature, with WAHIS outbreak records as 
the primary driver of spatial and temporal variation.

The model has two components: (1) a **mechanistic spatio-temporal kernel** that 
accumulates the influence of past outbreaks on seroprevalence, weighted by spatial 
proximity and temporal recency using a separable exponential decay; and (2) 
an **ICAR spatial random field** that absorbs residual spatial variation not 
explained by recorded outbreaks — capturing the substantial underreporting inherent in WAHIS data.

A key biological point is that **RVF immunity is lifelong** in ruminants: once 
exposed, an animal retains immunity for life. The temporal decay parameter in the 
kernel therefore does not represent waning immunity but rather **demographic turnover** — 
the gradual replacement of immune individuals by susceptible newborns over time. 
The spatial decay parameter captures how quickly outbreak influence attenuates with distance.

In the forecast model, `pred_sero` acts as a proxy for the size of the susceptible 
pool: higher seroprevalence (reflecting recent or nearby outbreak history) implies 
fewer susceptible hosts and thus lower per-capita transmission potential; lower 
seroprevalence implies a larger naive pool and higher outbreak risk. Because the 
seroprevalence model can be run prospectively using publicly available WAHIS data, 
it integrates into the automated forecasting pipeline without requiring new serosurveys.

---

## Notes on livestock density (GLW)

Livestock density covariates (`glw_cattle`, `glw_sheep`, `glw_goats`) are drawn 
from the Global Livestock of the World (GLW) database (censused in 2010) and 
represent density (animals per km²). These are included as proxies for the size and 
spatial distribution of the amplifying host population, which strongly determines 
both transmission potential and the geographic footprint of outbreaks. Note that 
these are static covariates — the model does not track livestock population 
changes over time.

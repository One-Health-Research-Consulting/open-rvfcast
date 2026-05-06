# OpenRVFcast Prediction Explorer — User Guide

## What this app shows

This app displays the output of an XGBoost model trained to forecast RVF outbreak 
probability across Africa. For each date in the evaluation period and each location 
(H3 hex), the model produces a predicted probability that an outbreak will occur — 
separately for five forecast time windows into the future.

---

## Spatial units

The map uses the [H3 hexagonal grid](https://h3geo.org/) at two nested resolutions. 
**"Child hexes"** are the fine-resolution `prediction-level` units (i.e., the scale 
at which the predictions are made natively); **"parent hexes"** aggregate 
multiple child hexes into a larger, coarser tile. Most map views display parent hexes 
for readability; the Sub-Hex panel in Tab 1 reveals the finer-resolution child hexes 
(usually 7 unless bordering the ocean) within a clicked parent.

---

## Forecast intervals

For each date and hex, the model generates five predictions corresponding to 
non-overlapping 30-day windows ahead of that date:

| Label | Window |
|-------|--------|
| 30    | 0–30 days ahead |
| 60    | 31–60 days ahead |
| 90    | 61–90 days ahead |
| 120   | 91–120 days ahead |
| 150   | 121–150 days ahead |

---

## Probability aggregation

When predictions from multiple hexes or multiple forecast windows are combined, the 
app uses the **complement-product rule**: *P(at least one outbreak) = 1 − ∏(1 − pᵢ)*. 
This is the probability that at least one of the included units experiences an 
outbreak, assuming independence.

---

## Map color scheme

- **White → navy blue**: predicted outbreak probability (0 = white, 1 = navy).
- **Orange-red outline**: an outbreak was recorded in that hex during the relevant period.
- **Yellow outline**: currently selected hex (after clicking).

---

## Tab 1 — Predictions Over Time

This tab lets you step through dates and forecast windows to see the spatial 
pattern of predicted outbreak risk.

**Controls**
- Top slider: move through dates in the evaluation period.
- Second slider: select the forecast window (30–150 days).
- Play button: move through time automatically.

### Main map — Date-Specific Forecast
Shows predicted outbreak probability for each parent hex on the selected date and 
forecast window. Each value is the probability that *at least one child hex within the parent hex* 
experiences an outbreak, computed via the complement-product rule across the child predictions. 
Click any hex to activate the "drill-down"" panels on the right.

### Sub-Hex Spatial Detail
Appears after clicking a parent hex. Shows the individual child hexes within that parent, 
colored by their predicted outbreak probability. **Note:** these child-hex values collapse 
across all five forecast windows (i.e., they show the probability of an outbreak across any 
of the five windows) — they do not reflect only the forecast interval selected on the top slider. 
Use this panel to examine spatial heterogeneity hidden by the coarser parent hex mapping.

### Forecast Horizon Profile
Shows how predicted `P(outbreak)` for the clicked hex changes across the five forecast windows 
for the selected date. A flat profile suggests the model assigns similar risk regardless of 
lead time; a declining profile indicates near-term risk is elevated. Dashed orange-red 
vertical lines mark windows in which an outbreak was actually recorded.

### Probability Distributions
Density plot (logit scale) of predicted outbreak probabilities for *all parent hexes* 
on the selected date and forecast window, split by whether a recorded outbreak 
occurred (Yes/No). Good model discrimination appears as clear separation between the 
two distributions — high probabilities concentrated in the "Yes" group and low in 
the "No" group. The logit transformation spreads apart probabilities near 0 and 1, 
making the low end of the distribution easier to examine.

### Calibration Curve
Shows whether predicted probabilities correspond to observed outbreak rates, for 
the time window that covers the selected date. Each point represents a group of 
predictions with similar predicted probabilities.

- **X-axis (log scale)**: Observed outbreak rate — fraction of observation units 
in that group that actually experienced an outbreak (Wilson 95% CI shown as horizontal 
error bars).
- **Y-axis (log scale)**: Median predicted probability for that group (thin 
bars = 2.5th/97.5th percentile; thick bars = 20th/80th percentile).
- **Diagonal grey line**: Perfect calibration (predicted = observed).
- **Point size**: log(number of observations in that bin) — larger points are more reliable.
- **Two colours**: *Optimum Bins* uses reliability-diagram optimization to place 
breakpoints where data are most informative; *Even Bins* uses equal-width probability 
intervals across [0, 1]. Divergence between them reveals instability in specific probability ranges.

Points above the diagonal indicate the model over-predicts; points below indicate 
under-prediction. Both axes are on a log scale to spread out the low-probability 
range where most predictions cluster.

---

## Tab 2 — Predictions by Forecast Time Horizon and Hex Over Time

This tab examines the full time history of predictions for a specific location.

### Average map — Average P(outbreak)
Each parent hex is colored by its *mean* predicted outbreak probability averaged 
across **all dates and all forecast windows** in the evaluation period. This shows 
where the model consistently flags elevated risk. Orange-red outlines mark hexes 
that had at least one recorded outbreak at any point in the evaluation period.

### Parent Hex-Specific Forecast Time Series
After clicking a hex on the map, shows a time series of predicted P(outbreak) for 
that parent hex from the earliest to the most recent evaluation date. A separate 
colored line is drawn for each of the five forecast windows. The y-axis is log-scale; 
shaded background bands mark probability thresholds (0.001–0.01, 0.01–0.1, 0.1–0.5, >0.5). 
Vertical dashed lines mark dates when an outbreak was recorded. This view reveals 
seasonality and how different forecast windows track each other over time.

### Children Hex-Specific Forecast Time Series
Same as above but for the fine-resolution child hexes *within* the clicked parent. 
Line type distinguishes individual child hexes; color distinguishes forecast window. 
This reveals whether elevated risk at the parent-hex level is uniform across the area 
or concentrated in specific sub-locations.

---

## Tab 3 — Error Rates by Forecast Time Horizon

This tab examines model discrimination across the full distribution of predictions, 
both globally and for a selected hex and forecast window.

**Forecast interval slider**: Selects which forecast window feeds the hex-specific plot (lower right).

### Left map
Same average-probability map as Tab 2. Click a hex to populate the lower-right panel.

### Overall Predicted Outbreak Probability across Forecast Horizons
Violin plots showing the Africa-wide distribution of predicted outbreak probabilities 
for each forecast window (x-axis), split by observed outbreak status (Yes/No). 
The y-axis is log-scale. Better discrimination = greater separation between the 
"No" and "Yes" violins, especially at higher probability values. This view uses all 
dates and all hexes, so it shows aggregate model behavior rather than any specific 
location or time.

### Hex-Specific Predicted Outbreak Probability for a Forecast Horizon
After clicking a hex, shows a density plot (logit scale) of predictions for that 
hex across all dates in the evaluation period, for the forecast window selected by 
the slider. Colored by observed outbreak status. A well-performing hex would show 
the "Yes" distribution (outbreak recorded) shifted toward higher probabilities than 
the "No" distribution. Background shading marks probability-order-of-magnitude thresholds.

---

## Tab 4 — Variable Importance by Forecast Horizon

This tab shows which covariates drive predictions at each forecast horizon, 
using **SHAP values** ([SH]apley [A]dditive ex[P]lanations). SHAP decomposes each 
individual prediction into additive contributions from each input feature. A 
large |SHAP| value means that feature substantially shifted the prediction for 
that observation, regardless of direction. **Mean |SHAP|** averages these magnitudes 
across all observations.

The covariates fall into two groups:
- **Forecast covariates** (prefix `forecast`): ECMWF seasonal anomaly forecasts of 
precipitation, temperature, and relative humidity — the mean of a 51-member 
ensemble — averaged over each forecast window and matched to that window's lead time. 
Historical coverage is provided by hindcasts (the current forecasting method applied 
retrospectively).
- **Non-forecast covariates**: BioClim climate normals (1970–2000 averages), NDVI 
anomalies (deviation of vegetation greenness from the day-of-year historical mean), 
lagged weather and NDVI anomalies (averaged over the same five 30-day windows 
looking *backward*), land cover fractions, elevation, soil texture and drainage, livestock 
density (cattle, sheep, goats), and modeled host seroprevalence (`pred_sero` — see the 
Data Explorer guide for a full description).

**Forecast interval slider** selects which window to highlight in the line plots; 
heatmaps always show all five windows simultaneously.

### SHAP variable importance — raw values
Heatmap: top 20 non-forecast features (y-axis) × five forecast windows (x-axis). 
Fill color (viridis) = mean |SHAP|. Warmer/brighter color = greater average contribution 
to predictions at that horizon. Useful for identifying which features matter most and 
whether their importance is stable across lead times.

### SHAP variable importance — relative values
Same heatmap but fill = each feature's mean |SHAP| as a fraction of the total SHAP 
importance at that forecast interval. Removes absolute scale differences between 
intervals; shows which features dominate *relative to each other* at each horizon.

### SHAP — forecast covariates (line plot)
Mean |SHAP| (log scale) for the forecast-specific covariates across the five horizons. 
Shows how much the lead-time-matched climate anomaly forecasts contribute at each 
window — typically these increase in importance at longer horizons as static 
covariates become less differentiating.

### SHAP — non-forecast covariates (line plot)
Mean |SHAP| for the static and lagged covariates across forecast horizons. Since 
these covariates do not vary with forecast window, their importance trajectory 
reflects how much the model leans on baseline landscape/ecological context versus 
dynamic climate signals at each lead time.

---

## Cross-validation and evaluation structure

The model was trained on data up to a cut-off date (December 2020) and evaluated 
on held-out test data beyond that date. The evaluation follows a temporal 
expanding-window scheme: each outer fold trains on all data up to some point in 
time and tests on the subsequent ~150 days. The calibration curves and error 
diagnostics are based on these held-out test-period predictions.

**Date sampling**: The covariate stack is built using two randomly sampled dates 
per calendar month rather than daily data. This keeps the dataset manageable 
while still capturing seasonal variation. As a result, the time series in Tab 2 
are not evenly spaced — gaps between consecutive points correspond to roughly 
half-month intervals with some random variation.

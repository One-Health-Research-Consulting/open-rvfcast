# OpenRVFcast Forecast Explorer — User Guide

## What this app shows

The map shows the model's latest **forward forecasts** of Rift Valley fever outbreaks across Africa. Colour is the predicted probability that at least one outbreak (with cases) starts in the place during the chosen forecast window. Clicking a place shows three things: its forecast for every window, **how well the model's forecasts have scored there**, and its **outbreak history**.

There are two versions of the app:

| App | Places | Probability |
|---|---|---|
| Hexes | H3 hexes of about 12,500 km², the model's native units | The model's probability for the hex |
| Regions | Aggregated ADM2 regions of about 60,000 km² (districts merged within a country) | The probability of at least one outbreak anywhere in the region, combined from its hexes with a fitted Gaussian copula |

Regional probabilities are not averages of the hex probabilities. A region's chance of at least one outbreak is higher than any one of its hexes', but lower than if its hexes had outbreaks independently, because outbreaks in neighbouring hexes go together. The **Dependence bounds (max hex – independence)** column gives those two bounds: the largest single hex's probability (as if the region's hexes always had outbreaks together) and the probability if its hexes had outbreaks independently. It is not an uncertainty interval; a wide range means the regional probability depends a lot on how strongly the region's hexes move together.

---

## Controls

- **Forecast base date**: the date the forecasts were made from. The most recent is shown first.
- **Forecast window**: forecasts are made for five consecutive windows after the base date: days 1–29, 30–59, 60–89, 90–119 and 120–149.
- **Show forecast-quality mask**:
  - *faded* places: the model's skill there cannot be judged (no outbreaks in the back-test period);
  - *dashed white outline*: low forecast skill (see below).

The colour scale is logarithmic: each step is a multiplication, so differences between 0.01% and 0.1% are as visible as between 1% and 10%. Most places have very small probabilities; outbreaks are rare. The colour limits are the same as in the evaluation report's maps, so a colour means the same probability in every month.

---

## Forecast panel

- **P(outbreak)** for each window, for the chosen base date.
- **Rank**: where the place sits among all hexes (or regions) for that window. "Top 5%" means only 5% of places have a higher probability.
- **Dependence bounds (regions only)**: the lower and upper bounds described above (hover over the column title for a reminder).
- The plot shows the forecast across windows; the yellow ring is the window on the map. In the hex app the dashed line is the hex's typical (median) probability in the test period, to show whether the current forecast is unusually high for this place.

---

## Forecast-quality panel

Forecast quality comes from back-tests: forecasts the model made for past dates, compared with what happened. Each place is scored on its **log-loss gain over a flat forecast**: how much better the model's probabilities scored than a constant forecast equal to the overall outbreak rate. Positive is better than flat.

The back-tests used are the **outer folds** (models refitted on earlier data and used to forecast 2008–2020, before the test period) wherever the place has them, otherwise the **test period** (from 2021). Outer folds come first because they are evidence available *before* the period being forecast.

| Forecast quality | Meaning |
|---|---|
| **Demonstrated skill** | The back-tests included outbreaks here, and the model beat a flat forecast. |
| **Low Forecast Skill** | The back-tests included outbreaks here, but the model did not beat a flat forecast. Treat forecasts here with caution. |
| **No outbreaks in evaluation period** | No outbreaks happened here in the back-tests, so whether the model catches outbreaks here cannot be judged. This is most places, especially for hexes. |

The table adds:

- **Predictions scored** and **of which had an outbreak**: how much evidence the class rests on. A class built on two outbreak rows is weak evidence.
- **Skill rank**: 1 = the largest gain of all places.
- **Test-period gain**: the same score on the test period, for comparison.
- Hexes only:
  - **Timing skill (within-hex AUC)**: compares the hex's outbreak dates with its other dates in the test period. 0.5 means the model knew *where* outbreaks happen but not *when*.
  - **Gain over its own history**: whether the model beat the hex's own historical outbreak rate for that time of year.

---

## Outbreak-history panel

All labelled outbreaks (WAHIS reports with cases > 0) recorded in the place, by year. **# of times cases recorded** counts the distinct outbreak reports that year. Hexes count the outbreaks the model's labels place in the hex; regions count outbreaks by their reported coordinates.

The note under the summary says how many outbreaks started before the test period. Only those count as the "outbreak history" used in the forecast-quality context and in the evaluation report's maps.

---

## Where the data come from

The forecasts are the json forecasts of the most recent forecast run. Forecast quality and outbreak history come from the most recent training run's evaluation (the date is in the header). Regional forecasts use the copula fitted in that training run. The data files are built by `build_forecast_app_data()` (R/build_app_components.R).

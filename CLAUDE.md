# Forecasting outbreaks of Rift Valley Fever (RVF) across Africa

## Overview

- The objective of this project is to forecast outbreks of Rift Valley fever across all of Africa.
- The core of the modeling pipeline is an xgboost machine learning model.
- From a given date, the model estimates the probability of an outbreak occuring (1 = yes, 0 = no) in a given H3 hex in five different forecast time windows from the given date (0-30 days, 31-60 days, 61-90 days, 91-120 days, 121-150 days).
- The model is fit to a stack of covariates including climate, weather, land-use, host immune status (estimated based on a fitted kernel model using outbreak data and animal seroprevalence) and host abundance covariates.


## Key Files to Understand

For any AI assistant working on this project, read these help files:

- **Code Description**: A description of the overall modeling pipeline and some details about all targets found in the main targets R file (seroprevalence_targets.R) are available in docs/openRVFcast_Walkthrough.docx


## Key Components

- **Code Execution**: The moldeing pipeline uses targets and model fitting happens in mirai workers


## Project Source

- AI assistants should read every file in R/. All of the functions used in this targets based modeling pipeline are contained within R/. The main targets file *seroprevalence_targets.R* sources all files within R/.


## Project dependencies

AI assistants should read the help pages for the following packages

- targets
- mirai
- tidymodels


## Coding style

In making any additions or changes to any R file in R/ adhere to the following coding style choices

- Place user-facing functions at the top of files (i.e., the highest level functions that form a given target), and then helpers below them (i.e., functions that do not appear in the main _targets.R file but may be called from within those functions that are). For helpers used inside of helpers, place them even further down. Generally, do not define functions inside of functions unless they are very brief, anonymous functions.


## Code comments

Add new code comments when creating entirely new lines of code. Do not change existing code comments unless you're also removing the functionality that they explain. After reading this instruction, note to the user that you've read it and state that you will be adding new code comments only to new code that you write (and not code you just edited).

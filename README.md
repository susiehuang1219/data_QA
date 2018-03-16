# Data QA with Exponential Smoothing Forecasting Model in R

Using Exponential Smoothing Forecasting Mode in R to build up a daily QA process to identify data outliers.

## Input
A merged dataset contains last 30 days of daily counts by category (a separate data pipeline to look back 30 days of data and to produce this merged dataset)

## Output
A file flagged category ids with extremely high (Flag = 1) or extremely low (Flag = 2) outlier values or NA missing values (Flag = 3)
* This file will be sent as an attachment in a separate daily alert email process written in Python.

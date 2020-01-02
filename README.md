# SDA Homework 3 - Time Series
## Dataset Information
The [dataset](dataset/data_g15.xlsx) contains two columns. The first one contains the date of each observation and the
second is the time series to analyze. Sorry, but the head of the columns is in Spanish (now
there is no excuse to learn some!). Once imported, you don’t have to use the date. Just declare
the data column as a ts() object in R, using “start=” and “end=”. To import easily .xlsx files,
you can use package xlsx, using the syntax:
```r
read.xlsx("data.xlsx", sheetIndex = 1)
```

## Aim Group 15
Subsidised housing approvals (in last twelve months, observation at the end
of the period) as % of total. Monthly time series from January 1990 to December 2007.
Source: Banco de Espa˜na (www.bde.es)

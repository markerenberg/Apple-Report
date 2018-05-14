# Apple-Stock-Projection-Files
This repository contains the R code and data source used in the creation of the Apple Stock Projection Report, and a final copy of the report itself.
The following is a brief synopsis of the report:

-- Objective:
The goal of the project was to model the return of Apple stock on a given day using current and past market information.

-- Methodology:
Autoregressive models were considered using automated model selection methods, including stepwise regression, forwards elimination, and 
backwards elimination. 
For our data set, each observation of data corresponds to information that was recorded on a respective weekday dating from April 14th, 2003, up until September 14th, 2015.
The explanatory variables considered in the model include the day’s date, the Apple stock quote, the S&P 500 Index value, 
the CBOE Volatility Index value, the S&P Goldman Sachs Commodity Index value, the Dow Jones Barclays Capital Bond Index value, and the
Morgan Stanley Emerging Markets Index value, which are referred to in the report as DATE, PRICE, SPX, VIX, SPGSCITR, BNDLGB, and EEM
respectively .
In the end, model diagnostics revealed that a linear regresison model that included the price, historic price, volatility index, and 
commodity index variables was the best fitted model to predict the return of Apple stock.

-- Authors: 
This report is an academic report that was created for the University of Waterloo. All published content is the propery of its original authors,
as denoted in the report itself.

For any questions or inquiries, I can be reached at mkerenbe@uwaterloo.ca. 


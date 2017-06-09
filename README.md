# **normalweatherr**

[![Build Status](https://travis-ci.org/skgrange/normalweatherr.svg?branch=master)](https://travis-ci.org/skgrange/normalweatherr)

![](inst/extdata/images/icon_small.png)

## Introduction

**normalweatherr** is an R package to conduct meteorological/weather normalisation on air quality so trends and interventions can be investigated in a robust way. 

## Steps for usage

  1. Gain a mostly complete time-series of a numeric value with other variables which could explain variation of the numeric value. In the air quality domain, `value` will be pollutant concentration and the other variables will be meteorological variables such as wind speed, wind direction, atmospheric temperature, atmospheric relative humidity, and atmospheric pressure. However other variables could be used which might explain pollutant concentrations. 

  2. A statistical model is built which uses meteorological and time variables to explain pollutant concentrations. Any statistical model which can represent interactions and non-linear relationships among the predictors could be used. But here, random forest, support vector machines, and generalised boosted regression models are used. 
  
  3. Randomly allocate the meteorological and time variables (excluding the trend component) used in the model to predict concentrations based on these sampled variables. When this process is repeated hundreds of times and then aggregated, the result is a trend which represents "average" meteorology/weather.
  
  4. Continue on with appropriate trend analysis. Breakpoints, Theil-Sen estimations, etc...

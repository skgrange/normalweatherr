# **normalweatherr**

[![Build Status](https://travis-ci.org/skgrange/normalweatherr.svg?branch=master)](https://travis-ci.org/skgrange/normalweatherr)

![](inst/extdata/images/icon_small.png)

## Introduction

**normalweatherr** is an R package to conduct meteorological/weather normalisation on air quality so trends and interventions can be investigated in a robust way. 

## Usage steps 

  1. A statistical model is built which uses meteorological and time variables to explain hourly pollutant concentrations. Any statistical model which can represent interactions and non-linear relationships among the predictors could be used, but here, random forest and support vector machines are used. 
  
  2. Randomly allocate the meteorological and time variables used in the model to predict concentrations based on these sampled variables. When this process is repeated hundreds of times and then aggregated, the result is a trend which represents "average" meteorology/weather.
  
  3. Continue on with appropriate trend analysis. Breakpoints, etc...

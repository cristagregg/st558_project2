ST558 Project 2
================
Crista Gregg and Halid Kopanski
7/2/2021

-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
    -   [Rentals by Year](#rentals-by-year)
    -   [Rentals by Weather](#rentals-by-weather)
    -   [Casual vs. Registered bikers](#casual-vs-registered-bikers)
    -   [Average bikers by month](#average-bikers-by-month)
    -   [Holiday and Temp/Hum data](#holiday-and-temphum-data)
    -   [Correlation among numeric
        predictors](#correlation-among-numeric-predictors)
-   [Modeling](#modeling)
    -   [Linear Regression](#linear-regression)
        -   [Linear Fit 1](#linear-fit-1)
        -   [Linear Fit 2](#linear-fit-2)
    -   [Ensemble Tree](#ensemble-tree)
        -   [Random Forests](#random-forests)
        -   [Boosting Model](#boosting-model)
-   [Comparison](#comparison)

# Introduction

The following analysis breaks down bicycle sharing usage based on data
gathered for every recorded Sunday in the years 2011 and 2012. The data
was gathered from users of Capitol Bikeshare based in Washington DC. In
total, the dataset contains 731 entries. For each entry, 16 variables
were recorded. The following is the list of the 16 variables and a short
description of each:

| Variable   |                                                    Description                                                    |
|------------|:-----------------------------------------------------------------------------------------------------------------:|
| instant    |                                                   record index                                                    |
| dteday     |                                                       date                                                        |
| season     |                                       season (winter, spring, summer, fall)                                       |
| yr         |                                                 year (2011, 2012)                                                 |
| mnth       |                                                 month of the year                                                 |
| holiday    |                            whether day is holiday or not (extracted from \[Web Link\])                            |
| weekday    |                                                  day of the week                                                  |
| workingday |                            if day is neither weekend nor holiday is 1, otherwise is 0.                            |
| weathersit |                                                                                                                   |
| \-         |                                1: Clear, Few clouds, Partly cloudy, Partly cloudy                                 |
| \-         |                          2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist                          |
| \-         |            3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds             |
| \-         |                           4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog                           |
| temp       |                                        Normalized temperature in Celsius.                                         |
| atemp      |                                   Normalized perceived temperature in Celsius.                                    |
| hum        |                                               Normalized humidity.                                                |
| windspeed  |                                              Normalized wind speed.                                               |
| casual     |                                               count of casual users                                               |
| registered |                                             count of registered users                                             |
| cnt        |                                      sum of both casual and registered users                                      |
| *Sources*  | *Raw data and more information can be found [here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset)* |

In addition to summary statistics, this report will also model bicycle
users linear regression, random forests, and boosting. The model will
help determine anticipated number of users based on readily available
data. To achieve this, the response variables are casual, registered,
and cnt. The other 13 variables will be the predictors for models
developed later in this report.

# Data

``` r
set.seed(1)
bikes <- read_csv('day.csv')

day_function <- function(x){
  x <- as.character(x)
  switch(x, "0" = "Sunday", 
         "1" = "Monday", 
         "2" = "Tuesday", 
         "3" = "Wednesday", 
         "4" = "Thursday", 
         "5" = "Friday", 
         "6" = "Saturday")
}

season_function <- function(x){
    x <- as.character(x)
    switch(x, "1" = "Spring",
              "2" = "Summer",
              "3" = "Fall",
              "4" = "Winter")
}

bikes <- bikes %>% select(everything()) %>% 
  mutate(weekday = sapply(weekday, day_function), 
         season = sapply(season, season_function)) 
  

bikes$season <- as.factor(bikes$season)
bikes$yr <- as.factor(bikes$yr)
levels(bikes$yr) <- c('2019','2020')
bikes$mnth <- as.factor(bikes$mnth)
bikes$holiday <- as.factor(bikes$holiday)
bikes$weekday <- as.factor(bikes$weekday)
bikes$workingday <- as.factor(bikes$workingday)
bikes$weathersit <- as.factor(bikes$weathersit)

day <- params$day_of_week

#filter bikes by day of week
bikes <- filter(bikes, weekday == day)

#split data into train and test sets
train_rows <- sample(nrow(bikes), 0.7*nrow(bikes))
train <- bikes[train_rows,] %>% 
  select(-instant, -weekday, -casual, -registered)
test <- bikes[-train_rows,] %>% 
  select(-instant, -weekday, -casual, -registered)
```

# Summarizations

## Rentals by Year

The following table tells us the total number of rentals for each of the
two years of collected data, as well as the average number of rentals
per day.

``` r
bikes %>%
  group_by(yr) %>%
  summarise(total_rentals = sum(cnt), avg_rentals = round(mean(cnt)))
```

    ## # A tibble: 2 x 3
    ##   yr    total_rentals avg_rentals
    ##   <fct>         <dbl>       <dbl>
    ## 1 2019         177074        3405
    ## 2 2020         266953        5037

## Rentals by Weather

The following box plot shows us how many rentals we have for days that
are sunny or partly cloudy, misty, or rainy/snowy. We may expect some
differences in behavior between weekend days where less people might be
inclined to ride their bikes for pleasure, versus weekdays when more
people might brave moderately unpleasant weather to get to work.

``` r
ggplot(bikes, aes(factor(weathersit), cnt)) +
  geom_boxplot() +
  labs(x = 'Type of Weather', y = 'Number of Rental Bikes', title = 'Rental Bikes by Type of Weather') +
  scale_x_discrete(labels = c('Clear to some clouds', 'Misty', 'Light snow or rain')) +
  theme_minimal()
```

![](Report-Sunday_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Casual vs. Registered bikers

Below is a chart of the relationship between casual and registered
bikers. We might expect a change in the slope if we look at different
days of the week. Perhaps we see more registered bikers riding on the
weekday but more casual users on the weekend.

``` r
ggplot(bikes, aes(casual, registered)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal() +
  labs(title = 'Registered versus Casual Renters')
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Report-Sunday_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Average bikers by month

Below we see a plot of the average daily number of bikers by month. We
should expect to see more bikers in the spring and summer months, and
the least in the winter.

``` r
plot_mth <- bikes %>%
  group_by(mnth) %>%
  summarize(avg_bikers = mean(cnt))

ggplot(plot_mth, aes(mnth, avg_bikers)) +
  geom_line(group = 1, color = 'darkblue', size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title='Average daily number of bikers by month', y = 'Average Daily Bikers', x = 'Month') +
  scale_x_discrete(labels = month.abb)
```

![](Report-Sunday_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Holiday and Temp/Hum data

We would like to see what affect public holidays have on the types of
bicycle users for a given day. In this case, Sunday show the following
relationships:

``` r
bikes %>% ggplot(aes(x = as.factor(workingday), y = casual)) + geom_boxplot() + 
                labs(title = paste("Casual Users on", params$day_of_week)) + 
                xlab("") + 
                ylab("Casual Users") + 
                scale_x_discrete(labels = c('Public Holiday', 'Workday')) + 
                theme_minimal()
```

![](Report-Sunday_files/figure-gfm/holiday-1.png)<!-- -->

``` r
bikes %>% ggplot(aes(x = as.factor(workingday), y = registered)) + geom_boxplot() + 
                labs(title = paste("Registered Users on", params$day_of_week)) + 
                xlab("") + 
                ylab("Registered Users") +
                scale_x_discrete(labels = c('Public Holiday', 'Workday')) +
                theme_minimal()
```

![](Report-Sunday_files/figure-gfm/holiday-2.png)<!-- -->

Temperature and Humidity have an affect on the number of users on a
given day

First, normalized temperature data (both actual temperature and
perceived):

``` r
bike_temp <- bikes %>% select(cnt, temp, atemp) %>% 
                    gather(key = type, value = temp_norm, temp, atemp, factor_key = FALSE)

ggplot(bike_temp, aes(x = temp_norm, y = cnt, col = type, shape = type)) + 
        geom_point() + geom_smooth(formula = 'y ~ x', method = 'loess') +
        scale_color_discrete(name = "Temp Type", labels = c("Perceived", "Actual")) +
        scale_shape_discrete(name = "Temp Type", labels = c("Perceived", "Actual")) +
        labs(title = paste("Temperature on", params$day_of_week, "(Actual and Perceived)")) +
        xlab("Normalized Temperatures") +
        ylab("Total Users") + 
        theme_minimal()
```

![](Report-Sunday_files/figure-gfm/temp_hum-1.png)<!-- -->

Next the affect of humidity:

``` r
bikes%>% ggplot(aes(x = hum, y = cnt)) + geom_point() + geom_smooth(formula = 'y ~ x', method = 'loess') +
                labs(title = paste("Humidity versus Total Users on", params$day_of_week)) +
                xlab("Humidity (normalized)") +
                ylab("Total Number of Users") +
                theme_minimal()
```

![](Report-Sunday_files/figure-gfm/hum-1.png)<!-- -->

## Correlation among numeric predictors

Here we are checking the correlation between the numeric predictors in
the data

``` r
knitr::kable(round(cor(bikes[ , c(11:16)]), 3))
```

|            |  atemp |    hum | windspeed | casual | registered |    cnt |
|:-----------|-------:|-------:|----------:|-------:|-----------:|-------:|
| atemp      |  1.000 |  0.235 |    -0.230 |  0.729 |      0.587 |  0.685 |
| hum        |  0.235 |  1.000 |    -0.274 |  0.050 |      0.007 |  0.026 |
| windspeed  | -0.230 | -0.274 |     1.000 | -0.231 |     -0.273 | -0.272 |
| casual     |  0.729 |  0.050 |    -0.231 |  1.000 |      0.764 |  0.914 |
| registered |  0.587 |  0.007 |    -0.273 |  0.764 |      1.000 |  0.960 |
| cnt        |  0.685 |  0.026 |    -0.272 |  0.914 |      0.960 |  1.000 |

``` r
corrplot(cor(bikes[ , c(11:16)]), method = "circle")
```

![](Report-Sunday_files/figure-gfm/correlation-1.png)<!-- -->

# Modeling

## Linear Regression

Linear regression is one of the most common methods for modeling. It
looks at a set of predictors and estimates what will happen to the
response if the predictors change. This model is highly interpretable,
as it shows us the effect of each individual predictor as well as
interactions. We can see if the change in the response goes up or down
and in what quantity. Below we fit two linear regression models.

### Linear Fit 1

The first model will have a subset of predictors chosen by stepwise
selection.

``` r
set.seed(10)
lm.fit <- train(cnt ~ season + yr + weathersit + atemp + hum + windspeed, data = train[, 2:12], method = 'lm',
                preProcess = c('center', 'scale'),
                trControl = trainControl(method = 'cv', number = 10))
```

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, : These variables have zero variances: weathersit3

    ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient fit may be misleading

``` r
lm.fit
```

    ## Linear Regression 
    ## 
    ## 73 samples
    ##  6 predictor
    ## 
    ## Pre-processing: centered (9), scaled (9) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 65, 65, 66, 65, 66, 66, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared  MAE     
    ##   1026.189  0.69611   797.5713
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

Our first linear model has an R<sup>2</sup> of 1026.1885563.

### Linear Fit 2

Using transformations of predictors identified in the first linear fit
and dropping the year predictor (in an effort to forecast future usage).

``` r
set.seed(10)
lm.fit1 <- train(cnt ~ season + weathersit + poly(hum, 3) + I(atemp^0.5) + windspeed, data = train[, 2:12], method = 'lm',
                preProcess = c('center', 'scale'),
                trControl = trainControl(method = 'cv', number = 10))
lm.fit1
```

    ## Linear Regression 
    ## 
    ## 73 samples
    ##  5 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 65, 65, 66, 65, 66, 66, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   1226.021  0.6135773  1015.585
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

The R<sup>2</sup> value of the model changed to 1226.0214354

## Ensemble Tree

Ensemble trees methods come in many types and are very versatile when it
comes to regression or classification. For the following we will be
using the two most common and well known methods, Random Forests (a form
of bagging) and Boosting. Both these tree based methods involve
optimization during the development process. In the case of random
forests, the optimization involves varying the number of predictors
used. This is done to mitigate the effects of one or more predictors
from overshadowing other predictors. Boosting is method where the final
model is developed through an iterative combination of weaker models
where each interation builds upon the last. While both methods are very
flexible and tend to process good results the models themselves are as
interpretable as linear regression. We normally just analyze the output
of the models.

### Random Forests

### Boosting Model

The following are the results of Boosting model development using the
provided bike data

``` r
trctrl <- trainControl(method = "repeatedcv", 
                       number = 10, 
                       repeats = 3)

set.seed(2020)

boost_grid <- expand.grid(n.trees = c(20, 100, 500),
                          interaction.depth = c(1, 3, 5),
                          shrinkage = c(0.1, 0.01, 0.001),
                          n.minobsinnode = 10)

boost_fit <-  train(cnt ~ ., 
                    data = select(train, cnt, hum, temp, atemp, windspeed, workingday, season, weathersit), 
                    method = "gbm", 
                    verbose = F, #suppresses excessive printing while model is training
                    trControl = trctrl, 
                    tuneGrid = data.frame(boost_grid))
```

A total of 27 models were evaluated. Each differing by the combination
of boosting parameters. The results are show below:

``` r
print(boost_fit)
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 73 samples
    ##  7 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 66, 65, 65, 65, 65, 65, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   shrinkage  interaction.depth  n.trees  RMSE      Rsquared   MAE      
    ##   0.001      1                   20      1840.274  0.5731700  1492.9448
    ##   0.001      1                  100      1771.255  0.5762571  1435.0782
    ##   0.001      1                  500      1530.326  0.5761193  1246.5530
    ##   0.001      3                   20      1839.035  0.6212258  1491.8567
    ##   0.001      3                  100      1765.472  0.6189569  1429.9138
    ##   0.001      3                  500      1502.440  0.6241954  1222.8622
    ##   0.001      5                   20      1839.270  0.6136201  1492.1937
    ##   0.001      5                  100      1766.023  0.6148733  1430.3663
    ##   0.001      5                  500      1501.900  0.6250300  1221.8237
    ##   0.010      1                   20      1697.665  0.5633687  1376.5952
    ##   0.010      1                  100      1389.918  0.5779336  1159.1266
    ##   0.010      1                  500      1222.832  0.6190666  1073.5005
    ##   0.010      3                   20      1687.141  0.6102919  1365.4401
    ##   0.010      3                  100      1337.045  0.6301040  1112.6265
    ##   0.010      3                  500      1164.931  0.6475974  1001.5715
    ##   0.010      5                   20      1687.537  0.6066940  1365.5793
    ##   0.010      5                  100      1336.163  0.6324267  1113.4914
    ##   0.010      5                  500      1161.679  0.6552263  1001.3497
    ##   0.100      1                   20      1283.582  0.5974780  1098.4953
    ##   0.100      1                  100      1177.186  0.6389430  1027.2645
    ##   0.100      1                  500      1257.072  0.5983659  1064.7413
    ##   0.100      3                   20      1252.418  0.6159295  1084.2624
    ##   0.100      3                  100      1208.206  0.6246697  1017.3552
    ##   0.100      3                  500      1307.798  0.5706488  1076.9113
    ##   0.100      5                   20      1232.841  0.6328954  1066.7053
    ##   0.100      5                  100      1174.029  0.6361333   988.6317
    ##   0.100      5                  500      1303.932  0.5701083  1082.0755
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 500, interaction.depth = 5, shrinkage = 0.01 and n.minobsinnode = 10.

``` r
results_tab <- as_tibble(boost_fit$results[,c(1,2,4:6)])
```

The attributes of the best model is shown here.

``` r
boost_min <- which.min(results_tab$RMSE)

knitr::kable(results_tab[boost_min,])
```

| shrinkage | interaction.depth | n.trees |     RMSE |  Rsquared |
|----------:|------------------:|--------:|---------:|----------:|
|      0.01 |                 5 |     500 | 1161.679 | 0.6552263 |

# Comparison

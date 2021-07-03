ST558 Project 2
================
Crista Gregg, Halid Kopanski, Dionte Watie
7/2/2021

-   [Introduction](#introduction)
-   [Data](#data)
-   [Summarizations](#summarizations)
    -   [Contributions from Crista](#contributions-from-crista)
    -   [Holiday and Temp/Hum data](#holiday-and-temphum-data)
-   [Modeling](#modeling)
    -   [Linear Regression](#linear-regression)
    -   [Ensemble Tree](#ensemble-tree)
-   [Modeling](#modeling-1)
-   [Comparison](#comparison)

# Introduction

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

day <- params$day_of_week #will need to automate this

#filter bikes by day of week
bikes <- filter(bikes, weekday == day)
train_rows <- sample(nrow(bikes), 0.7*nrow(bikes))
train <- bikes[train_rows,]
test <- bikes[-train_rows,]
```

# Summarizations

## Contributions from Crista

The following table tells us the total number of rentals for each of the
two years of collected data, as well as the average number of rentals
per day.

``` r
bikes %>%
  group_by(yr) %>%
  summarise(total_rentals = sum(cnt), avg_rentals = round(mean(cnt)))
```

    ## # A tibble: 2 x 3
    ##      yr total_rentals avg_rentals
    ##   <dbl>         <dbl>       <dbl>
    ## 1     0        180221        3466
    ## 2     1        275282        5194

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

![](ST558-Project-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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

![](ST558-Project-2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Below we see a plot of the average daily number of bikers by month. We
should expect to see more bikers in the spring and summer months, and
the least in the winter.

``` r
plot_mth <- bikes %>%
  group_by(mnth) %>%
  summarize(avg_bikers = mean(cnt))

ggplot(plot_mth, aes(mnth, avg_bikers)) +
  geom_line(color = 'darkblue', size = 1.6) +
  theme_minimal() +
  labs(title='Average daily number of bikers by month', y = 'Average Daily Bikers', x = 'Month') +
  scale_x_discrete(limits = 1:12, labels = month.abb)
```

    ## Warning: Continuous limits supplied to discrete scale.
    ## Did you mean `limits = factor(...)` or `scale_*_continuous()`?

![](ST558-Project-2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Holiday and Temp/Hum data

We would like to see what affect public holidays have on the types of
bicycle users for a given day. In this case, Monday show the following
relationships:

``` r
bikes %>% ggplot(aes(x = as.factor(workingday), y = casual)) + geom_boxplot() + 
                labs(title = paste("Casual Users on", params$day_of_week)) + 
                xlab("") + 
                ylab("Casual Users") + 
                scale_x_discrete(labels = c('Public Holiday', 'Workday')) + 
                theme_minimal()
```

![](ST558-Project-2_files/figure-gfm/holiday-1.png)<!-- -->

``` r
bikes %>% ggplot(aes(x = as.factor(workingday), y = registered)) + geom_boxplot() + 
                labs(title = paste("Registered Users on", params$day_of_week)) + 
                xlab("") + 
                ylab("Registered Users") +
                scale_x_discrete(labels = c('Public Holiday', 'Workday')) +
                theme_minimal()
```

![](ST558-Project-2_files/figure-gfm/holiday-2.png)<!-- -->

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

![](ST558-Project-2_files/figure-gfm/temp_hum-1.png)<!-- -->

Next the affect of humidity:

``` r
bikes%>% ggplot(aes(x = hum, y = cnt)) + geom_point() + geom_smooth() +
                labs(title = paste("Humidity versus Total Users on", params$day_of_week)) +
                xlab("Humidity (normalized)") +
                ylab("Total Number of Users") +
                theme_minimal()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](ST558-Project-2_files/figure-gfm/hum-1.png)<!-- -->

# Modeling

## Linear Regression

## Ensemble Tree

# Modeling

# Comparison

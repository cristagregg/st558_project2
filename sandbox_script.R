library(tidyverse)
library(caret)

bike_day <- read_csv("day.csv")
#bike_hour <- read_csv("hour.csv")

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

bikes <- bikes %>% select(everything()) %>% mutate(weekday = sapply(weekday, day_function))

day <- params$day_of_week #will need to automate this


day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for(i in day_list){
  rmarkdown::render("<file.Rmd>", params = list(day_of_week = i))
}
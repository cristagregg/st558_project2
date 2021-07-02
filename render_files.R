day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for(i in day_list){
  rmarkdown::render("<file.Rmd>", params = list(day_of_week = i))
}
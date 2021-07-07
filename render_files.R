day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
#day_list <- c("Sunday", "Monday")

for(i in day_list){
  rmarkdown::render("ST558 Project 2.Rmd", params = list(day_of_week = i),
                    output_file = paste0("Report-", i))
}

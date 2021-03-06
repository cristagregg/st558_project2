### Crista Gregg and Halid Kopanski  

This project analyzes the number of bicycle rentals from the Capital Bikeshare system over 2011 and 2012. We are attempting to predict the number of users by different variables such as weather type, temperature, humidity, etc. This will be done for each day of the week.   

This project requires the tidyverse, caret, knitr, and corrplot packages. 

All reports are generated from a single Rmd file using the following code:
```
day_list <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

for(i in day_list){
  rmarkdown::render("ST558 Project 2.Rmd", params = list(day_of_week = i),
                    output_file = paste0("Report-", i))
}
```

Below are the links to each day's report:  
  
The analysis for [Monday is available here](Report-Monday.md).  
The analysis for [Tuesday is available here](Report-Tuesday.md).  
The analysis for [Wednesday is available here](Report-Wednesday.md).  
The analysis for [Thursday is available here](Report-Thursday.md).  
The analysis for [Friday is available here](Report-Friday.md).  
The analysis for [Saturday is available here](Report-Saturday.md).  
The analysis for [Sunday is available here](Report-Sunday.md).  

The code to generate the above reports is located [here](render_files.R).

library(rmarkdown)

#Get the data
source("Google_Sheets.R")
source("Indiana_Data.R")
source("Testing_Data.R")

this_day = Sys.Date()-2
save(this_day, file = "this_day.Rdata")

#The COVID Dashboard
fname = paste("G:/Shared drives/Dashboard - COVID-19/Grace internal ",
              format(this_day), ".pdf", sep = "")
#fname = paste("G:/Shared drives/Dashboard - COVID-19/Grace internal ",
#              "2021-01-25", ".pdf", sep = "")
render("Grace_internal4.Rmd", output_file = fname)

source("Testing_Double_Check.R")
source("Tables.R")

render("index.Rmd", output_file = "index.html")

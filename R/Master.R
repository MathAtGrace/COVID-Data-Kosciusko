library(rmarkdown)

this_day = Sys.Date()
save(this_day, file = "data/this_day.Rdata")

#Get the data
source("R/Google_Sheets.R")
source("R/clean_contact_tracing_data.R")
source("R/Indiana_Data.R")

#The COVID Dashboard
fname = paste("G:/Shared drives/Dashboard - COVID-19/Grace internal ",
              format(this_day), ".pdf", sep = "")
render("daily_report.Rmd", output_file = fname)
render("daily_report.Rmd", output_file = "daily_report.pdf")
render("website/Central_Europe.Rmd", output_file = "../website/Central_Europe.html")

source("R/Tables.R")


#source("Testing_Double_Check.R")

#render("index.Rmd", output_file = "index.html")

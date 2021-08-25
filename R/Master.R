library(rmarkdown)

#Get the data
source("R/Google_Sheets.R")
source("R/clean_contact_tracing_data.R")
source("R/Indiana_Data.R")

this_day = Sys.Date()
save(this_day, file = "this_day.Rdata")

#The COVID Dashboard
fname = paste("G:/Shared drives/Dashboard - COVID-19/Grace internal ",
              format(this_day), ".pdf", sep = "")
render("Grace_internal4.Rmd", output_file = fname)

source("Tables.R")


#source("Testing_Double_Check.R")

render("index.Rmd", output_file = "index.html")

library(rmarkdown)

#Get the data
source("Google_Sheets.R")
source("Indiana_Data.R")
source("Test_Data.R")

#The COVID Dashboard
fname = paste("G:/Shared drives/Dashboard - COVID-19/Grace internal ",
              format(Sys.Date()), ".pdf", sep = "")
render("Grace_internal4.Rmd", output_file = fname)

render("index.Rmd", output_file = "index.html")

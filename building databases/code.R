library(readxl)

# data <- read_xls('https/minfin.am/website/images/files/hamaxmbvac.xls')

library(gdata)

download.file('https://minfin.am/website/images/files/hamaxmbvac.xls', destfile = 'file.xls')

data <- read.xls('file.xls')
data <- read.xls('hamaxmbvac.xls')

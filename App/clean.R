library(readxl)

export_import <- read_xlsx("export_import.xlsx")
View(export_import)

export_import <- export_import %>% 
  select(-"ապրանքներ")

colnames(export_import) <- c("Commodity","Codes",
                             "year","Time Period","export in tonn",
                             "export in 1000$", "import in tonn",
                             "import in 1000$")
sapply(export_import, class)



### Quarter

quarter_abs <- read_xlsx("quarter.xlsx",sheet = 1)
quarter_growth <- read_xlsx("quarter.xlsx",sheet = 2)

colnames(quarter_abs)[c(1,2)] = c("Year", "Quarter")

for (i in 1:length(quarter_abs$Year)) {
  if(is.na(quarter_abs$Year[i])) {
    quarter_abs$Year[i] = j
  }
  j = quarter_abs$Year[i]
}


colnames(quarter_growth)[c(1,2)] = c("Year", "Quarter")

for (i in 1:length(quarter_growth$Year)) {
  if(is.na(quarter_growth$Year[i])) {
    quarter_growth$Year[i] = j
  }
  j = quarter_growth$Year[i]
}


yearly_abs <- read_xlsx("yearly.xlsx", sheet = 1)
yearly_growth <- read_xlsx("yearly.xlsx", sheet = 2)

colnames(yearly_abs)[1] <- "Year"
colnames(yearly_growth)[1] <- "Year"

library(writexl)

write_xlsx(export_import,"export_import.xlsx")
write_xlsx(quarter_abs,"quarter_abs.xlsx")
write_xlsx(quarter_growth,"quarter_growth.xlsx")
write_xlsx(yearly_abs,"yearly_abs.xlsx")
write_xlsx(yearly_growth,"yearly_growth.xlsx")



df <- readxl::read_xlsx("full_data.xlsx")



df_2014<- df[,c(1,2,grep("2014",colnames(df)))]
df_2015<- df[,c(1,2,grep("2015",colnames(df)))]
df_2016<- df[,c(1,2,grep("2016",colnames(df)))]
df_2017<- df[,c(1,2,grep("2017",colnames(df)))]
df_2018<- df[,c(1,2,grep("2018",colnames(df)))]

writexl::write_xlsx(df_2014,"df_2014.xlsx")
writexl::write_xlsx(df_2015,"df_2015.xlsx")
writexl::write_xlsx(df_2016,"df_2016.xlsx")
writexl::write_xlsx(df_2017,"df_2017.xlsx")
writexl::write_xlsx(df_2018,"df_2018.xlsx")



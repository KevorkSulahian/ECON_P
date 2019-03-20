df <- readxl::read_xlsx("each_month.xlsx")
main <- readxl::read_xlsx("main.xlsx")

df <- merge(df[-c(1)], main,by.x = "ID", by.y = "X__1")

df <- df[c(8,1,2,3,4,5,6,7)]

agg2 <- df

colnames(agg2) <- c("Կենդանի կենդանիներ և կենդանական ծագման արտադրանք",
                    "id"                                              
                    ,"year","period",                                
                    "Export_in_tonnas" ,"Export",                                         
                    "Import_in_tonnas","Import")
agg2$year <- as.character(agg2$year)
agg2$period <- as.character(agg2$period)
agg3 <- agg2

# Export_in_tonnas
agg2 <- agg3[c(1:4,5)]
pls <- expand.grid(id = unique(agg2$id), period = c(as.character(unique(agg2$period)))) %>%
  mutate(period = as.character(period)) %>%
  left_join(agg2, by = c('id', 'period')) %>%
  spread(key = period, value = c(Export_in_tonnas), fill = 0)

complete <- NULL

for (i in (colnames(pls[-c(1,2,3)]))) {
  name <- paste0("Export_in_tonnas ", i)
  complete <- append(complete,name)
}
colnames(pls) <-c(colnames(pls[c(1:3)]), complete)

# Export
agg2 <- agg3[c(1:4,6)]
pls2 <- expand.grid(id = unique(agg2$id), period = c(as.character(unique(agg2$period)))) %>%
  mutate(period = as.character(period)) %>%
  left_join(agg2, by = c('id', 'period')) %>%
  spread(key = period, value = c(Export), fill = 0)

complete <- NULL

for (i in (colnames(pls2[-c(1,2,3)]))) {
  name <- paste0("Export ", i)
  complete <- append(complete,name)
}
colnames(pls2) <-c(colnames(pls2[c(1:3)]), complete)


# Import_in_tonnas
agg2 <- agg3[c(1:4,7)]
pls3 <- expand.grid(id = unique(agg2$id), period = c(as.character(unique(agg2$period)))) %>%
  mutate(period = as.character(period)) %>%
  left_join(agg2, by = c('id', 'period')) %>%
  spread(key = period, value = c(Import_in_tonnas), fill = 0)

complete <- NULL

for (i in (colnames(pls3[-c(1,2,3)]))) {
  name <- paste0("Import_in_tonnas ", i)
  complete <- append(complete,name)
}
colnames(pls3) <-c(colnames(pls3[c(1:3)]), complete)


# Import
agg2 <- agg3[c(1:4,8)]
pls4 <- expand.grid(id = unique(agg2$id), period = c(as.character(unique(agg2$period)))) %>%
  mutate(period = as.character(period)) %>%
  left_join(agg2, by = c('id', 'period')) %>%
  spread(key = period, value = c(Import), fill = 0)

complete <- NULL

for (i in (colnames(pls4[-c(1,2,3)]))) {
  name <- paste0("import ", i)
  complete <- append(complete,name)
}
colnames(pls4) <-c(colnames(pls4[c(1:3)]), complete)



# bind bitches

complete_data <- merge(pls,pls2[-2], by = c("id", "year"))
complete_data <- merge(complete_data,pls3[-2], by = c("id", "year"))
complete_data <- merge(complete_data,pls4[-2], by = c("id", "year"))

complete_data <- complete_data[complete.cases(complete_data),]


complete_data %>% 
  gather(variable, value, -(month:student)) %>%
  unite(temp, student, variable) %>%
  spread(temp, value)


complete_data %>% spread(year, complete_data[-c(1:3)])






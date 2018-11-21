library(dplyr)

data <- read.csv("data.csv")

# Step 1 - fixing and understanding the data

sapply(data, class)

# either factor or numeric


colnames(data) <- c("ID", "Country", "Indicator", "Type", "2007-2008", "2008-2009","2009-2010","2010-2011",
                    "2011-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018")
data$ID <- as.character(data$ID)
data$Country <- as.character(data$Country)
data$Indicator <- as.character(data$Indicator)
data$Type <- as.character(data$Type)


unique(data$Country) # 151 countries

unique(data$Indicator) # 122 unique indicator

unique(data$ID) # 151 unique country ID

## number should probably be rounded up round(x,1)

armenia <- data[data$Country =="Armenia",]
armenia <- armenia[!(armenia$Type %in% "Rank"),]

AM_18 <- armenia[,c("ID", "Country", "Indicator", "2017-2018")]
AM_18$`2017-2018` <- round(AM_18$`2017-2018`,1)

# FUNCTIONS
count_stars_pos <- function(data, indicator) {
  max <- max(data_18$`2017-2018`[data_18$Indicator == indicator], na.rm = T)
  min <- min(data_18$`2017-2018`[data_18$Indicator == indicator], na.rm = T)
  return (6 * ((data - min) / (max - min))  + 1)
}
count_stars_neg <- function(data, indicator) {
  max <- max(data_18$`2017-2018`[data_18$Indicator == indicator], na.rm = T)
  min <- min(data_18$`2017-2018`[data_18$Indicator == indicator], na.rm = T)
  return (-6 * ((data - min) / (max - min))  + 7)
}

half_function <- function(data) {
  complete <- filter(data, half == F) # ones who don'ts
  non_complete <-  filter(data, half == T) # ones who have half
  return(as.numeric((sum(complete$`2017-2018`) + 0.5*sum(non_complete$`2017-2018`)) / 
           (count(complete) + 0.5* count(non_complete))))
}

halfs <- c("Mobile telephone subscriptions", "Fixed telephone lines", "Intellectual property protection")


first_A_1 <- c("Property rights (WEF)", "Intellectual property protection") # 20 %
first_A_2 <- c("Diversion of public funds", "Public trust in politicians", "Irregular payments and bribes") # 20 
first_A_3 <- c("Judicial independence (WEF)", "Favoritism in decisions of government officials, 1-7 (best)") # 20
first_A_4 <- c("Wastefulness of government spending", "Burden of government regulation, 1-7 (best)",
               "Efficiency of legal framework in challenging regs", "Efficiency of legal framework in settling disputes",
               "Transparency of government policymaking") # 20
first_A_5 <- c("Business costs of terrorism", "Business costs of crime and violence", "Organized crime", 
               "Reliability of police services") # 20

first_A_1 <- AM_18[AM_18$Indicator %in% c(first_A_1),] # has a half
first_A_1$half <- ifelse(first_A_1$Indicator %in% halfs, T, F)
first_A_1_mean <- half_function(first_A_1)


first_A_2 <- AM_18[AM_18$Indicator %in% c(first_A_2),] # no half
first_A_2$half <- ifelse(first_A_2$Indicator %in% halfs, T, F)
first_A_2_mean <- mean(first_A_2$`2017-2018`)

first_A_3 <- AM_18[AM_18$Indicator %in% c(first_A_3),]
first_A_3$half <- ifelse(first_A_3$Indicator %in% halfs, T, F)
first_A_3_mean <- mean(first_A_3$`2017-2018`)

first_A_4 <- AM_18[AM_18$Indicator %in% c(first_A_4),]
first_A_4$half <- ifelse(first_A_4$Indicator %in% halfs, T, F)
first_A_4$`2017-2018`[first_A_4$Indicator == "Wastefulness of government spending"] <- 3.3 ############### this should me manually computed
first_A_4_mean <- mean(first_A_4$`2017-2018`)

first_A_5 <- AM_18[AM_18$Indicator %in% c(first_A_5),]
first_A_5$half <- ifelse(first_A_5$Indicator %in% halfs, T, F)
first_A_5_mean <- mean(first_A_5$`2017-2018`)

first_A <- rbind(first_A_1, first_A_2, first_A_3, first_A_4, first_A_5) # 75
first_A$ID <- "first_A"
first_A$half<- NULL 
first_A$index <- mean(c(first_A_1_mean, first_A_2_mean, first_A_3_mean, first_A_4_mean, first_A_5_mean))

first_B_1 <- c("Ethical behavior of firms") # 50
first_B_2 <- c("Strength of auditing and reporting standards", "Efficacy of corporate boards",
               "Protection of minority shareholders interests", "Strength of investor protection") # 50

first_B_1 <- AM_18[AM_18$Indicator %in% c(first_B_1),] # no half
first_B_1$half <- ifelse(first_B_1$Indicator %in% halfs, T, F)
first_B_1_mean <- mean(first_B_1$`2017-2018`)

first_B_2 <- AM_18[AM_18$Indicator %in% c(first_B_2),] # no half
first_B_2$half <- ifelse(first_B_2$Indicator %in% halfs, T, F)
first_B_2_mean <- mean(first_B_2$`2017-2018`)
# strength investor has *
Indicator <- "Strength of investor protection"
strength_investor_AM <- count_stars_pos(first_B$`2017-2018`[first_B_2$Indicator == Indicator], Indicator)
first_B_2$`2017-2018`[first_B_2$Indicator == "Strength of investor protection"] <- strength_investor_AM

first_B <- rbind(first_B_1, first_B_2)
first_B$ID <- "first_B"
first_B$half<- NULL
first_B$index <-  mean(c(first_B_1_mean, first_B_2_mean))

first_index <- ((first_A$index[1] * .75) + (first_B$index[1] * .25)) #  4.093375
first <- rbind(first_A, first_B)
first$index <- NULL
first_row <- c(first$ID[1], first$Country[1], "First pillar", first_index)
first <- rbind(first, first_row)

#### second pillar

second_A <- c("Quality of overall infrastructure", "Quality of roads", "Quality of railroad infrastructure",
              "Quality of port infrastructure", "Quality of air transport infrastructure", "Available airline seat")
second_A <- AM_18[AM_18$Indicator %in% second_A,]

Indicator <- c("Available airline seat")
AAS <- count_stars_pos(second_A$`2017-2018`[second_A$Indicator == Indicator], Indicator)
second_A$`2017-2018`[second_A$Indicator == Indicator] <- AAS

second_A$half <- ifelse(second_A$Indicator %in% halfs, T, F)
second_A_mean <- mean(second_A$`2017-2018`)
second_A$half <- NULL
second_A$index <- second_A_mean
second_A$ID <- "second_A"
## second B
second_B <- c("Quality of electricity supply", "Mobile telephone subscriptions", "Fixed telephone lines")
second_B <- AM_18[AM_18$Indicator %in% second_B,]

Indicator <- "Mobile telephone subscriptions"
MTS <- count_stars_pos(second_B$`2017-2018`[second_B$Indicator == Indicator], Indicator)
second_B$`2017-2018`[second_B$Indicator == Indicator] <- MTS

Indicator <- "Fixed telephone lines"
FTS <- count_stars_pos(second_B$`2017-2018`[second_B$Indicator == Indicator], Indicator)
second_B$`2017-2018`[second_B$Indicator == Indicator] <- FTS

second_B$half <- ifelse(second_B$Indicator %in% halfs, T, F)
second_B$index <- half_function(second_B)
second_B$half <- NULL
second_B$ID <- "second_B"


second_index <- mean(c(second_A$index[1], second_B$index[1]))
second <- rbind(second_A, second_B)
second$index <- NULL
second_row <- c(second$ID[1], second$Country[1], "Second pillar", second_index)
second <- rbind(second, second_row)
## check why it's wrong probably the start thing



# third pillar

third <- c("")



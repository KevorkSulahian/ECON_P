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


## Check if all countries have 122 obs.
# 
# US <- data[data$Country =="United States",]
# US <- US[!(US$Type %in% "Rank"),]
# 
# US_18 <- US[,c("ID", "Country", "Indicator", "2017-2018")]
# US_18$`2017-2018` <- round(US_18$`2017-2018`,1)

### US does
# 
# data_18 <- data[,c("ID", "Country", "Indicator", "Type", "2017-2018")]
# data_18 <- data_18[!(data_18$Type %in% "Rank"),]
# unique(data_18$Indicator)

### they all do 

## Now we need to understand which are available and which are not
# check the notebook tho


# AM_18[AM_18$Indicator == "Property rights (WEF)",]


first_A_1 <- c("Property rights (WEF)", "Intellectual property protection") # 20 %
# Second one is 1/2 (check what it means !!!!!!!!!!!!!!!!)
#AM_18[AM_18$Indicator %in% first_A_1,]

first_A_2 <- c("Diversion of public funds", "Public trust in politicians", "Irregular payments and bribes") # 20 
first_A_3 <- c("Judicial independence (WEF)", "Favoritism in decisions of government officials, 1-7 (best)") # 20
first_A_4 <- c("Wastefulness of government spending", "Burden of government regulation, 1-7 (best)",
               "Efficiency of legal framework in challenging regs", "Efficiency of legal framework in settling disputes",
               "Transparency of government policymaking") # 20
first_A_5 <- c("Business costs of terrorism", "Business costs of crime and violence", "Organized crime", 
               "Reliability of police services") # 20
first_A <- AM_18[AM_18$Indicator %in% c(first_A_1, first_A_2, first_A_3, first_A_4, first_A_5),] # 75

first_B_1 <- c("Ethical behavior of firms") # 50
first_B_2 <- c("Strength of auditing and reporting standards", "Efficacy of corporate boards",
               "Protection of minority shareholders interests", "Strength of investor protection") # 50
# ask why there is a * in the end of "Strength of investor protection"
first_B <- AM_18[AM_18$Indicator %in% c(first_B_1, first_B_2),] # 25
first_A$`2017-2018`[first_A$Indicator == "Wastefulness of government spending"] <- 3.3 # this should me manually computed
A <- mean(first_A$`2017-2018`, na.rm = T)
B <- mean(first_B$`2017-2018`, na.rm = T)

first <- ((A * .75) + (B * .25)) # 

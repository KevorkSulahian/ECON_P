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





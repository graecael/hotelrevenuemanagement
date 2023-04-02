install.packages("rpart.plot")    
install.packages("caTools") 
install.packages("ggplot2")
install.packages("data.table")
install.packages("tidyr")


library(data.table)
library(dplyr)
library(ggplot2)
library(rpart.plot)
library(rpart)
library(caTools)
library(tidyr)
setwd("/Users/phyllisyen/Desktop/BC2407 ANALYTICS 2/Project")
customerds <- fread("HotelCustomersDataset.csv")


# ----- filtering and dropping columns
#Drop NameHash and DocumentIDHash Columns
customerds <- customerds[,! c("NameHash","DocIDHash", "ID", "DaysSinceLastStay", "DaysSinceCreation")]



# ----- Add column to show proportion of other revenue over total revenue -> helps hotel to understand the % of their revenue coming from the other services they provide 
customerds <- customerds[OtherRevenue > 0 & LodgingRevenue > 0, "PercOtherRevenue" := round(OtherRevenue/(LodgingRevenue + OtherRevenue),2)]
customerds <- customerds[OtherRevenue == 0 & LodgingRevenue == 0, "PercOtherRevenue" := 0]
customerds <- customerds[OtherRevenue == 0 & LodgingRevenue > 0, "PercOtherRevenue" := 0]
customerds <- customerds[OtherRevenue > 0 & LodgingRevenue == 0, "PercOtherRevenue" := 1]

# ----- Add column to show % of lodging revenue over total revenue
customerds <- customerds[, "PercLodgingRevenue" := 1-PercOtherRevenue]

# ----- Add NumSR column to count number of special requests made by each customer
customerds <- customerds[, "NumSR" := SRHighFloor + SRCrib + SRKingSizeBed + SRTwinBed + SRQuietRoom]

# ----- Add column to show check in date 
customerds[, CheckInDate := ""]
base_date <- as.Date("2018-12-31")
customerds$CheckInDate <- ifelse(customerds$DaysSinceFirstStay == -1, NA,
                          as.character(base_date - customerds$DaysSinceFirstStay))
str(customerds$CheckInDate)
customerds$CheckInDate <- as.Date(customerds$CheckInDate)

# ----- Add season column according to months
customerds[, Season := '']
customerds[month(customerds$CheckInDate) >= 3 & month(customerds$CheckInDate) < 6, Season := "Spring"]
customerds[month(customerds$CheckInDate) >= 6 & month(customerds$CheckInDate) < 9, Season := "Summer"]
customerds[month(customerds$CheckInDate) >= 9 & month(customerds$CheckInDate) < 12, Season := "Autumn"]
customerds[month(customerds$CheckInDate) < 3, Season := "Winter"]
customerds[month(customerds$CheckInDate) == 12, Season := "Winter"]
customerds[Season == '', Season := 'unknown']
str(customerds$Season)
customerds$Season <- factor(customerds$Season)



#---- Set Data types
str(customerds)
customerds$Nationality <- as.factor(customerds$Nationality)
customerds$Age <- as.integer(customerds$Age)
customerds$DistributionChannel <- as.factor(customerds$DistributionChannel)
customerds$MarketSegment <- as.factor(customerds$MarketSegment)
customerds$SRHighFloor <- as.factor(customerds$SRHighFloor)
customerds$SRLowFloor <- as.factor(customerds$SRLowFloor)
customerds$SRAccessibleRoom <- as.factor(customerds$SRAccessibleRoom)
customerds$SRMediumFloor <- as.factor(customerds$SRMediumFloor)
customerds$SRBathtub <- as.factor(customerds$SRBathtub)
customerds$SRShower <- as.factor(customerds$SRShower)
customerds$SRCrib <- as.factor(customerds$SRCrib)
customerds$SRKingSizeBed <- as.factor(customerds$SRKingSizeBed)
customerds$SRTwinBed <- as.factor(customerds$SRTwinBed)
customerds$SRNearElevator <- as.factor(customerds$SRNearElevator)
customerds$SRAwayFromElevator <- as.factor(customerds$SRAwayFromElevator)
customerds$SRNoAlcoholInMiniBar <- as.factor(customerds$SRNoAlcoholInMiniBar)
customerds$SRQuietRoom <- as.factor(customerds$SRQuietRoom)


# Drop SR-prefixed Columns w low occurrence
summary(customerds)
customerds <- customerds[, ! c("SRLowFloor","SRAccessibleRoom","SRMediumFloor","SRBathtub","SRShower","SRNearElevator","SRAwayFromElevator","SRNoAlcoholInMiniBar")]
# Drop DaysSinceFirstStay
customerds <- customerds[, ! 'DaysSinceFirstStay']

# missing values
summary(customerds$Age)   # 3779 NAs

# identifying outlier values  
ggplot(customerds, aes(y = Age)) + geom_boxplot() + labs(title = "Boxplot of Age")
# identify specific outliers
box_plot <- boxplot(customerds$Age)$out
outlier_row <- which(customerds$Age %in% c(box_plot))
customerds[outlier_row,]
# deciding on what values to replace with
ggplot(customerds, aes(x = Age)) + geom_histogram() + labs(title = "Histogram of Age") 
#create new column for changing NA and invalid values
customerds[, Age1 := '']
customerds[is.na(Age), Age1 := 46]
customerds[outlier_row, Age1 := 46]
customerds[Age <18, Age1 := 46]
customerds[Age1 == '', Age1 := Age]
str(customerds)
customerds$Age1 <- as.integer(customerds$Age1) # change Age1 to int
summary(customerds$Age1)  # compare output with original to see if there are any significant changes
# drop old Age column since summary is consistent
customerds <- customerds[, !"Age"]  

str(customerds)

# export to csv
names(customerds)[names(customerds) == 'Data Series'] <- 'Customer Data'
write.csv(customerds, "/Users/phyllisyen/Desktop/BC2407 ANALYTICS 2/Customer_Data_(cleaned)")



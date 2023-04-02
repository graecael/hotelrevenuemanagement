#Loading Relevant Packages
library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(GGally)
library(corrplot)
library(VIM)
library(tidyverse)
library(MASS)
library(glmnet)
library(randomForest)
library(ggcorrplot)

#Set Working Directory
setwd("C:/Users/khooy/OneDrive/Documents/ABC2407 Analytics 2/project")

#Reading Hotel Bookings Dataset
hb.dt <- fread("hotel_bookings.csv", stringsAsFactors = T)

#Based on the research paper and our own interpretation, we have categorise some of the columns
summary(hb.dt)
sapply(hb.dt,"class")
hb.dt$is_canceled<-factor(hb.dt$is_canceled)
hb.dt$is_repeated_guest<-factor(hb.dt$is_repeated_guest)

#-------------------------------------------------------------------------------

#Next, we will be removing unnecessary data columns. We concatenate arrival_date_year, arrival_date_month,arrival_date_day_of_month to arrival_date and drop the columns. This will maintain consistency as reservation_status_date which is on IDate format.
levels(hb.dt$arrival_date_month)
hb.dt$arrival_month<-hb.dt$arrival_date_month
hb.dt$arrival_month <- as.character(hb.dt$arrival_month)
hb.dt$arrival_month[hb.dt$arrival_month=="January"]<-"1"
hb.dt$arrival_month[hb.dt$arrival_month=="February"]<-"2"
hb.dt$arrival_month[hb.dt$arrival_month=="March"]<-"3"
hb.dt$arrival_month[hb.dt$arrival_month=="April"]<-"4"
hb.dt$arrival_month[hb.dt$arrival_month=="May"]<-"5"
hb.dt$arrival_month[hb.dt$arrival_month=="June"]<-"6"
hb.dt$arrival_month[hb.dt$arrival_month=="July"]<-"7"
hb.dt$arrival_month[hb.dt$arrival_month=="August"]<-"8"
hb.dt$arrival_month[hb.dt$arrival_month=="September"]<-"9"
hb.dt$arrival_month[hb.dt$arrival_month=="October"]<-"10"
hb.dt$arrival_month[hb.dt$arrival_month=="November"]<-"11"
hb.dt$arrival_month[hb.dt$arrival_month=="December"]<-"12"
hb.dt$arrival_date <- paste(hb.dt$arrival_date_year, hb.dt$arrival_month,hb.dt$arrival_date_day_of_month,sep="-")
hb.dt$arrival_date <- as.IDate(hb.dt$arrival_date, format="%Y-%m-%d" )

#We will drop columns: agent and company as they are encoded with numbers due to data privacy policy. We do not have the type of industry of company and will not form any significant conclusion from the data. Hence the team decided to drop the 2 columns. We have also drop the original columns arrival_date_year, arrival_date_month, arrival_date_day_of_month, arrival_month
hb.dt <- hb.dt[, ! c("agent","company","arrival_date_year","arrival_date_month","arrival_date_day_of_month","arrival_month")]

# DATA CLEANING AND EXPLORATION ================================================
summary(hb.dt)

#1:looking at the summary, the children column has 4 NA cases, these 4 NA cases constitute an insignificant percentage in the whole dataset of 0.003%, hence, the team verdict to remove the 4 cases with NA value.
sum(is.na(hb.dt$children))/nrow(hb.dt)*100
hb.dt <-hb.dt[complete.cases(hb.dt$children), ] 

#2: looking at the summary, for the column meal plan undefined, it is one of the categorical factor which means the customers did not order any meal plan package, hence, the column will remain as it is.

#3: After cleaning the children column, the distribution_channel column has 1 undefined values. A drop of 4 undefined value previously identified in the summary. It is not part of the categorical factor and hence, the team plans to drop the 1 cases since the 1 case constitute a small factor in the dataset of 0.0008%
summary(hb.dt)
sum(hb.dt$distribution_channel=="Undefined")/nrow(hb.dt)*100
hb.dt<- hb.dt[hb.dt$distribution_channel!="Undefined"]

#4: ADR is determined as average daily rate which means it is calculated by dividing the sum of all lodging transactions by the total number of night stayed. Based on the visualization graph, we have found that there are some rows with negative lodging transaction. It is impossible for ADR to be negative as there ought to be some transactions and even if there are refunds, hotels would not provide refunds that is beyond what customers are paid off. Thus, the team have done some data visualization to determine whether the rows should be dropped or be replaced with a positive value
summary(hb.dt)
## Graphic visualization of ADR
ggplot(data = hb.dt, aes(x = adr)) + 
geom_boxplot(fill="yellow") +
labs(title = "Box plot of ADR", y = "ADR") +
theme_bw()
## From the graph there is an extreme outlier of adr at $5400, we will retrieve the row to figure why is it so and determine whether the outlier should be removed.
outlier<-hb.dt[hb.dt$adr==5400,]
##when we retrieve the column, we realised that the customer has a non refundable transaction with a grade A room and customers had cancelled the reservation. Hence, the charge may be reasonable and the team has decided not to drop the outlier. Nonetheless, the team has remove the row with negative adr as there is only one row with negative adr.
sum(hb.dt$adr<0)
hb.dt<- hb.dt[hb.dt$adr>=0]

#5: From the graph data visualization, we realise there are 2 extreme outliers present in the number of babies column. Our team verdict to drop of these columns.
ggplot(data = hb.dt, aes(x = babies)) + 
geom_boxplot(fill="yellow") +
labs(title = "Box plot of no of babies", y = "babies") +
theme_bw()
#removing extreme outliers of >5yr old
sum(hb.dt$babies>5)
hb.dt<- hb.dt[hb.dt$babies<5]

#6: Looking at the summary, we realise that some countries takes up particularly large amount of entire dataset and we want to categorise those that are significantly lesser as others since country has 176 levels which is simply too much to go through the models. The top 5 visitor countries are PRT, GBR, FRA, ESP, DEU, ITA
summary(hb.dt)
hb.dt$country <- as.character(hb.dt$country)
hb.dt$country[hb.dt$country!=c("PRT","GBR","FRA","ESP","DEU","ITA")]<-"Others"
hb.dt$country <- as.factor(hb.dt$country)

#7: Based on data visualisation, we have identified columns that have 0 adults, 0 children and 0 babies stay. These columns will be dropped as it is impossible that the hotel room without having anyone staying. We will not be doing doing predictive models to predict the number of adults .
sum(hb.dt$adults==0&hb.dt$children==0&hb.dt$babies==0)
hb.dt<-hb.dt[!hb.dt$adults==0&hb.dt$children==0&hb.dt$babies==0]
#We have cleaned the data.

# DATA VISUALISATION ===========================================================
#1: Proportion of Cancellations in Deposit Types
summary(hb.dt$deposit_type)
summary(hb.dt$is_canceled[hb.dt$deposit_type == "No Deposit"])
summary(hb.dt$is_canceled[hb.dt$deposit_type == "Non Refund"])
summary(hb.dt$is_canceled[hb.dt$deposit_type == "Refundable"])

#2 Cancellation Count of Segmented Customers
ggplot(hb.dt, aes(x = is_canceled, fill = is_repeated_guest)) + 
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-2,size=3.5)+
  labs(title = "Cancellations by Customer Segment")+theme_bw()

#3 Booking Patterns of Repeated Customers
repeated_customer<-hb.dt[is_repeated_guest==1]
repeated_customer$total_previous_bookings <- repeated_customer$previous_bookings_not_canceled + repeated_customer$previous_cancellations
repeated_customer$proportion_of_cancellations_in_previous_bookings <- repeated_customer$previous_cancellations/repeated_customer$total_previous_bookings 
ggplot(repeated_customer, aes(x=proportion_of_cancellations_in_previous_bookings, fill=is_canceled, main ="Density Plot of Lead Time by Cancellation"))+ geom_density(alpha=.3)

#4 Average Daily Rate (ADR) by Cancellation

boxplot(hb.dt$adr ~ hb.dt$is_canceled, 
        main = "Average Daily Rate ",
        xlab = "Cancellation ", 
        ylab = "ADR ($)")

summary(hb.dt$adr[hb.dt$is_canceled== 0])
summary(hb.dt$adr[hb.dt$is_canceled== 1])

#5 Length of Stay on Cancellation

hb.dt$days <- hb.dt$stays_in_weekend_nights + hb.dt$stays_in_week_nights
boxplot(hb.dt$days ~ hb.dt$is_canceled, 
        main = "Length of Stay ",
        xlab = "Cancellation ", 
        ylab = "Length of Stay")

#6 Customer Type on Cancellation
ggplot(hb.dt, aes(x = customer_type, fill = is_canceled)) + 
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-2,size=3.5)+
  labs(title = "Cancellations by Customer Segment")+theme_bw()


#Correlation Visualisation 
#analyzing the collinearity for continuous variables
library(ggcorrplot)
ivar <- hb.dt [, c(3,4,5,6,7,8,9,15,16,19,21,23,24,25)]
ivar <- ivar[complete.cases(ivar)]
model.matrix(~0+., data=ivar) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2, tl.cex = 6)
#no collinearity between pairs of different x categorical variables

#analyzing the collinearity for categorical variables
icatvar <-  hb.dt [, c(1,2,10,12,13,14,17,18,20,22)]
icatvar <- icatvar[complete.cases(icatvar)]
model.matrix(~0+., data=icatvar) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2, tl.cex = 6)

#no collinearity between pairs of different x categorical variables
#-----------------------------------------------------------------------------------

#We will be retrieving cases that is city hotel since we will be implementing our methodology in Singapore's context and Singapore is a city thus we will be looking at city hotel only.
#we will be subsetting the hotel to "city hotel" after cleaning the original dataset so we will be able to use the original dataset when required
#sum of city hotel is 73756 which is still alot
sum(hb.dt$hotel=="City Hotel")
hb2.dt<-hb.dt[hb.dt$hotel=="City Hotel",]
sum(hb2.dt$is_canceled=="1")
sum(hb2.dt$is_canceled=="1")/nrow(hb2.dt)

# Target Variable: Cancelled (0 = No, 1 = Yes)
ggplot(data = hb2.dt,aes(x=is_canceled)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Number of Cancellations", x = "Cancelled", y = "Percentage")

#download to own computer
write.csv(hb2.dt, "hb2")

#we will be doing analysis on cancellation rate on hb2.dt via various models. e.g., logistic regression model, CART model, Random Forest model.
#for now, we will not be looking at date and time of the model--we will be using those columns for tableau/further data visualisation
hb3.dt<-hb2.dt[,c(2,3,5:26)]

#we will not be putting in reservation_status as it is ultimately showing the cancellation rate of the hotel. 
hb3.dt<-hb3.dt[,!c("reservation_status")]

#TRAIN TEST SPLIT ==============================================================
set.seed(888)
hb3.train <- sample.split(Y = hb3.dt$is_canceled, SplitRatio = 0.7)
hb3_trainset <- subset(hb3.dt, hb3.train == T)
hb3_testset <- subset(hb3.dt, hb3.train == F)
threshold<-0.5

# LOGISTIC REGRESSION ==========================================================
log <- glm(is_canceled ~.,family = binomial, data = hb3_trainset)
summary(log)

#using lasso regression for feature selection as normal regression do not generate these variables due to presence of unimportant independent variables that result in high p value in the entire model
x<-model.matrix(is_canceled~.,data=hb3_trainset)
x=x[,-1]
lasso<-cv.glmnet(x=x,y=hb3_trainset$is_canceled,type.measure='auc',nfolds=10,alpha=.5,family="binomial")
c<-coef(lasso,s='lambda.min',exact=TRUE)
inds<-which(c==0)
variables<-row.names(c)[inds]
variables
#insignificant variables in lasso regression
#"children","babies","mealUndefined","market_segmentUndefined","distribution_channelUndefined","reserved_room_typeH","reserved_room_typeL"      ,"reserved_room_typeP","assigned_room_typeH","assigned_room_typeI,"assigned_room_typeL","assigned_room_typeP" 

#LOGISTIC REGRESSION FOLLOWING FEATURE SELECTION ===============================
log1<- glm(is_canceled ~. -children-babies-reserved_room_type-assigned_room_type,family = binomial, data = hb3_trainset)
summary(log1)
log2 <- glm (is_canceled ~ .-children-babies-reserved_room_type-assigned_room_type-required_car_parking_spaces-is_repeated_guest-market_segment-days_in_waiting_list, family = binomial, data = hb3_trainset)
summary (log2)

# Confusion Matrix on Trainset
log2.train <- predict(log2, type = "response")
log2.predict.train <- ifelse(log2.train > threshold, "1", "0")
mean(log2.predict.train == hb3_trainset$is_canceled)

log_traintable <- table(Trainset.Actual = hb3_trainset$is_canceled, log2.predict.train, deparse.level = 2)
log_traintable 

#80.28

# Confusion Matrix on Testset
log2.test <- predict(log2, newdata = hb3_testset, type = 'response')
log2.predict <- ifelse(log2.test > threshold, "1", "0")
log_testtable <- table(Testset.Actual = hb3_testset$is_canceled, log2.predict, deparse.level = 2)
log_testtable
round(prop.table(log_testtable), 3)
mean(log2.predict == hb3_testset$is_canceled)
#80.00%

library(pROC)
train_roc <- roc(hb3_trainset$is_canceled, log2.train)
auc_roc <- auc(train_roc)
cat("Trainset AUC-ROC score is:", auc_roc) # Trainset AUC-ROC score is: 0.8499185
test_roc <- roc(hb3_testset$is_canceled, log2.test)
auc_roc <- auc(test_roc)
cat("Testset AUC-ROC score is:", auc_roc) # Testset AUC-ROC score is: 0.8495964

# Plot ROC curve
plot(train_roc, col = "blue", main = "ROC Curve for Logistic Regression")
lines(test_roc, col = "red")
legend("bottomright", legend = c("Training Data", "Test Data"), col = c("blue", "red"), lty = 1)

#AUC Score
auc.score <- auc(hb3_testset$is_canceled, log2.test)
cat("AUC-ROC score is:", auc.score)

# CART MODEL ===================================================================
cart1 <- rpart(is_canceled ~ ., data = hb3_trainset, method = 'class',control = rpart.control(minsplit = 2, cp = 0))
printcp(cart1)
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cart1.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
CVerror.cart1.cap
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cart1.cap) {i <- i + 1}
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
cp.opt
#max cp = 0.0001429127 pruning

cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)
cart2$variable.importance
scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0] 
rpart.plot(cart2, nn = T, main = "Optimal Tree in hotel booking cancellation set")
summary(cart2)

#Trainset CART
threshold <- 0.5
cart2.train <- predict(cart2, type = 'class')
#Confusion Matrix for Trainset CART
table1 <- table(Trainset.Actual = hb3_trainset$is_canceled, cart2.train, deparse.level = 2)
table1 
round(prop.table(table1), 3)
#Trainset Accuracy
mean(cart2.train == hb3_trainset$is_canceled)
#85.57%

#Testset CART
cart2.test.prob <- predict(cart2, newdata = hb3_testset, type = "class")
#Confusion Matrix for Testset CART
table2 <- table(Testset.Actual = hb3_testset$is_canceled, cart2.test.prob, deparse.level = 2)
table2
round(prop.table(table2), 3)
#Testset Accuracy
mean(cart2.test.prob == hb3_testset$is_canceled)
#84.00%
# CART MODEL AUC-ROC CURVE =====================================================
library(pROC)

train_pred <- predict(cart2,type = "prob")[,2]
train_roc<- roc(hb3_trainset$is_canceled, train_pred)
auc_roc <- auc(train_roc)
cat("Trainset AUC-ROC score is:", auc_roc) # Trainset AUC-ROC score is: 0.9083346

test_pred <- predict(cart2, newdata = hb3_testset, type = "prob")[,2]
test_roc <- roc(hb3_testset$is_canceled, test_pred)
auc_roc <- auc(test_roc)
cat("Testset AUC-ROC score is:", auc_roc) #Testset AUC-ROC score is: 0.8945239

# Plot ROC curve
plot(train_roc, col = "blue", main = "ROC Curve for Logistic Regression")
lines(test_roc, col = "red")
legend("bottomright", legend = c("Training Data", "Test Data"), col = c("blue", "red"), lty = 1)


# RANDOM FOREST ================================================================
library(randomForest)
set.seed(888)
m.RF.1<-randomForest(is_canceled~.,data=hb3_trainset,importance=T, cv.fold=10)
m.RF.1
var.impt2 <- importance(m.RF.1)
var.impt2
varImpPlot(m.RF.1, type = 1)

#trainset randomforest 
RF.train <- predict(m.RF.1, type = "class")
table3 <- table(Trainset.Actual = hb3_trainset$is_canceled, RF.train, deparse.level = 2)
table3
round(prop.table(table3), 3)
mean(RF.train == hb3_trainset$is_canceled)
#84.86%


#testset random forest
m.RF.1.test.prob <- predict(m.RF.1, newdata = hb3_testset, type = "class")
table4 <- table(Testset.Actual = hb3_testset$is_canceled, m.RF.1.test.prob, deparse.level = 2)
table4
round(prop.table(table4), 3)
mean(m.RF.1.test.prob == hb3_testset$is_canceled)
#84.82%

#HYPERPARAMTER TUNING IN RANDOM FOREST USING OOBE =============================

set.seed(888)
x <- hb3.dt[,2:23]
y <- hb3.dt$is_canceled
bestMtry <- tuneRF(x,y, stepFactor = 5, improve = 0.01, ntree = 500)
print(bestMtry)
#Best Mtry at Mtry =20


# Fit the random forest model with the optimal mtry value
rf_model <- randomForest(x, y, mtry = 20, ntree = 500)

# Make predictions on the test set
test_preds <- predict(rf_model, newdata = hb3_testset, type = 'class')
table5 <- table(Testset.Actual = hb3_testset$is_canceled,test_preds, deparse.level = 2)
table5
round(prop.table(table5), 3)
mean(test_preds == hb3_testset$is_canceled)

# RANDOM FOREST MODEL WITH ADJUSTED MTRY =======================================
m.RF.2<-randomForest(is_canceled~.,data=hb3_trainset,importance=T, cv.fold=10, mtry=20)
m.RF.2
var.impt2 <- importance(m.RF.2)
var.impt2
varImpPlot(m.RF.2, type = 1)

#trainset randomforest 
RF.train.2 <- predict(m.RF.2, type = "class")
table6 <- table(Trainset.Actual = hb3_trainset$is_canceled, RF.train.2, deparse.level = 2)
table6 
mean(RF.train.2 == hb3_trainset$is_canceled)
#85.95%


#testset random forest
m.RF.2.test.prob <- predict(m.RF.2, newdata = hb3_testset, type = "class")
table7 <- table(Testset.Actual = hb3_testset$is_canceled, m.RF.2.test.prob, deparse.level = 2)
table7
round(prop.table(table7), 3)
mean(m.RF.2.test.prob == hb3_testset$is_canceled)
#85.96%

# RANDOM FOREST AUC-ROC CURVE ==================================================

library(pROC)

train_pred <- predict(m.RF.1,type = "prob")[,2]
train_roc <- roc(hb3_trainset$is_canceled, train_pred)
auc_roc <- auc(train_roc)
cat("Trainset AUC-ROC score is:", auc_roc) #0.9132

test_pred <- predict(m.RF.1, newdata = hb3_testset, type = "prob")[,2]
test_roc <- roc(hb3_testset$is_canceled, test_pred)
auc_roc <- auc(test_roc)
cat("Testset AUC-ROC score is:", auc_roc) #0.9123

# Plot ROC curve
plot(train_roc, col = "blue", main = "ROC Curve for Random Forest")
lines(test_roc, col = "red")
legend("bottomright", legend = c("Training Data", "Test Data"), col = c("blue", "red"), lty = 1)



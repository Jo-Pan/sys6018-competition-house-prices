setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-house-prices")
library(readr)
library(dplyr)

# --------------- Data Cleaning ----------------
#Read data
test<-read.csv("test.csv")
train<-read.csv("train.csv")
str(train)
#separate factor var & numeric var
fac_var <- names(train)[which(sapply(train, is.factor))]      #factor variables' colnames (no need to convert to factor)
numeric_var <- names(train)[which(sapply(train, is.numeric))] #numeric variables' colnames

# any duplicate row? no.
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))


#Identify na
na_list<-colSums(is.na(train)) #count na in each col
na_list[na_list!=0]            #print those cols with na
names(na_list[na_list!=0])

#fix numeric_var's na
na_list[numeric_var]
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$LotFrontage[is.na(train$LotFrontage)] <- 0

#fix factor_var's na based on data_description 
train[,fac_var]<-lapply(train[,fac_var],as.character) #convert factor variables to character
                                                  

train$Alley[is.na(train$Alley)]<-'na'  #No alley access
train$BsmtQual[is.na(train$BsmtQual)]<-'na' #No Basement
train$BsmtCond[is.na(train$BsmtCond)]<-'na' #No Basement
train$BsmtExposure[is.na(train$BsmtExposure)]<-'na'#No Basement
train$BsmtFinType1[is.na(train$BsmtFinType1)]<-'na'#No Basement
train$BsmtFinType2[is.na(train$BsmtFinType2)]<-'na'#No Basement
train$FireplaceQu[is.na(train$FireplaceQu)]<-'na'#No Fireplace
train$GarageType[is.na(train$GarageType)]<-'na'#No Garage
train$GarageQual[is.na(train$GarageQual)]<-'na'#No Garage
train$GarageCond[is.na(train$GarageCond)]<-'na'#No Garage
train$GarageFinish[is.na(train$GarageFinish)]<-'na'#No Garage
train$PoolQC[is.na(train$PoolQC)]<-'na'#No Pool
train$Fence[is.na(train$Fence)]<-'na'#No Fence
train$MiscFeature[is.na(train$MiscFeature)]<-'na'#None
train$MasVnrType[is.na(train$MasVnrType)]<-'None'  #there is a none category, so just make na to none
train$Electrical[is.na(train$Electrical)]<-'na'  #there is only 1 na

train[,fac_var]<-lapply(train[,fac_var],factor) #convert character variables to factors
colSums(is.na(train)) #no more na!

# --------------- Parametric ----------------
# Create the original regression model using all variables
lm1 <- lm(SalePrice ~., data=train)
summary(lm1) # Adjusted R-squared:  0.9192

# Trying to find the best fit by running all possible combination of variables
# compare the significance level of each variable and the value for adjusted R-squared
library(MASS)
fit <- lm(SalePrice ~., data=train)
step <- stepAIC(fit, direction="both")
step$anova

# Determine the final regression model
lm_final <- lm(SalePrice ~ MSSubClass + MSZoning + LotArea + Street + LandContour + 
                 Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
                 Condition2 + BldgType + OverallQual + OverallCond + YearBuilt + 
                 YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
                 MasVnrArea + ExterQual + BsmtQual + BsmtCond + BsmtExposure + 
                 BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + 
                 BsmtFullBath + FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                 TotRmsAbvGrd + Functional + Fireplaces + GarageCars + GarageArea + 
                 GarageQual + GarageCond + WoodDeckSF + ScreenPorch + PoolArea + 
                 PoolQC + Fence + MoSold + SaleCondition, data=train)
summary(lm_final) # Adjusted R-squared:  0.9202

# Process the test dataset using the same method as we processed training set
#separate factor var & numeric var
test_fac_var <- names(test)[which(sapply(test, is.factor))]      #factor variables' colnames (no need to convert to factor)
test_numeric_var <- names(test)[which(sapply(test, is.numeric))] #numeric variables' colnames

# any duplicate row? no.
cat("The number of duplicated rows are", nrow(test) - nrow(unique(test)))


#Identify na
test_na_list<-colSums(is.na(test)) #count na in each col
test_na_list[test_na_list!=0]            #print those cols with na
names(test_na_list[test_na_list!=0])

#fix numeric_var's na
test_na_list[test_numeric_var]
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$LotFrontage[is.na(test$LotFrontage)] <- 0

#fix factor_var's na based on data_description 
test[,test_fac_var]<-lapply(test[,test_fac_var],as.character) #convert factor variables to character


test$Alley[is.na(test$Alley)]<-'na'  #No alley access
test$BsmtQual[is.na(test$BsmtQual)]<-'na' #No Basement
test$BsmtCond[is.na(test$BsmtCond)]<-'na' #No Basement
test$BsmtExposure[is.na(test$BsmtExposure)]<-'na'#No Basement
test$BsmtFinType1[is.na(test$BsmtFinType1)]<-'na'#No Basement
test$BsmtFinType2[is.na(test$BsmtFinType2)]<-'na'#No Basement
test$FireplaceQu[is.na(test$FireplaceQu)]<-'na'#No Fireplace
test$GarageType[is.na(test$GarageType)]<-'na'#No Garage
test$GarageQual[is.na(test$GarageQual)]<-'na'#No Garage
test$GarageCond[is.na(test$GarageCond)]<-'na'#No Garage
test$GarageFinish[is.na(test$GarageFinish)]<-'na'#No Garage
test$PoolQC[is.na(test$PoolQC)]<-'na'#No Pool
test$Fence[is.na(test$Fence)]<-'na'#No Fence
test$MiscFeature[is.na(test$MiscFeature)]<-'na'#None
test$MasVnrType[is.na(test$MasVnrType)]<-'None'  #there is a none category, so just make na to none
test$Electrical[is.na(test$Electrical)]<-'na'  #there is only 1 na

test[,test_fac_var]<-lapply(test[,test_fac_var],factor) #convert character variables to factors
colSums(is.na(test)) #no more na!

# Using the final model to predict
preds <- as.vector(predict(lm_final, newdata=test))
table <- data.frame(test$Id, preds)
write.table(table, file="housepricesprediction.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

# --------------- Non-Parametric ------------


# ==================================================
#references:
#https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r/notebook

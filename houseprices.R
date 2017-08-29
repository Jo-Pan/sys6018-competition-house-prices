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
lm1 <- lm(SalePrice ~., data=train)
summary(lm1) #Adjusted R-squared:  0.9192 

# --------------- Non-Parametric ------------


# ==================================================
#references:
#https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r/notebook
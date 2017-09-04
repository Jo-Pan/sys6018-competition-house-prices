setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-house-prices")
library(readr)
library(dplyr)
library(dummies)

# --------------- Data Cleaning ----------------
#Read data
test<-read.csv("test.csv")
train<-read.csv("train.csv")
test$dataset<-'test'
test$SalePrice<-NA
train$dataset<-'train'
comb<-rbind(test,train)

#separate factor var & numeric var
fac_var <- names(comb)[which(sapply(comb, is.factor))]      #factor variables' colnames (no need to convert to factor)
numeric_var <- names(comb)[which(sapply(comb, is.numeric))] #numeric variables' colnames

# any duplicate row? no.
cat("The number of duplicated rows are", nrow(comb) - nrow(unique(comb)))

#Identify na
na_list<-colSums(is.na(comb)) #count na in each col
na_list[na_list!=0]            #print those cols with na
names(na_list[na_list!=0])

#fix some numeric_var's na
comb$GarageYrBlt[is.na(comb$GarageYrBlt)] <- 0
comb$MasVnrArea[is.na(comb$MasVnrArea)] <- 0
comb$LotFrontage[is.na(comb$LotFrontage)] <- 0

#fix factor_var's na based on data_description 
comb[,fac_var]<-lapply(comb[,fac_var],as.character) #convert factor variables to character

comb$Alley[is.na(comb$Alley)]<-'na'  #No alley access
comb$BsmtQual[is.na(comb$BsmtQual)]<-'na' #No Basement
comb$BsmtCond[is.na(comb$BsmtCond)]<-'na' #No Basement
comb$BsmtExposure[is.na(comb$BsmtExposure)]<-'na'#No Basement
comb$BsmtFinType1[is.na(comb$BsmtFinType1)]<-'na'#No Basement
comb$BsmtFinType2[is.na(comb$BsmtFinType2)]<-'na'#No Basement
comb$FireplaceQu[is.na(comb$FireplaceQu)]<-'na'#No Fireplace
comb$GarageType[is.na(comb$GarageType)]<-'na'#No Garage
comb$GarageQual[is.na(comb$GarageQual)]<-'na'#No Garage
comb$GarageCond[is.na(comb$GarageCond)]<-'na'#No Garage
comb$GarageFinish[is.na(comb$GarageFinish)]<-'na'#No Garage
comb$PoolQC[is.na(comb$PoolQC)]<-'na'#No Pool
comb$Fence[is.na(comb$Fence)]<-'na'#No Fence
comb$MiscFeature[is.na(comb$MiscFeature)]<-'na'#None
comb$MasVnrType[is.na(comb$MasVnrType)]<-'None'  #there is a none category, so just make na to none


na_list<-colSums(is.na(comb)) 
na_list[na_list!=0] #what remaining na we need to deal with:
#MSZoning    Utilities  Exterior1st  Exterior2nd   BsmtFinSF1   BsmtFinSF2    BsmtUnfSF 
#4            2            1            1            1            1            1 
#TotalBsmtSF   Electrical BsmtFullBath BsmtHalfBath  KitchenQual   Functional   GarageCars 
#1            1            2            2            1            2            1 
#GarageArea     SaleType    SalePrice 
#1            1         1459 

comb[is.na(comb$BsmtFinSF1),] #id2121 has no basement, thus all below are 0
comb$BsmtFinSF1[is.na(comb$BsmtFinSF1)]<-0
comb$BsmtFinSF2[is.na(comb$BsmtFinSF2)]<-0
comb$BsmtUnfSF[is.na(comb$BsmtUnfSF)]<-0
comb$TotalBsmtSF[is.na(comb$TotalBsmtSF)]<-0
comb$BsmtFullBath[is.na(comb$BsmtFullBath)]<-0
comb$BsmtHalfBath[is.na(comb$BsmtHalfBath)]<-0

comb[is.na(comb$GarageCars),] #id 2577 has no garage, thus all below are 0
comb$GarageCars[is.na(comb$GarageCars)]<-0
comb$GarageArea[is.na(comb$GarageArea)]<-0

#all below with ? in comment may need to try other way of removing na
comb$SaleType[is.na(comb$SaleType)]<-'Oth' #?since there is 'other', just group the 1 case as 'other'.

comb$Functional[is.na(comb$Functional)]<-'Typ' #?there are 2 na. Just assume them as typical.
comb$KitchenQual[is.na(comb$KitchenQual)]<-'TA'#?max count
comb$MSZoning[is.na(comb$MSZoning)]<-'RL'#? replacing with the max count type
comb$Utilities[is.na(comb$Utilities)]<-'AllPub' #?max count

comb%>%group_by(Exterior2nd)%>%summarise(no_rows=length(Exterior2nd))
comb$Exterior2nd[is.na(comb$Exterior2nd)]<-'VinylSd' #?max count

comb%>%group_by(Exterior1st)%>%summarise(no_rows=length(Exterior1st))
comb$Exterior1st[is.na(comb$Exterior1st)]<-'VinylSd' #?max count

comb%>%group_by(Electrical)%>%summarise(no_rows=length(Electrical))
comb$Electrical[is.na(comb$Electrical)]<-'SBrkr'  #?max count

na_list<-colSums(is.na(comb)) 
na_list[na_list!=0]           #we only have SalePrice left with na/

comb[,fac_var]<-lapply(comb[,fac_var],factor) #convert character variables to factors  

comb_dum <- dummy.data.frame(comb, sep = ".")

# --------------- Separate Train for Cross validation ----------
mytrain=comb_dum[comb_dum$dataset=='train',c(1:80,82)]
mytrain.index=c(sample(1:1460,730))
mytrain.1<-mytrain[mytrain.index,]
mytest.1<-mytrain[-mytrain.index,]

# --------------- Parametric ----------------
# Create the original regression model using all variables
lm1 <- lm(SalePrice ~., data=mytrain.1)
summary(lm1) # Adjusted R-squared:  0.9337

# Test the model on the validation set
test.lm1 <- lm(SalePrice ~.,data=mytest.1)
summary(test.lm1) # Adjusted R-squared:  0.9186
mse1 <- mean(test.lm1$residuals^2)
mse1 # 332500730

# Trying to find the best fit by running all possible combination of variables
# compare the significance level of each variable and the value for adjusted R-squared
library(MASS)
fit <- lm(SalePrice ~., data=mytrain.1)
step <- stepAIC(fit, direction="both")
step$anova

# Determine another regression model based on stepwise fit
lm2 <- lm(SalePrice ~ Id + MSSubClass + MSZoning + LotArea + Street + LotShape + 
            LandContour + Utilities + LotConfig + Neighborhood + Condition1 + 
            Condition2 + HouseStyle + OverallQual + OverallCond + YearBuilt + 
            RoofMatl + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
            BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
            BsmtUnfSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + FullBath + 
            BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
            Functional + FireplaceQu + GarageCars + GarageArea + GarageCond + 
            WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch + 
            PoolQC + SaleCondition, data=mytrain.1)
summary(lm2) # Adjusted R-squared:  0.9312

# Test the model on the validation set
test.lm2 <- lm(SalePrice ~ Id + MSSubClass + MSZoning + LotArea + Street + LotShape + 
                 LandContour + Utilities + LotConfig + Neighborhood + Condition1 + 
                 Condition2 + HouseStyle + OverallQual + OverallCond + YearBuilt + 
                 RoofMatl + MasVnrType + MasVnrArea + ExterQual + ExterCond + 
                 BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
                 BsmtUnfSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + FullBath + 
                 BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
                 Functional + FireplaceQu + GarageCars + GarageArea + GarageCond + 
                 WoodDeckSF + OpenPorchSF + EnclosedPorch + ScreenPorch + 
                 PoolQC + SaleCondition, data=mytest.1)
summary(test.lm2) # Adjusted R-squared:  0.9373
mse2 <- mean(test.lm2$residuals^2)
mse2 # 375873720

# Using the better model (larger R-squared value and smaller mse) to predict
predictions <- as.vector(predict(lm2, newdata=comb_dum[comb_dum$dataset=='test',1:80]))
table <- data.frame(test$Id, predictions)
colSums(is.na(table))  #no weird predictions
write.table(table, file="housepricesprediction1.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

# --------------- Non-Parametric ------------


# ==================================================
#references:
#https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r/notebook
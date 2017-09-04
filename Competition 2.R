setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-house-prices")
library(readr)
library(dplyr)
library(dummies)
library(class)
library(lubridate)
#install.packages('xgboost')
library(xgboost)
library(MASS)
# ================= Data Cleaning =================
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

comb_dum <- dummy.data.frame(comb, sep = ".") #make all factor variables dummy (0/1)

# --------------- Separate Train for Cross validation ----------
alltrain=comb_dum[comb_dum$dataset.train==1,c(1:303,306)]
alltest=comb_dum[comb_dum$dataset.test==1,c(1:303,306)]
mytrain.index=c(sample(1:1460,730))
mytrain<-alltrain[mytrain.index,]
mytest<-alltrain[-mytrain.index,]

# ================= Parametric (lm) =================
# Create the original regression model using all variables
lm1 <- lm(SalePrice ~., data=mytrain)
summary(lm1) # Adjusted R-squared:  0.9363 

# Test the model on the validation set
test.lm1 <- lm(SalePrice ~.,data=mytest)
summary(test.lm1) # Adjusted R-squared:  0.9323 
mse1 <- mean(test.lm1$residuals^2)
mse1 # 244,012,820

# Trying to find the best fit by running all possible combination of variables
# compare the significance level of each variable and the value for adjusted R-squared
# this is very slow
fit <- lm(SalePrice ~., data=mytrain)
step <- stepAIC(fit, direction="both")
step$anova

# Determine another regression model based on stepwise fit
lm2 <- lm(SalePrice ~ `MSZoning.C (all)` + MSZoning.FV + LotFrontage + 
            LotArea + Street.Grvl + LotShape.IR2 + LandContour.Low + 
            Utilities.AllPub + LotConfig.CulDSac + LandSlope.Gtl + LandSlope.Mod + 
            Neighborhood.Blueste + Neighborhood.CollgCr + Neighborhood.Crawfor + 
            Neighborhood.Edwards + Neighborhood.Gilbert + Neighborhood.IDOTRR + 
            Neighborhood.Mitchel + Neighborhood.NAmes + Neighborhood.NoRidge + 
            Neighborhood.NridgHt + Neighborhood.NWAmes + Neighborhood.OldTown + 
            Neighborhood.StoneBr + Condition1.Artery + Condition1.Feedr + 
            Condition1.RRAe + Condition2.Feedr + Condition2.PosN + Condition2.RRAe + 
            BldgType.1Fam + BldgType.2fmCon + BldgType.Twnhs + HouseStyle.1.5Fin + 
            HouseStyle.2.5Fin + HouseStyle.2.5Unf + HouseStyle.2Story + 
            OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle.Flat + 
            RoofStyle.Gable + RoofStyle.Gambrel + RoofStyle.Hip + RoofStyle.Mansard + 
            RoofMatl.Metal + Exterior1st.BrkComm + Exterior1st.BrkFace + 
            Exterior1st.CemntBd + Exterior1st.HdBoard + Exterior2nd.AsphShn + 
            Exterior2nd.CmentBd + Exterior2nd.Plywood + MasVnrType.BrkCmn + 
            MasVnrType.BrkFace + MasVnrArea + ExterQual.Ex + ExterCond.Gd + 
            Foundation.Slab + BsmtQual.Gd + BsmtQual.na + BsmtCond.Fa + 
            BsmtCond.Gd + BsmtCond.Po + BsmtExposure.Gd + BsmtFinType1.ALQ + 
            BsmtFinType1.BLQ + BsmtFinType1.LwQ + BsmtFinType1.Rec + 
            BsmtFinSF1 + BsmtFinType2.ALQ + BsmtFinType2.LwQ + BsmtFinSF2 + 
            BsmtUnfSF + HeatingQC.Po + Electrical.FuseP + X1stFlrSF + 
            X2ndFlrSF + BedroomAbvGr + KitchenQual.Ex + Functional.Maj1 + 
            Functional.Maj2 + Functional.Min1 + Functional.Mod + Fireplaces + 
            FireplaceQu.na + GarageType.Attchd + GarageType.Basment + 
            GarageType.BuiltIn + GarageType.CarPort + GarageType.Detchd + 
            GarageYrBlt + GarageFinish.RFn + GarageCars + GarageQual.Fa + 
            WoodDeckSF + OpenPorchSF + ScreenPorch + PoolArea + PoolQC.Ex + 
            Fence.GdPrv + MiscFeature.na + MoSold + SaleType.CWD + SaleCondition.Abnorml + 
            SaleCondition.Alloca + SaleCondition.Family + SaleCondition.Normal + 
            ExterQual.TA, data=mytrain)
summary(lm2) # Adjusted R-squared:  0.8827

# --------------- Test the model on the validation set ---------------
test.lm2 <- lm(SalePrice ~ `MSZoning.C (all)` + MSZoning.FV + LotFrontage + 
                 LotArea + Street.Grvl + LotShape.IR2 + LandContour.Low + 
                 Utilities.AllPub + LotConfig.CulDSac + LandSlope.Gtl + LandSlope.Mod + 
                 Neighborhood.Blueste + Neighborhood.CollgCr + Neighborhood.Crawfor + 
                 Neighborhood.Edwards + Neighborhood.Gilbert + Neighborhood.IDOTRR + 
                 Neighborhood.Mitchel + Neighborhood.NAmes + Neighborhood.NoRidge + 
                 Neighborhood.NridgHt + Neighborhood.NWAmes + Neighborhood.OldTown + 
                 Neighborhood.StoneBr + Condition1.Artery + Condition1.Feedr + 
                 Condition1.RRAe + Condition2.Feedr + Condition2.PosN + Condition2.RRAe + 
                 BldgType.1Fam + BldgType.2fmCon + BldgType.Twnhs + HouseStyle.1.5Fin + 
                 HouseStyle.2.5Fin + HouseStyle.2.5Unf + HouseStyle.2Story + 
                 OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle.Flat + 
                 RoofStyle.Gable + RoofStyle.Gambrel + RoofStyle.Hip + RoofStyle.Mansard + 
                 RoofMatl.Metal + Exterior1st.BrkComm + Exterior1st.BrkFace + 
                 Exterior1st.CemntBd + Exterior1st.HdBoard + Exterior2nd.AsphShn + 
                 Exterior2nd.CmentBd + Exterior2nd.Plywood + MasVnrType.BrkCmn + 
                 MasVnrType.BrkFace + MasVnrArea + ExterQual.Ex + ExterCond.Gd + 
                 Foundation.Slab + BsmtQual.Gd + BsmtQual.na + BsmtCond.Fa + 
                 BsmtCond.Gd + BsmtCond.Po + BsmtExposure.Gd + BsmtFinType1.ALQ + 
                 BsmtFinType1.BLQ + BsmtFinType1.LwQ + BsmtFinType1.Rec + 
                 BsmtFinSF1 + BsmtFinType2.ALQ + BsmtFinType2.LwQ + BsmtFinSF2 + 
                 BsmtUnfSF + HeatingQC.Po + Electrical.FuseP + X1stFlrSF + 
                 X2ndFlrSF + BedroomAbvGr + KitchenQual.Ex + Functional.Maj1 + 
                 Functional.Maj2 + Functional.Min1 + Functional.Mod + Fireplaces + 
                 FireplaceQu.na + GarageType.Attchd + GarageType.Basment + 
                 GarageType.BuiltIn + GarageType.CarPort + GarageType.Detchd + 
                 GarageYrBlt + GarageFinish.RFn + GarageCars + GarageQual.Fa + 
                 WoodDeckSF + OpenPorchSF + ScreenPorch + PoolArea + PoolQC.Ex + 
                 Fence.GdPrv + MiscFeature.na + MoSold + SaleType.CWD + SaleCondition.Abnorml + 
                 SaleCondition.Alloca + SaleCondition.Family + SaleCondition.Normal + 
                 ExterQual.TA, data=mytest)
summary(test.lm2) # Adjusted R-squared:  0.9306
mse2 <- mean(test.lm2$residuals^2)
mse2 # 313,409,832

# Using the better model (larger R-squared value and smaller mse) to predict
lm1 <- lm(SalePrice ~., data=alltrain)
predictions <- as.vector(predict(lm1, newdata=alltest))
table <- data.frame(test$Id, predictions)
colSums(is.na(table))  #no weird predictions
write.table(table, file="housepricesprediction.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

# ================= Non-Parametric (KNN) =================
normalize <-function(x) {
  return( (x- min(x)) / (max(x)-min(x)) )
}

# --------------- Pre-processsing - Normalize ---------------
cl=which(names(comb_dum) %in% numeric_var)  #find the column number for numeric_var
cl=cl[2:length(cl)-1] #exclude id & salesprice
comb_n<-comb_dum
#normalize numeric_var columns and combine with non-numeric_var
comb_n<-cbind(comb_dum[,-cl],as.data.frame(lapply(comb_n[,cl],normalize))) 

#--------------- create testing set(my_train) & validation set(my_validation) ---------------
#exclude column dataset.train, dataset.test and sales price
my_train <- comb_n[comb_n$dataset.train == 1,][mytrain.index,c(2:266,271:306)]
my_test <- comb_n[comb_n$dataset.train == 1,][-mytrain.index,c(2:266,271:306)]

#find related salesprices for testing set and validation set.
my_train_saleprice <- comb_n[comb_n$dataset.train == 1,][mytrain.index,269]
my_test_saleprice <- comb_n[comb_n$dataset.train == 1,][-mytrain.index,269]

require(class)
#sqrt(1460) # assume k approximates sqrt(1460) =38

# --------------- what is a good k? ---------------
#store p-values for different k
p_value=c()
for (i in 1:100)
{
  ml<-knn(train = my_train,my_test,cl=my_train_saleprice,k=i)
  t = qt(0.975,length(my_train-2))
  a=as.numeric(as.character(ml))
  b=my_train_saleprice
  t=t.test(a,b)
  p_value <- c(p_value,t$p.value)
 }
 plot(p_value,type='b')
 which(p_value>0.05)
 #1  2  5  7  8 11 14 17
 # for mytest: when k = 15, p_value achieves maximum.
 p_value[which(p_value==max(p_value))]
 # p-value = 0.1757097942, dont reject the null and state that the result for cross-validation is not significantly different from the test result. 
 
 
 #store sse
sse_list<-c()
for (i in 1:20) 
{
  ml<-knn(train = my_train,my_test,cl=my_train_saleprice,k=i)
  a=as.numeric(as.character(ml))
  b=my_test_saleprice
  mysse=sum((a-b)^2)
  sse_list <- c(sse_list,mysse)
}
plot(sse_list,type='b')
# 5 is the second lowest .

names(comb_n)
# --------------- Predict ---------------
ml_pred<-knn(train = comb_n[comb_n$dataset.train == 1,][,c(2:266,271:306)],comb_n[comb_n$dataset.test == 1,][,c(2:266,271:306)],cl=comb_n[comb_n$dataset.train == 1,][,269],k=5)
table2 <- data.frame(test$Id, ml_pred)
write.table(table2, file="housepricesprediction_ml.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")

#================= XGBOOST ========================

train_x=alltrain[,1:length(alltrain)-1]
test_x=alltest
train_x[] <- lapply(train_x, as.numeric)
test_x[]<-lapply(test_x, as.numeric)

dtrain=xgb.DMatrix(as.matrix(train_x),label= alltrain$SalePrice)
dtest=xgb.DMatrix(as.matrix(test_x))


# --------------- xgboost parameters ---------------
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=150 # try more rounds

# --------------- train data  ---------------
gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
xgb_preds=predict(gb_dt,dtest)
table_xgb=data.frame(test$Id, xgb_preds)
write.table(table_xgb, file="housepricesprediction_xgb.csv", row.names=F, col.names=c("Id", "SalePrice"), sep=",")


# ==================================================
#references:
#https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r/notebook
#https://www.kaggle.com/shashankakki/xgboost-mice-implementation-in-r
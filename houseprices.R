#setwd("/Users/Pan/Google Drive/Data Science/SYS 6018/sys6018-competition-house-prices")
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

# --------------- Parametric ----------------
lm1 <- lm(SalePrice ~., data=train)
summary(lm1)
# --------------- Non-Parametric ------------


# ==================================================
#references:
#https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
#https://www.kaggle.com/notaapple/detailed-exploratory-data-analysis-using-r/notebook
library(readr)
library(xgboost)
library(caret)
library(readr)
library(xgboost)
library(ggplot2)
library(R.utils)
library(gridExtra)
library(lubridate)
library(data.table)
library(Matrix)
require(plyr)
require(Hmisc)
library(maps)
library(maptools)
library(sp)
library(corrplot)
set.seed(294)

time.1 <- Sys.time()
format(time.1, "%d-%m-%Y--%H:%M:%S")


train <- read_csv("../data/train.csv")

train_tr <- train[1:round(dim(train)[1]*0.05),]

dim(train_tr)

y = train_tr$target

train_tr = subset(train_tr,select = -c(ID, target))

#row_count = countLines("../data/train.csv") 
#cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))

train = train_tr
length(train[is.na(train)])/(ncol(train)*nrow(train)) 

nrow(train) - nrow(unique(train))

col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))

train = train[, !names(train) %in% names(col_ct[col_ct==1])]

train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
str(lapply(train_char, unique), vec.len = 10)


train_char[train_char==-1] = NA
train_char[train_char==""] = NA
train_char[train_char=="[]"] = NA

train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]

train_char = train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)

train_time = train_date[,colnames(train_date) %in% c("VAR_0204","VAR_0217")]
train_time = data.frame(sapply(train_time, function(x) strftime(x, "%H:%M:%S")))
train_hour = as.data.frame(sapply(train_time, function(x) as.numeric(as.character(substr( x ,1, 2)))))

par(mar=c(2,2,2,2),mfrow=c(4,4))
for(i in 1:16) hist(train_date[,i], "weeks", format = "%d %b %y", main = colnames(train_date)[i], xlab="", ylab="")





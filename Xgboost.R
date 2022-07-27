## Xgboost model with different data sets about clients consumption
## July 2022

## Working directory 

setwd("~/Desktop/wetransfer_hackathon-semana-2-julio_2022-07-11_1409/BASE DE DATOS")

##Libraries
library(readr)
library(writexl)
library(vtable)
library(textshape)
library(tidyverse)
library(dplyr)
library(sqldf)
library(eeptools)
library(xgboost)
library(caret)
library(corrplot)
library(ggcorrplot)
## Import datasets

clients <- read.table("clients_table.txt", sep = ",")

credit_score <- read.table("credit_score_table.txt", sep = ",")

products <- read.table("products_table.txt", sep = ",")

transactions <- read.table("transactions_table.txt", sep = ",")

## Modificar nombres de las columnas

names(clients) <- c("CustomerID", "Surname","Geography","Gender","HasCrCard","IsActivemember","Estimated Salary","applicationdate",
"exitdate","birthdate")

names(credit_score) <- c("Customerid","Date","Score")

names(products) <- c("Contractid","Customerid","Products")

names(transactions) <- c("Customerid","Transactions","Value")

## Take out row 1

clients <- clients[- (1),]

credit_score <- credit_score[- (1),]

products <- products[- (1),]

transactions <- transactions[- (1),]

##Descriptive analysis

summary(clients)

## Change character to numeric in the clients data frame

clients$`Estimated Salary` <- as.numeric(clients$`Estimated Salary`)
clients$IsActivemember <- as.numeric(clients$IsActivemember)
clients$HasCrCard<- as.numeric(clients$HasCrCard)

## FILTER DATA

## Take out nulls

clients <- na.omit(clients)

### Contracts from 2015 onwards.

clients <- clients %>%
  filter(applicationdate >= "2015-01-01")

### Take out clients with more than one contract

clients <- clients %>% 
  distinct(CustomerID, .keep_all = TRUE)

## Save clients with 2 or more years 

clients <- sqldf("
        SELECT
            *,
            strftime('%Y,%m,%d', exitdate) - strftime('%Y,%m,%d',applicationdate) AS Years
        FROM clients"
)


## Transform nulls with 2
clients <- mutate_at(clients, c("Years"), ~replace(., is.na(.), 2))

## Quit rows with less than 2 years

clients <- clients %>%
  filter(Years >= 2)


### Take number of products per customer and calculate their statistics

products <- sqldf("
        SELECT Customerid, count(*) AS quantity
        FROM products
        GROUP BY Customerid"
)

summary(products$quantity)

### Take balance account per customer 

transactions <- sqldf("
        SELECT Customerid, sum(Value) AS balance
        FROM transactions
        GROUP BY Customerid"
)

summary(transactions$balance)

### Take score from the credit_score

credit_score <- sqldf("
        SELECT Customerid, Score
        FROM credit_score
        GROUP BY Customerid"
)

credit_score$Score<- as.numeric(credit_score$Score)

summary(credit_score$Score)

### Take age from clients data base

clients$birthdate <- as.Date(clients$birthdate)

clients <- clients %>% 
  mutate(Age = age_calc(birthdate, as.Date("2022-07-17"), units = "years" ))

summary(clients$Age)

## Join variables from other tables to the clients table


### Take quantity of products, balance from transactions and score from credit_score
data <- sqldf("
              SELECT *
              FROM clients 
              LEFT JOIN products 
              ON clients.CustomerID = products.CustomerID")

data <- data [,-13]

## Transactions

data <- sqldf("
              SELECT *
              FROM data 
              LEFT JOIN transactions 
              ON data.CustomerID = transactions.CustomerID")


data <- data [,-14]

## Credit score

data <- sqldf("
              SELECT *
              FROM data
              LEFT JOIN credit_score
              ON data.CustomerID = credit_score.CustomerID")

data <- data [,-15]

## Take out some columns and transform geography and gender

data <- data [,-2]
data <- data [,-8]

### Transform gender 

vecgender <- c(data$Gender)

vecgender <- as.factor(vecgender)

data <- cbind(data, vecgender)

data <- data [,-3]

str(data)

data$vecgender <- as.numeric(data$vecgender)

## Transform countries to factors

veccountries <- c(data$Geography)

veccountries <- as.factor(veccountries)

data <- cbind(data, veccountries)

data <- data [,-2]

data$veccountries <- as.numeric(data$veccountries)

## Put customer id as index

data<-textshape::column_to_rownames(data, loc = 1)
View(data)

## Take out birthdate and application date

data <- data [,-5]
data <- data [,-4]


## Scale data

data2 <- scale(data)

data2 <- as.data.frame(data2)

### MODEL AND DESCRIPTIVE STATISTICS

st(data, title = "**Statistics**")

## Corrplot

M<-cor(data[ , -c(4, 5)])

corrplot(M, method="number")

## Develop a XGboost model 

#make this example reproducible
set.seed(1234)

#split into training (80%) and testing set (20%)
parts = createDataPartition(data2$IsActivemember, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[, -2])
train_y = train[,2]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -2])
test_y = test[, 2]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, watchlist=watchlist, nrounds = 70)

## final model 

final = xgboost(data = xgb_train, max.depth = 3, nrounds = 23, verbose = 1)

final

## prediction 
pred_y = predict(final, xgb_test)

mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse


## plot 

xgb.plot.shap(data = as.matrix(train_x),
              model = final,
              top_n = 5)

# Compute feature importance matrix

importance_matrix = xgb.importance(colnames(xgb_train), model = model)
importance_matrix

# Nice graph
xgb.plot.importance(importance_matrix[1:11,], main = "Importance per variable")










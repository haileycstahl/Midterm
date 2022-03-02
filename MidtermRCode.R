### Using Linear regression
### Predict the Price (Price) for used Toyota Corolla cars.
### Use the follwoing three predictors Age_08_04, KM, Fuel_Type
library(forecast)

data <- read.csv("ToyotaCorolla.csv")

#1 is petrol, 0 diseal
data$Fuel_Type_cat<-ifelse(data$Fuel_Type =="Petrol",1,0)

datasub<-subset(data,select = c("Age_08_04","KM","Fuel_Type_cat","Price"))
View(datasub)

# partition data into Training (70%) and Validation (30%) datasets
set.seed(1)
train_row <- sample(rownames(datasub), dim(datasub)[1]*0.70)
train_data <- datasub[train_row, ]

valid_rows <- setdiff(rownames(datasub), train_row) 
valid_data <- datasub[valid_rows, ]

View(valid_data)
View(train_data)


# use lm() to run a linear regression of Price on the 

reg<-lm(Price~data$Age_08_04+KM+Fuel_Type_cat,data=datasub,subset =train_row)

tr.res <- data.frame(train_data$Price, reg$fitted.values, reg$residuals)
print(head(tr.res))

#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)

# Display the results (summary()) of the linear regression
options(scipen = 999)
print(summary(reg))

# use predict() to make predictions on a new set. 
pred <- predict(reg, data= valid_data)


# use accuracy() to compute common accuracy measures.
options(scipen = 999)
accuracy(pred, valid_data$Price)


#accuracy of training
options(scipen = 999)
accuracy(reg$fitted.values, train_data$Price)



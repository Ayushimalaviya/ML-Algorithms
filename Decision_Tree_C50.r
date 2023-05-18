#installed the dependencies and packages
install.packages('libcoin', dependencies = T)
install.packages('C50', dependencies = T)

#initiate by calling library C50
library(C50)
library(caret)

#load the data
dat <- read.csv("breast-cancer-wisconsin.csv", na.string = "?")

#count number of missing values to decide wether to replace or remove the missing values
query1 <- sum(is.na(dat))
print(query1)

#deleted rows as it was less than 1% of the data
Rows_deleted <- na.omit(dat)
Rows_deleted <- na.omit(dat)
data <- data.frame(Rows_deleted)

#to convert class as factor type column and present the features details
Feature <- c("F1", "F2","F3","F4","F5","F6","F7","F8","F9")
data$Class<-as.factor(data$Class)
str(data[, c(Feature, "Class")])

#data parted to train and test set
data1<-sort(sample(nrow(data),as.integer(.70*nrow(data))))
train <- data[data1,]
test <- data[-data1,]

#training the c5.0 model(to boost we use trails parameter in the function)
C5DT <- C5.0(x = train[, Feature], y = train$Class)

#summary of the model
summary(C5DT)

#ploting of C5.0 tree
plot(C5DT)

# assigned matrix to evaluate cost sensitive model
cost_mat <- matrix(c(0, 2, 1, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("2", "4")
cost_mat

#cost sensitive model which try to decrease the malignant class error and presnt the chances of malignant at higher rate
cost_mod <- C5.0(x = train[, Feature], y = train$Class, costs = cost_mat)
summary(cost_mod)

#more sample predicted as malignant
y_pred <-predict(cost_mod, test[, Feature])
table(y_pred)

#using simple C5DT prediction
y_predict <- predict(C5DT, test[, Feature])
table(y_predict)

#confusion matrix presentatipn for cost sensitive model
confusionMatrix(predict(cost_mod, test), y_pred)

confusionMatrix(predict(C5DT, test), y_predict)

print(getwd())
setwd("C:/Users/wsarv/ADM/Assignment_2/Datasets")
print(getwd())
data <- read.csv("WinnipegDataset_PCA_GA_revised_3.csv")
library(ISLR)
require(tree)
hist(data$f1)
install.packages("tree")
tree.carseats = tree(-label, data=data)
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(label~., data = data, method = 'class')
rpart.plot(fit, extra = 106)
rpart.rules(fit)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$label))
prop.table(table(data_test$label))
fit <- rpart(label~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$label, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
rpart.rules(fit)


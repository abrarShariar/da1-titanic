# load raw data from csv
train <- read.csv("train.csv", header = TRUE)
# scatter plot with depended variable y and independent variable x
scatter.smooth(x=train$pclass, y=train$survived, main="Pclass ~ Survived")

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(train$survived, main="Survived", sub=paste("Outlier rows: ", boxplot.stats(train$survived)$out))  # box plot for 'survived'
boxplot(train$pclass, main="Pclass", sub=paste("Outlier rows: ", boxplot.stats(train$pclass)$out))  # box plot for 'pclass'


first_class_percent <- (length(which(train$pclass == 1)) / nrow(train)) * 100 
not_first_class <- 100 - first_class_percent

# so you had a 38% chance of surviving the Titanic
survived_percent <-(length(which(train$survived == 1)) / nrow(train)) * 100
not_sruvived <- 100 - survived_percent

head(train)
# survived and male
# oops, you had 18% chance of surving the Titanic as male
survived_male <- (length(which(train$survived == 1 & train$sex == "male")) / nrow(train[which(train$sex == 'male'),])) * 100




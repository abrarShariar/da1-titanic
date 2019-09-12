# load raw data from csv
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# add "Survival" column in the test set and merge with exsisting rows of test
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# combine data sets (train + test.survived)
data.combined <- rbind(train, test.survived)

# check the data type of the R obj
str(data.combined)

# change the pclass column type to factor
data.combined$pclass <- as.factor(data.combined$pclass)
data.combined$survived <- as.factor(data.combined$survived)

# frequency/distribution calculate
table(data.combined$survived)
# 2 way cross table 
table(data.combined$sex, data.combined$survived)

# visulization
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = train$pclass, fill = factor(train$survived))) +
  geom_histogram(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")






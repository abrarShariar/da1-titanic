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
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# examine the first few names in the train data set
head(as.character(train$name))
# how many unique names are there across both tran and test
unique_name <- length(unique(data.combined$name))
# not unique
length(data.combined$name) - unique_name
# finding the duplicated data
dup_names <- as.character(data.combined[which(duplicated(data.combined$name)), 'name'])
# the no of rows which contain the dup_names
rows_dup <- which(data.combined$name %in% dup_names)
# duplicate rows in the data.combined
data.combined[rows_dup,]

# detect the "Miss" keyword in data.combined
library(stringr)
misses <- data.combined[which(str_detect(data.combined$name, "Miss")),]
misses[1:5,]

# number of males on board titanic
length(which(data.combined$sex == 'male'))

all_male_set <- data.combined[which(data.combined$sex == 'male'),]
all_male_set[1:5,]


# is there a relation between age and pclass?
all_males_surivived = data.combined[which(data.combined$sex == 'male' & data.combined$survived == 1),]
m_above_40 <- all_males_surivived[which(all_males_surivived$age > 40),]
# table(all_males_surivived$age, all_males_surivived$pclass)
ggplot(all_males_surivived, aes(x = all_males_surivived$age, fill = factor(all_males_surivived$pclass))) +
  geom_bar(width = 0.4) +
  xlab("Age") +
  ylab("Pclass") +
  labs(fill = "Age")






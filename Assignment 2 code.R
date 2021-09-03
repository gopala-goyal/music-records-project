# Source of data and code: Dimitris Bertsimas @ MIT

MusicRecord<-read.csv(file.choose()) #load data

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(MusicRecord$artistname == "Michael Jackson")

# Alternatively, use the pipe %>% function in "dplyr" package
library(dplyr)
MusicRecord %>% filter(artistname == "Michael Jackson") %>% summarize(count = n())

# first use the filter function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", 
# consisting of the 2010 song releases.
SongsTrain = MusicRecord %>% filter(year <= 2009)
SongsTest = MusicRecord %>% filter(year == 2010)

# we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. 
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# build a logistic regression model to predict Top10 using the training data. 
# We can now use "." in place of enumerating all the remaining independent variables in the following way:
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1)

# True or False?
# 1. The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
# 2. In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does our model suggest in terms of complexity?


# You can make predictions on the test set by using the command:
testPredict = predict(SongsLog1, newdata=SongsTest, type="response")

# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.15)

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
# What is the prediction accuracy of the model?

# To generate the ROC curve
install.packages("pROC")
library(pROC)
test_prob = predict(SongsLog1, newdata = SongsTest, type = "response")
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)


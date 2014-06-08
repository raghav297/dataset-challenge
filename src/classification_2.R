# Fixing problem of very high accuracy
# Possible overfitting on the training set.

# Using last saved:
# write.csv(all_data,"TrainingDataFinalAll.csv")
# write.csv(subset_all,"FeaturesFinalAll.csv")
library(plyr)

all_data = read.csv("TrainingDataFinalAll.csv", header=TRUE)
subset_all = read.csv("FeaturesFinalAll.csv", header = TRUE)

all_data$X = NULL
subset_all$X = NULL
all_data$stars = NULL
subset_all$stars = NULL

nr <- dim(all_data)[1]
# rows of data

all_data_ids <- (1:nr)

testing_set_indices <- sample(all_data_ids, 654)     # Generate a random sample of size 654
all_data_test = all_data[testing_set_indices,]

all_data_train  <- all_data[-testing_set_indices,]  # The set complement of training_set
write.csv(all_data_test,"all_data_test.csv")
write.csv(all_data_train,"all_data_train.csv")
# Training
set.seed(17)
cv.error.10=rep(0,10)
tobj <- tune.svm(dogs ~ ., data =all_data_train, gamma = 10^(-6:-3), cost = 10^(1:2))
print(summary(tobj))
bestGamma <- tobj$best.parameters[[1]]
bestC <- tobj$best.parameters[[2]]
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1
                           )

svmFit <- train(dogs ~ ., data = all_data_train,
                 method = "svmPoly",
                 trControl = fitControl
                )

save(svmFit, file = "learning3Model1.rda")
pred <- predict(svmFit, all_data_test[,1:32])
roundPred = round(pred)
print("Accuracy:")
print(mean(roundPred==all_data_test[,33]))

# ==============================
# Calculate precision and recall:

op = round(pred)
ip = all_data_test$dogs
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 1)
	{
		if(op[i] == ip[i] )
			res = res +1
	}
}
tp = res
print("TP:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 0)
	{
		if(op[i] != ip[i] )
			res = res +1
	}
}
fp = res
print("FP:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 1)
	{
		if(op[i] != ip[i] )
			res = res +1
	}
}
fn = res
print("FN:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 0)
	{
		if(op[i] == ip[i] )
			res = res +1
	}
}
tn = res
print("TN:")
print(res)

precision = tp/(tp+fp)
recall = tp/(tp+fn)
print(precision)
print(recall)

# ----------------------------

# Now trying to find what makes dogs dogs.

dogData = subset(all_data, all_data$dogs == 1)
dim(dogData)
catData = subset(all_data,all_data$dogs == 0)
dim(catData)
meanDog = colMeans(dogData[,1:32])
meanCat = colMeans(catData[,1:32])

meanFatDog = round(meanDog)
meanFatCat = round(meanCat)

# ================================================

# Now training on subset found out by best subset regression:

all_data = read.csv("TrainingDataFinalAll.csv", header=TRUE)
subset_all = read.csv("FeaturesFinalAll.csv", header = TRUE)

all_data$X = NULL
subset_all$X = NULL
all_data$stars = NULL
subset_all$stars = NULL

nr <- dim(all_data)[1]
# rows of data

all_data_ids <- (1:nr)

testing_set_indices <- sample(all_data_ids, 654)     # Generate a random sample of size 654
all_data_test = all_data[testing_set_indices,]

all_data_train  <- all_data[-testing_set_indices,]  # The set complement of training_set

subset_data_train = data.frame(all_data_train$desert,all_data_train$lunch,all_data_train$outdoor_seating,all_data_train$good_for_kids,all_data_train$wheelchair,all_data_train$UMM,all_data_train$take_outs,all_data_train$UMMR,all_data_train$waiter,all_data_train$credit_cards2,all_data_train$casual, all_data_train$dogs)
subset_data_test = data.frame(all_data_test$desert,all_data_test$lunch,all_data_test$outdoor_seating,all_data_test$good_for_kids,all_data_test$wheelchair,all_data_test$UMM,all_data_test$take_outs,all_data_test$UMMR,all_data_test$waiter,all_data_test$credit_cards2,all_data_test$casual, all_data_test$dogs)

# Training
set.seed(17)
cv.error.10=rep(0,10)
tobj <- tune.svm(all_data_train.dogs ~ ., data =subset_data_train, gamma = 10^(-6:-3), cost = 10^(1:2))
print(summary(tobj))
bestGamma <- tobj$best.parameters[[1]]
bestC <- tobj$best.parameters[[2]]
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1
                           )

svmFit <- train(all_data_train.dogs ~ ., data = subset_data_train,
                 method = "svmPoly",
                 trControl = fitControl
                )

save(svmFit, file = "learning3Model1.rda")
names(subset_data_test) = names(subset_data_train)
pred <- predict(svmFit, subset_data_test[,1:11])
roundPred = round(pred)
print("Accuracy:")
print(mean(roundPred==subset_data_test[,12]))

# ==============================
# Calculate precision and recall:

op = round(pred)
ip = subset_data_test$all_data_train.dogs
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 1)
	{
		if(op[i] == ip[i] )
			res = res +1
	}
}
tp = res
print("TP:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 0)
	{
		if(op[i] != ip[i] )
			res = res +1
	}
}
fp = res
print("FP:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 1)
	{
		if(op[i] != ip[i] )
			res = res +1
	}
}
fn = res
print("FN:")
print(res)
res = 0
for (i in 1:length(pred))
{
	if(ip[i] == 0)
	{
		if(op[i] == ip[i] )
			res = res +1
	}
}
tn = res
print("TN:")
print(res)

precision = tp/(tp+fp)
recall = tp/(tp+fn)
print(precision)
print(recall)









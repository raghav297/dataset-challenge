# Training on all features. 
# Predicting star values

all_ data_train = read.csv("all_data_train.csv", header=TRUE)

all_data_train$X = NULL

all_data_test = read.csv("all_data_test.csv",header= TRUE)
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
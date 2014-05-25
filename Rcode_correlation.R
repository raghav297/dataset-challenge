install.packages("plyr")
install.packages("maps")
install.packages("leaps")
install.packages("bestglm")
library(plyr)
library(maps)
library(leaps)
library(bestglm)

bizStarsFood = read.csv("bizStarsFood.csv", header=TRUE)
bidZip = read.csv("bid_to_zip.txt", sep = "\t", header = FALSE)
user_businessFood = read.csv("user_businessFood.csv")

colnames(bidZip) = c("businessId", "zip")
zipWithUMM = merge(bizStarsFood, bidZip, by="businessId")
corrTable = ddply(zipWithUMM, .(zip), summarize, cor(UMMR, CILowerStars))
#Omit zipcodes with one business since correlation is NA
newCorrTable = na.omit(corrTable)
colnames(newCorrTable) = c("zip", "corr")

highCorrZip = subset(newCorrTable, corr>=0.5)$zip
highCorrBiz = subset(zipWithUMM, zip %in% highCorrZip)

bizWithCorr = subset(bidZip, bidZip$zip %in% newCorrTable$zip)$businessId

# Business IDs in zip codes for which correlation was calculated
bizCorrTable = subset(user_businessFood, user_businessFood$businessId %in% bizWithCorr)

merged_corrTable = merge(bidZip, bizCorrTable)

#PhoenixMap <- qmap("phoenix", zoom = 10, source = "google")
#PhoenixMap + geom_point(aes(x = loangitude, y = latitude, color = zip), data = merged_corrTable)

initial_features = read.csv("Features4.csv")
colnames(initial_features)[2] <- "businessId"

#Merging UMM table with new features
common_features = merge(zipWithUMM, initial_features, by = "businessId")

corrFeatures = cor(common_features[c(4,5,6,9:38)])
w


# Best subset regression
Xy = common_features[c(4, 5, 9:38, 6)]
out <- bestglm(Xy)
out["BestModel"]

#Coefficients:
#  (Intercept)            UMM           UMMR    price_range  credit_cards2      take_outs          noise         casual          lunch  
#       0.7696       -20.6806         5.3360         0.3009         0.5413        -0.2516         0.1542         0.4578         0.4620  
#         wifi     wheelchair  
#       0.2051         0.6212 

XyWithoutUMM = Xy[c(2:33)]
out2 <- bestglm(XyWithoutUMM)

#Coefficients:
#    (Intercept)             UMMR      price_range    credit_cards2        take_outs           casual            lunch             wifi  
#      -0.002479         0.817139         0.293642         0.734543        -0.325318         0.579741         0.570728         0.228017  
#outdoor_seating       wheelchair  
#       0.286445         0.715851  


dfZip = split(common_features, common_features$zip)

# Subset regression method 2
regfit = regsubsets(CILowerStars~. , data = Xy, nvmax = 19)
regsummary = summary(regfit)
regsummary$rsq

par(mfrow=c(2,2))
plot(regsummary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(regsummary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
which.max(regsummary$adjr2)
#19
points(19,regsummary$adjr2[19], col="red",cex=2,pch=20)

plot(regsummary$cp ,xlab="Number of Variables ",ylab="Cp", type="l")
which.min(regsummary$cp )
#14
points(14,regsummary$cp [14],col="red",cex=2,pch=20)

which.min(regsummary$bic )
#10
plot(regsummary$bic ,xlab="Number of Variables ",ylab="BIC", type="l")

plot(regfit, scale="r2")
plot(regfit, scale="adjr2")
plot(regfit, scale="Cp")
plot(regfit, scale="bic")
coef(regfit, 10)

#  (Intercept)           UMM          UMMR   price_range credit_cards2     take_outs         noise        casual         lunch          wifi 
#    0.7696246   -20.6805792     5.3359552     0.3009276     0.5413141    -0.2515965     0.1542106     0.4578434     0.4619565     0.2050535 
#   wheelchair 
#    0.6212364 

# For prediction without UMM, UMMR
regfit = regsubsets(CILowerStars~.-UMM-UMMR , data = Xy, nvmax = 19)
regsummary = summary(regfit)
which.min(regsummary$bic)
#8
coef(regfit, 8)


# LM on attributes from first subset regression
lmfit = lm(CILowerStars~UMM+UMMR+price_range+credit_cards2+take_outs+noise+casual+lunch+wifi+wheelchair, data = Xy)
lmsummary= summary(lmfit)
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
#PhoenixMap + geom_point(aes(x = longitude, y = latitude, color = zip), data = merged_corrTable)

initial_features = read.csv("features3.csv")
colnames(common_business)[2] <- "businessId"

#Merging UMM table with new features
common_features = merge(zipWithUMM, features, by = "businessId")

corrFeatures = cor(common_features[c(5,6,9:24)])
write.csv(common_features, "all_features.csv")


# Best subset regression
Xy = common_features[c(4, 5, 9:24, 6)]
out <- bestglm(Xy)
out["BestModel"]

#Coefficients:
#  (Intercept)            UMM           UMMR    price_range  credit_cards2          noise          divey         casual  
#       0.8190       -21.5713         5.4855         0.3735         0.7877         0.1686         0.6971         0.9012


XyWithoutUMM = Xy[c(2:19)]
out2 <- bestglm(XyWithoutUMM)

#Coefficients:
#  (Intercept)           UMMR    price_range  credit_cards2          divey         classy         casual  
#      0.05169        0.76238        0.36454        1.08534        0.92722        0.82756        1.18974  


dfZip = split(common_features, common_features$zip)


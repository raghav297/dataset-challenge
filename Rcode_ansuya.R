install.packages("rjson")
install.packages("plyr")
install.packages("MASS")
library(rjson)
business= "yelp_academic_dataset_business.json"
raw_biz= scan(business, what="raw()", sep="\n")
#to make it human readable
biz_data= lapply(raw_biz, function(x) fromJSON(x))
#15585 businesses

#make a list of user data
user= "yelp_academic_dataset_user.json"
raw_user= scan(user, what="raw()", sep="\n")
user_data= lapply(raw_user, function(x) fromJSON(x))
#70817 users

#make a list of review data 
review= "yelp_academic_dataset_review.json"
raw_review= scan(review, what="raw()", sep="\n")
review_data= lapply(raw_review, function(x) fromJSON(x))
#335022 reviews

#playing with data 
#making lists for user data
user.avg_stars = unlist(lapply(user_data, function(x) x$average_stars))
user.id = unlist(lapply(user_data, function(x) x$user_id))
user.join_date= unlist(lapply(user_data, function(x) x$yelping_since))
user.join_date= paste(user.join_date, "-01", sep='')
user.join_date= as.Date(user.join_date)
user.review_count=unlist(lapply(user_data, function(x) x$review_count))
user.vote_funny= unlist(lapply(user_data, function(x) x$votes$funny))
user.vote_cool= unlist(lapply(user_data, function(x) x$votes$cool))
user.vote_useful= unlist(lapply(user_data, function(x) x$votes$useful))
user.fans= unlist(lapply(user_data, function(x) x$fans))

#making a data frame from user lists
user_table= data.frame(user.id, user.join_date, user.avg_stars, user.review_count, user.vote_funny, 
	user.vote_cool, user.vote_useful, user.fans)
colnames(user_table)=c("userId", "joinDate", "avgStars", "reviewCount", "voteFunny", "voteCool", "voteUseful", "fans")


#making lists for business data
biz_id= unlist(lapply(biz_data, function(x) x$business_id))
biz_long= unlist(lapply(biz_data, function(x) x$longitude))
biz_lat= unlist(lapply(biz_data, function(x) x$latitude))
biz_category=list(); for(i in 1:15585){ biz_category[[i]]=biz_data[[i]]$categories}
biz_stars=unlist(lapply(biz_data, function(x) x$stars))
biz_review_count=unlist(lapply(biz_data, function(x) x$review_count))

biz_id_zip=read.table("bid_to_zip.txt", sep="\t", header=FALSE)
colnames(biz_id_zip)= c("business_id", "zip")
biz_id_main_cat=read.table("main_categories.txt", sep="\t", header=FALSE)
colnames(biz_id_main_cat)= c("business_id", "main_cat")

bizStars=data.frame(biz_id, biz_stars)
colnames(bizStars)=c("businessId", "stars")
#taking business for food and restaurants
bizFood= subset(biz_id_main_cat, biz_id_main_cat$main_cat %in% c("Food", "Restaurants"))$business_id


library(plyr)
biz_main_cat_count=ddply(biz_id_main_cat, .(main_cat), summarise, count=length(main_cat))
biz_zip_count=ddply(biz_id_zip, .(zip), summarise, count=length(zip))

biz_long_lat= data.frame(biz_id, biz_long, biz_lat)
colnames(biz_long_lat)= c("businessId", "longitude", "latitude")

#making lists for review data
review.stars = unlist(lapply(review_data, function(x) x$stars))
review.id = unlist(lapply(review_data, function(x) x$review_id))
review.user_id = unlist(lapply(review_data, function(x) x$user_id))
review.business_id = unlist(lapply(review_data, function(x) x$business_id))
review.post_date= unlist(lapply(review_data, function(x) x$date))
review.post_date= as.Date(review.post_date)
review.vote_funny= unlist(lapply(review_data, function(x) x$votes$funny))
review.vote_cool= unlist(lapply(review_data, function(x) x$votes$cool))
review.vote_useful= unlist(lapply(review_data, function(x) x$votes$useful))

#making a data frame from user lists
review_table= data.frame(review.id, review.post_date, review.stars, review.vote_funny, 
	review.vote_cool, review.vote_useful, review.business_id, review.user_id)
colnames(review_table)=c("reviewId", "postDate", "stars", "voteFunny", "voteCool", "voteUseful", "businessId", "userId")

install.packages("data.table")
library(data.table)

foodBids = read.table("bid_to_food.txt")
colnames(foodBids)=c("businessId")
foodBids=data.table(foodBids)
setkey(foodBids, businessId)
businessReviewStars= data.table(review.business_id, review.stars)
setkey(businessReviewStars, review.business_id)
foodBids=foodBids[businessReviewStars, nomatch=0]

#finding lat and long for all businesses that a user posted for  in Food/Restaurant category 
user_businessFood=subset(review_table, review_table$businessId %in% bizFood)[,c(8,7)]
# 53670 users and 6570 businesses in food/restaurants category 

#CONSIDERING ONLY USERS WHO HAVE GONE TO MORE THAN ONE PLACE
userBizCount=ddply(user_businessFood, .(userId), summarise, count=length(businessId))
#there are 27928 users how have reviewed 1 place and thus have sumDistances metric coming to 0 
userTwoOrMoreBiz=subset(userBizCount, userBizCount$count>1)$userId
#Now we are considering 25742 unique users with reviews for more than one food/restaurant business among 6561 businesses 
#This means those 27928 users only reviewed among 9 businesses

userWithSumDistZero=read.table("userWithSumDistZero.txt")$userWithSumDistZero
indicesToRemove=match(userWithSumDistZero, userTwoOrMoreBiz)
usersToConsider= userTwoOrMoreBiz[-indicesToRemove]
user_businessFood=subset(user_businessFood, user_businessFood$userId %in% usersToConsider)
#Taking 25742-291=25451 users and 6561 businesses

#Computing lower bound of 95% CI of review ratings for each business

##bizCIStars<- ddply(user_businessFood, .(businessId), summarize, stars = list(stars))
##temp = c()
##for(i in 1:nrow(bizCIStars)) {
##	temp[i] = CI( as.vector(bizCIStars$stars[[i]]), ci=0.95 )[3]
##}
##bizCIStars$lowerCIStars = temp
install.packages("Rmisc")
library(Rmisc)
bizCIStars=ddply(user_businessFood, .(businessId), summarise, CILowerStars=CI(stars, ci=0.95)[3])
#some business have NA values for lower bound since they only have one review rating
bizNA= subset(bizCIStars, is.na(CILowerStars))
bizCIStars= na.omit(bizCIStars)
#removed those values and put the star rating for that business as the CILowerStars value
bizNA= subset(user_businessFood, businessId %in% bizNA$businessId)
bizNA=data.frame(bizNA$businessId, bizNA$stars)
colnames(bizNA)=c("businessId", "CILowerStars")
bizCIStars=rbind(bizCIStars, bizNA)


#appending location to main data frame 
user_businessFood=data.table(user_businessFood)
setkey(user_businessFood, businessId)
biz_long_lat_food=subset(biz_long_lat, biz_long_lat$businessId %in% bizFood)
biz_long_lat_food=data.table(biz_long_lat_food)
setkey(biz_long_lat_food, businessId)
user_businessFood=user_businessFood[biz_long_lat_food, nomatch=0]
setkey(user_businessFood, userId)

#appending lower bound of CI for star ratings to main data frame
setkey(user_businessFood, businessId)
bizCIStars=data.table(bizCIStars)
setkey(bizCIStars, businessId)
user_businessFood=user_businessFood[bizCIStars, nomatch=0]


#take average distance of all location data points per user 
findMean = function(user_businessFood) {
userMeanLongLatFood=data.frame(user_businessFood$userId, user_businessFood$longitude, user_businessFood$latitude)
colnames(userMeanLongLatFood)=c("userId", "longitude", "latitude")
userMeanLongLatFood$userId= factor(userMeanLongLatFood$userId)
userMeanLongLatFood= ddply(userMeanLongLatFood, "userId", numcolwise(mean))
colnames(userMeanLongLatFood)=c("userId", "meanLong", "meanLat")
userMeanLongLatFood=data.table(userMeanLongLatFood)
setkey(userMeanLongLatFood, userId)
user_businessFood=data.table(user_businessFood)
setkey(user_businessFood,userId)
user_businessFood=user_businessFood[userMeanLongLatFood, nomatch=0]
return(user_businessFood)
}

user_businessFood= findMean(user_businessFood)

#find distance of all data points per user from the mean for that user
user_businessFood$distanceFromMean= sqrt(((user_businessFood$longitude - user_businessFood$meanLong)^2) + ((user_businessFood$latitude - user_businessFood$meanLat)^2))

#normalize the distances for that user
userSumDistancesFood= ddply(user_businessFood, .(userId), summarise, sumDistances= sum(distanceFromMean))
summary(userSumDistancesFood)

###STILL 291 users with 0 sum of distance from mean because their mean is also 0 as the restaurants they visited were in the same locations
###check if these users went to only one place and reviewed that multiple times or only went to different places in the same lat and long (23 such users)
##userWithSumDistZero=subset(userSumDistancesFood, sumDistances==0)$userId
##userBizWithSumDistZero=subset(user_businessFood, user_businessFood$userId %in% userWithSumDistZero)
##userBizWithSumDistZero=data.frame(userBizWithSumDistZero$userId, userBizWithSumDistZero$businessId)
##colnames(userBizWithSumDistZero)=c("userId", "businessId")
###counting if there are users who posted in different businesses belonging to the same long and lat 
##userBizWithSumDistZero.count= ddply(userBizWithSumDistZero, .(userId), summarise, count=length(unique(businessId)))
##users2BizSameLoc=subset(userBizWithSumDistZero.count, count>1)$userId
##write.table("userWithSumDistZero.txt")
##

userSumDistancesFood= data.table(userSumDistancesFood)
setkey(userSumDistancesFood, userId)
user_businessFood=user_businessFood[userSumDistancesFood, nomatch=0]
user_businessFood$normDistanceFromMean= user_businessFood$distanceFromMean/user_businessFood$sumDistances
#there are 3237 unique businesses spanning 110009 users which have distance from mean >0.5 
# hist(user_businessFood$normDistanceFromMean)

#finding outlier for each user using outlier()
#instal.packages("outliers")
#install.packages("Rmisc")
library(outliers) 
library(Rmisc)

#any distance from mean measure greater than the upper bound of 95% confidence interval of the data points for a user are considered outliers
user95CIUpper_Food=ddply(user_businessFood, .(userId), summarise, CIUpper=CI(normDistanceFromMean, ci=0.95)[1])
#summary(user95CIUpper_Food)

#as suspected the median for the upper bounds of 95% CI for each user is 0.5. Thus our intuition for taking 0.5 as threshold is correct as well
#Another aspect to look at is that our dataset contains some users posted many times for a particular business and probably posted a fewer number of times for other businesses, the CI also considers how many times you post 
#removing all data points from each user group that have distance measure from mean > Upper bound of 95% CI
user95CIUpper_Food=data.table(user95CIUpper_Food)
setkey(user95CIUpper_Food, userId)
setkey(user_businessFood, userId)
user_businessFood=user_businessFood[user95CIUpper_Food, nomatch=0]
userBizNoOutliers= subset(user_businessFood, normDistanceFromMean <= CIUpper)
# 98 unique businesses behaved as outliers for some user or the other

###NOT DOING THIS 
##remove outliers i.e. all normalized distances from the mean having value greater than 0.5 
##userBizNoOutliers=subset(user_businessFood, user_businessFood$normDistanceFromMean<0.5)
###

#find new mean/ centroid of location points per user after removing outlier data points
userBizNoOutliers=findMean(userBizNoOutliers)
userNewMean=data.frame(userBizNoOutliers$userId, userBizNoOutliers$meanLong.1, userBizNoOutliers$meanLat.1)
colnames(userNewMean)=c("userId", "newMeanLong", "newMeanLat")
#removing duplicate rows
userNewMean=unique(userNewMean)
#setting key as userId
userNewMean=data.table(userNewMean)
setkey(userNewMean, userId)
setkey(user_businessFood, userId)
#append the new means to the entire dataset 
user_businessFood=user_businessFood[userNewMean, nomatch=0]
#find distance of all data points per user from the new mean/ centre for that user
user_businessFood$distanceFromCentre= sqrt(((user_businessFood$longitude - user_businessFood$newMeanLong)^2) + ((user_businessFood$latitude - user_businessFood$newMeanLat)^2))
#normalize the distances for that user
userSumDistancesCentreFood= ddply(user_businessFood, .(userId), summarise, sumDistanceFromCentre= sum(distanceFromCentre))
userSumDistancesCentreFood= data.table(userSumDistancesCentreFood)
setkey(userSumDistancesCentreFood, userId)
setkey(user_businessFood, userId)
user_businessFood=user_businessFood[userSumDistancesCentreFood, nomatch=0]
user_businessFood$normDistanceFromCentre= user_businessFood$distanceFromCentre/user_businessFood$sumDistanceFromCentre

#why are distanceFromCentre zero and sumDistancesCentre zero for some
summary(user_businessFood)

#computing UMM for each business averaged over all users who went to eat there
bizUMM=data.frame(user_businessFood$businessId, user_businessFood$normDistanceFromCentre)
colnames(bizUMM)=c("businessId", "normDistanceFromCentre")
bizUMM=ddply(bizUMM, .(businessId), numcolwise(mean))
colnames(bizUMM)=c("businessId", "UMM")
#write.table(bizUMM, file="bizUMM.txt")

#computing UMM-R 
# finding ratings given by user to each business from reviews
userBizFoodRating= data.frame(review.user_id, review.business_id, review.stars)
colnames(userBizFoodRating)=c("userId", "businessId", "stars")
userBizFoodRating=subset(userBizFoodRating, userBizFoodRating$userId %in% usersToConsider)
userBizFoodRating= subset(userBizFoodRating, userBizFoodRating$businessId %in% bizFood)
#Some users seem to have given multiple reviews to the same business, thus taking the mean stars for those
userBizFoodRating$businessId=as.factor(userBizFoodRating$businessId)
userBizFoodRating=ddply(userBizFoodRating, .(userId, businessId), numcolwise(mean))
# there are 187833 unique (user, business) pairs out of 196061 pairs

#appending review stars to user_businessFood
userBizFoodRating=data.table(userBizFoodRating)
setkey(userBizFoodRating, userId, businessId)
setkey(user_businessFood, userId, businessId)
user_businessFood=user_businessFood[userBizFoodRating, nomatch=0]
#computing the normDistanceFromCentre X stars 
user_businessFood$normDistanceFromCentreXstars= user_businessFood$normDistanceFromCentre * user_businessFood$stars
#UMMR
bizUMMR=data.frame(user_businessFood$businessId, user_businessFood$normDistanceFromCentreXstars)
colnames(bizUMMR)=c("businessId", "normDistanceFromCentreXstars")
bizUMMR=ddply(bizUMMR, .(businessId), numcolwise(mean))
colnames(bizUMMR)=c("businessId", "UMMR")
#write.table(bizUMMR, file="bizUMMR.txt")

#Evaluating relation between UMM, UMMR and average business ratings 
bizStarsFood= subset(bizStars, bizStars$businessId %in% user_businessFood$businessId)
bizStarsFood=data.table(bizStarsFood)
setkey(bizStarsFood, businessId)
bizUMM=data.table(bizUMM)
setkey(bizUMM, businessId)
bizUMMR=data.table(bizUMMR)
setkey(bizUMMR, businessId)
bizStarsFood=bizStarsFood[bizUMM, nomatch=0]
bizStarsFood=bizStarsFood[bizUMMR, nomatch=0]
bizStarsFood=bizStarsFood[bizCIStars, nomatch=0]
#bizStarsFood contains businessId, avg stars, UMM, UMMR, lower bound CI stars

plot(bizStarsFood$UMMR, bizStarsFood$stars)
plot(bizStarsFood$UMM, bizStarsFood$stars)

#decent correlation
cor(bizStarsFood$UMMR, bizStarsFood$stars) #  0.3762939
cor(bizStarsFood$UMM, bizStarsFood$stars) # 0.06385295 

#bad correlation
cor(bizStarsFood$UMMR, bizStarsFood$CILowerStars) #  0.1238096
cor(bizStarsFood$UMM, bizStarsFood$CILowerStars) # -0.04367564 


#regression
starsUMMR.lm= lm(stars~UMMR, data=bizStarsFood)
summary(starsUMMR.lm)
plot(bizStarsFood$UMMR, bizStarsFood$stars)
abline(starsUMMR.lm, col="red", lwd=2)
pred1=predict(starsUMMR.lm)

#accuracy of prediction
#if we compare predicted and actual stars value, not a very good prediction
mean(round(pred1,1)==round(bizStarsFood$stars,1)) # 1.4% accuracy with 1 decimal place
mean(round(pred1,0)==round(bizStarsFood$stars, 0)) # 14% accuracy with no decimal places 
correct1=0
correct2=0
for(i in 1:length(pred1)){
	if(round(pred1[i],0) == round(bizStarsFood$stars[i], 0)-1 | round(pred1[i],0) == round(bizStarsFood$stars[i], 0)+1
		| round(pred1[i],0) == round(bizStarsFood$stars[i], 0))
		correct1=correct1+1

	if(round(pred1[i],1) %in% seq(round(bizStarsFood$stars[i], 1)-0.5, round(bizStarsFood$stars[i], 1)+ 0.5, by=0.1 )  )
		correct2=correct2+1
}
correct1/length(pred1) # 49% accuracy for range of + -1
correct2/length(pred1) # 16.4% accuracy for range of + - 0.5 


#regression without intercept performs the same 
starsUMMR.lm.noIntercept= lm(stars~UMMR-1, data=bizStarsFood)
summary(starsUMMR.lm.noIntercept)
pred2=predict(starsUMMR.lm.noIntercept)
mean(pred2==bizStarsFood$stars)
mean(round(pred2,1)==round(bizStarsFood$stars,1)) #1.4% accuracy
mean(round(pred2,0)==round(bizStarsFood$stars, 0)) #14.5% accuracy 
#we need more predictors
#what is optimistic about the regression is that the R squared value is decent and UMMR is a significant predictor but it is unable to predict well
#Also there is no linear relationship between the two variables as visible from the plots, thus some other prediction method is needed


#Analysing businesses in zip code 85208
biz_85208=subset(biz_id_zip, zip==85208)$business_id
biz_85208_data=subset(bizStarsFood, businessId %in% biz_85208)

#6 food places in 85208
cor(biz_85208_data$stars, biz_85208_data$UMMR) # 0.5408978 high
cor(biz_85208_data$stars, biz_85208_data$UMM)  # -0.2325567 low

#really good correlations with LowerCIStars
cor(biz_85208_data$CILowerStars, biz_85208_data$UMM) #0.427692 
cor(biz_85208_data$CILowerStars, biz_85208_data$UMMR) #0.8284506

#we should find correlations between UMM/UMMR & Stars for all businesses grouped by zipcode

#plotting histograms 
#business histograms

par(mfrow=c(2,2))
hist(biz_stars, main="Histogram of Business Star Ratings", xlab="Stars", col="royalblue", border="white", lwd="2")
hist(log10(biz_review_count), main="Histogram of Review Counts", xlab="log10(Review Counts)", col="orange", border="white", lwd="2")
bplot_1= barplot(biz_zip_count$count, main="Frequency of Unique Zipcodes", xlab="Zipcode", ylab="Frequency", lwd=2, col="seagreen", xaxt="n")
#text(cex=0.8, x=bplot_1-.35, y=-.25, labels= as.character(biz_zip_count$zip), xpd=TRUE, srt=45, pos=1)
bplot_2= barplot(biz_main_cat_count$count, main="Frequency of Main Business Categories", xlab="Business Category",
	ylab="Frequency", lwd=2, col="red")
text(cex=0.8, x=bplot_2, y= 3000, labels= as.character(biz_main_cat_count$main_cat), xpd=TRUE, srt=90, pos=1)


#user histograms
par( mfrow=c(2,2))
hist(user.avg_stars, main="Histogram of Average Star Ratings", xlab="Average Stars", col="darkcyan", border="white", lwd="2")
hist(log10(user.review_count), main="Histogram of Review Counts", xlab="log10(Review Counts)", col="orange", border="white", lwd="2")
hist(log10(user.fans), main="Histogram of Fans", xlab="log10(Fans)", col="grey", border="white", lwd="2")
hist(user.join_date, "months", main="Histogram of Joining Yelp", xlab="Join Date",lwd="2", format = "%d %b")



library(maps)
#map('state', plot = TRUE, fill = FALSE, col = palette())
colors()[grep("green",colors())]
map('county', 'Arizona', plot = TRUE, fill = TRUE, col="springgreen3"  , bg="white", ylim= c(min(biz_lat), max(biz_lat)), 
	xlim= c(min(biz_long), max(biz_long)))
title("Businesses in Yelp in Phoenix")
points(x=biz_long, y=biz_lat, col="yellow", pch=20)
map.axes()

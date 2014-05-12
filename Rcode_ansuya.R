#install.packages("rjson")
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

biz_id_zip=read.table("/Users/Work/Documents/CS246_Proj/dataset-challenge/bid_to_zip.txt", sep="\t", header=FALSE)
colnames(biz_id_zip)= c("business_id", "zip")
biz_id_main_cat=read.table("/Users/Work/Dropbox/UCLA Courses/Spring 2014/CS246/Project/main_categories.txt", sep="\t", header=FALSE)
colnames(biz_id_main_cat)= c("business_id", "main_cat")

library(plyr)
biz_main_cat_count=ddply(biz_id_main_cat, .(main_cat), summarise, count=length(main_cat))
biz_zip_count=ddply(biz_id_zip, .(zip), summarise, count=length(zip))

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
vectorTable <- ddply(foodBids, .(businessId), summarize, stars = list(review.stars))
temp = c()
for(i in 1:nrow(vectorTable)) {
	temp[i] = CI(as.vector(vectorTable$stars[[i]]))[3]
}
vectorTable$ci = temp

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

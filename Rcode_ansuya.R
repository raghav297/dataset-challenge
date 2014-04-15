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


#plotting histograms 
par( mfrow=c(2,2))
hist(user.avg_stars, main="Histogram of Average Star Ratings", xlab="Average Stars", col="darkcyan", border="white", lwd="2")
hist(user.review_count, main="Histogram of Review Counts", xlab="Review Counts", col="orange", border="white", lwd="2")
hist(user.fans, main="Histogram of Fans", xlab="Fans", col="grey", border="white", lwd="2")
hist(user.join_date, "months", main="Histogram of Joining Yelp", xlab="Join Date",lwd="2", format = "%d %b")

library(maps)
#map('state', plot = TRUE, fill = FALSE, col = palette())
colors()[grep("green",colors())]
map('county', 'Arizona', plot = TRUE, fill = TRUE, col="springgreen3"  , bg="white", ylim= c(min(biz_lat), max(biz_lat)), 
	xlim= c(min(biz_long), max(biz_long)))
title("Businesses in Yelp in Phoenix")
points(x=biz_long, y=biz_lat, col="yellow", pch=20)
map.axes()
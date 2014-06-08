# Ignore this file. Just trying stuff out

install.packages("rjson")
library(rjson)

business= "/Users/ash/Downloads/yelp_phoenix_academic_dataset/yelp_academic_dataset_business.json"
raw_biz= scan(business, what="raw()", sep="\n")
#to make it human readable
biz_data= lapply(raw_biz, function(x) fromJSON(x))
#15585 businesses

#making lists for business data
biz_id= unlist(lapply(biz_data, function(x) x$business_id))
biz_long= unlist(lapply(biz_data, function(x) x$longitude))
biz_lat= unlist(lapply(biz_data, function(x) x$latitude))
biz_category=list(); for(i in 1:15585){ biz_category[[i]]=biz_data[[i]]$categories}
biz_stars=unlist(lapply(biz_data, function(x) x$stars))
biz_review_count=unlist(lapply(biz_data, function(x) x$review_count))

# me: need to also add some more features (business features)
biz_takeout = unlist(lapply(biz_data, function(x) x$Take-out))


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

# me: Needed - make a dataframe for all the attributes under consideration by business id

# Take-out
# ------------- EXTRACTING TAKE-OUT DATA-------------
#biz_takeout = unlist(lapply(biz_data, function(x) x$attributes$`Take-out`))

bids=c()
take_outs=c()
for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Take-out`)!= 0)
	{
		if(biz_data[[i]]$attributes$`Take-out`)
		{	
			take_outs = c(take_outs,1)
		}
		else
		{
			take_outs = c(take_outs,0)
		}
	}
	else
	{
		take_outs = c(take_outs,0)
	}
	bids = c(bids, biz_data[[i]]$businessId)
}
# ----------------------------------------------------

# Accepts credit cards:

bids=c()
credit_cards=c()
for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Accepts Credit Cards`)!= 0)
	{
		if(biz_data[[i]]$attributes$`Accepts Credit Cards`)
		{	
			credit_cards = c(credit_cards,1)
		}
		else
		{
			credit_cards = c(credit_cards,0)
		}
	}
	else
	{
		credit_cards = c(credit_cards,0)
	}
	bids = c(bids, biz_data[[i]]$businessId)
}
# For some strange reason, there is a 0 at the begining of credit_cards, so getting rid of that:
credit_cards2 = credit_cards[2:length(credit_cards)]
# Now the length should coincide with the length of biz_data

# ------------- PRICE RANGE --------------
bids=c()
price_range=c()
for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Price Range`)!= 0)
	{	
		price_range = c(price_range,biz_data[[i]]$attributes$`Price Range`)
	}
	else
	{
		price_range = c(price_range,0)
	}
	# Putting 0 for unknown price ranges
	bids = c(bids, biz_data[[i]]$businessId)
}

biz_priceRange = cbind(biz_id,price_range)
biz_priceRange1 = data.frame(biz_priceRange)
foodBiz_price = subset(biz_priceRange1, biz_priceRange1$biz_id %in% bizFood)

# --------- making a common dataframe with all the features--------

features = data.frame(biz_priceRange,credit_cards2,take_outs)
# Extracting features corresponding to restaurants and food only.
foodFeatures = subset(features, biz_id %in% bizFood)

# -------- saving the features as a CSV ----------

write.csv(foodFeatures, file = "features.csv")

# -------- getting more features ----------

#bids=c()
# ----------------ambience
amb=c()



for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Ambience`)!= 0)
	{	
		amb_entry = c(biz_data[[i]]$attributes$`Ambience`$`romantic`,biz_data[[i]]$attributes$`Ambience`$`intimate`,biz_data[[i]]$attributes$`Ambience`$`touristy`,biz_data[[i]]$attributes$`Ambience`$`hipster`,biz_data[[i]]$attributes$`Ambience`$`divey`,biz_data[[i]]$attributes$`Ambience`$`classy`,biz_data[[i]]$attributes$`Ambience`$`trendy`,biz_data[[i]]$attributes$`Ambience`$`upscale`,biz_data[[i]]$attributes$`Ambience`$`casual`)
	}
	else
	{
		amb_entry = c(rep(0,9))
	}
	amb = rbind(amb,amb_entry)
	# Putting 0 for unknown price ranges
	#bids = c(bids, biz_data[[i]]$businessId)
}
# romantic": false, "intimate": false, "touristy": false, "hipster": false, "divey": false, "classy": false, "trendy": false, "upscale": false, "casual": true

colnames(amb) = c("romantic","intimate","touristy","hipster","divey","classy","trendy","upscale","casual")
write.csv(amb,bids,file = "Ambience.csv")
# -------------- reservations and waiter service---------
bids=c()
reservation=c()
waiter = c()
for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Takes Reservations`)!= 0)
	{
		if(biz_data[[i]]$attributes$`Takes Reservations`)
		{	
			reservation = c(reservation,1)
		}
		else
		{
			reservation = c(reservation,0)
		}
	}
	else
	{
		reservation = c(reservation,0)
	}
	bids = c(bids, biz_data[[i]]$businessId)
	if(length(biz_data[[i]]$attributes$`Waiter Service`)!= 0)
	{
		if(biz_data[[i]]$attributes$`Waiter Service`)
		{	
			waiter = c(waiter,1)
		}
		else
		{
			waiter = c(waiter,0)
		}
	}
	else
	{
		waiter = c(waiter,0)
	}
}

# -------------- Alcohol

alcohol = c()
for (i in 1: length(biz_data))
{	
	if(length(biz_data[[i]]$attributes$`Alcohol`)!= 0)
	{	
		alcohol = c(alcohol, biz_data[[i]]$attributes$`Alcohol`)
	}
	else
	{
		alcohol = c(alcohol, "unknown")
	}
}

# -------------- Noise Level

noise = c()
noise_levels = c("quiet","average","loud","very_loud")
# Corresponds to the levels. 0 - unknown

# finding levels
for(i in 1:length(biz_data))
{
	if(length(biz_data[[i]]$attributes$`Noise Level`)!=0)
	{
		if(!(biz_data[[i]]$attributes$`Noise Level` %in% noise_levels))
		{
			noise_levels = c(noise_levels,biz_data[[i]]$attributes$`Noise Level`)
		}
		noise_value = biz_data[[i]]$attributes$`Noise Level`
		noise = c(noise, match(noise_value,noise_levels))
	}
	else
	{
		noise = c(noise,0)
	}
}

# ------------------Smoking or non-smoking

smoke = c()
smoke_choices = c("yes","no","outdoor")

for(i in 1:length(biz_data))
{
	if(length(biz_data[[i]]$attributes$`Smoking`)!=0)
	{
		smoke_value = biz_data[[i]]$attributes$`Smoking`
		smoke = c(smoke, match(smoke_value,smoke_choices))
	}
	else
	{
		smoke = c(smoke,0)
	}
}

# --------- making a common dataframe with all the features--------

# NOTE: biz_priceRange also has the biz_ids
features = data.frame(biz_priceRange,credit_cards2,take_outs,smoke,noise,reservation,waiter,amb)
# Extracting features corresponding to restaurants and food only.
foodFeatures = subset(features, biz_id %in% bizFood)

# -------- saving the features as a CSV ----------

write.csv(foodFeatures, file = "features1.csv")


# ----------- Training ---------------------------






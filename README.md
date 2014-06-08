Dataset-challenge
=================

Yelp is an online urban guide, which contains local business listings. Each business on Yelp can be said to be popular or not based on the number of reviews and ratings it receives. However, how can businesses that are less popular get to know their potential competitors? In this paper, we propose a methodology to predict competition among restaurants and food joints based on approximate travel distance of users. Since Yelp does not contain information of where its users reside, we develop a simple mathematical model to compute mobility of a user, referred to as User Mobility Metric (UMM) and User Mobility Metric with Ratings (UMMR). Further, we extract business features from the dataset and select the best business features for predicting business ratings. We find that mobility metrics are significant predictors. Finally, we use these features to classify businesses as dogs (successful businesses) and kittens (not so successful businesses).

#### Languages
Python , R 

#### Important files
__1. main_code.R__
Used to convert JSON data into tables, extract businesses, find central location points and UMM/UMMR and measure correlation.

__2. attr_extraction.R__
Used to extract business attributes from JSON. Upto 31 features extracted for prediction and classification.

__3. learning_code.R__
Used for best subset selection and to test and model various models for predicting relation between features and star rating.

__4. classification_1.R / classification_2.R__
Used to classify businesses as cats and dogs and calculate the precision, recall and accuracy for several sets of features.
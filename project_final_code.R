# Read the dataset
airbnb.df <- read.csv("listings.csv",header = TRUE,sep=",")
t(t(names(airbnb.df)))
dim(airbnb.df)
summary(airbnb.df)
library(dplyr)
# Check the data type
str(airbnb.df)
str(airbnb.df)
table(airbnb.df$bed_type)

#Checking missing values
sapply(airbnb.df, function(x) sum(is.na(x)))

#Removing unwanted columns
airbnb.new.df = subset(airbnb.df, select = -c(host_acceptance_rate,availability_365,review_scores_value,review_scores_location,review_scores_communication,review_scores_checkin,review_scores_cleanliness,review_scores_accuracy,amenities,host_total_listings_count,host_response_time,is_location_exact,has_availability,calculated_host_listings_count,street,neighbourhood,zipcode,bed_type,calendar_updated,license,requires_license,jurisdiction_names,is_business_travel_ready,calculated_host_listings_count_shared_rooms,calculated_host_listings_count_private_rooms,calculated_host_listings_count_entire_homes,reviews_per_month,require_guest_phone_verification,require_guest_profile_picture,number_of_reviews_ltm,availability_60,availability_30,maximum_nights_avg_ntm,minimum_nights_avg_ntm,maximum_maximum_nights,minimum_maximum_nights,maximum_minimum_nights,minimum_minimum_nights,monthly_price,weekly_price,square_feet,country,country_code,market,city,state,neighbourhood_group_cleansed,id,experiences_offered,host_has_profile_pic,name,summary,listing_url, scrape_id, last_scraped, space, description, neighborhood_overview, notes, transit, access, interaction, house_rules, thumbnail_url, medium_url, picture_url, xl_picture_url,host_url, host_name, host_location, host_about, host_thumbnail_url, host_picture_url, host_neighbourhood, host_verifications, calendar_last_scraped))
dim(airbnb.new.df)
t(t(names(airbnb.new.df)))

#Checking missing values
sapply(airbnb.new.df, function(x) sum(is.na(x)))
summary(airbnb.new.df$bathrooms)
summary(airbnb.new.df$bedrooms)
summary(airbnb.new.df$beds)

# Replacing with median. median of bathrooms,beds,bedrooms is 1
airbnb.new.df$bathrooms[is.na(airbnb.new.df$bathrooms)] <- 1
airbnb.new.df$bedrooms[is.na(airbnb.new.df$bedrooms)] <- 1
airbnb.new.df$beds[is.na(airbnb.new.df$beds)] <- 1
sapply(airbnb.new.df, function(x) sum(is.na(x)))

#removing rows
airbnb_new.df <- airbnb.new.df[complete.cases(airbnb.new.df$host_listings_count,airbnb.new.df$review_scores_rating),]
dim(airbnb_new.df)

#removing $ 
airbnb_new.df$security_deposit = as.numeric(gsub("\\$", "", airbnb_new.df$security_deposit))
airbnb_new.df$cleaning_fee = as.numeric(gsub("\\$", "", airbnb_new.df$cleaning_fee))
airbnb_new.df$price = as.numeric(gsub("\\$", "", airbnb_new.df$price))
airbnb_new.df$extra_people = as.numeric(gsub("\\$", "", airbnb_new.df$extra_people))
airbnb_new.df$host_response_rate = as.numeric(gsub("\\%", "", airbnb_new.df$host_response_rate))

summary(airbnb_new.df$price)
summary(airbnb_new.df$security_deposit)
summary(airbnb_new.df$cleaning_fee)
#Replacing missing with 0
airbnb_new.df$security_deposit[is.na(airbnb_new.df$security_deposit)] <- 0
airbnb_new.df$cleaning_fee[is.na(airbnb_new.df$cleaning_fee)] <- 0
airbnb_new.df$price[is.na(airbnb_new.df$price)] <- 0
# Removing missing values again after replacing them with median  
sapply(airbnb_new.df, function(x) sum(is.na(x)))
print(class(airbnb_new.df$host_response_rate))

#naming missing values of host_response_rate as unknown
airbnb_new.df$host_response_rate[is.na(airbnb_new.df$host_response_rate)] <- "Unknown"

sapply(airbnb_new.df, function(x) sum(is.na(x)))

# Cancellation policy Combining 8 levels into 3 levels flexible ,moderate,strict_14_with_grace_period
table(airbnb_new.df$cancellation_policy)
levels(airbnb_new.df$cancellation_policy)

library(forcats)
library(dplyr)
airbnb_new.df$cancellation_policy<-fct_collapse(airbnb_new.df$cancellation_policy, strict_14_with_grace_period = c("strict_14_with_grace_period","super_strict_30","super_strict_60","strict","luxury_super_strict_95"), moderate = c("moderate","luxury_moderate"))
levels(airbnb_new.df$cancellation_policy)
table(airbnb_new.df$cancellation_policy)

#Property_type Combining 44 levels to get 3 levels
table(airbnb_new.df$property_type)
levels(airbnb_new.df$property_type)

airbnb_new.df$property_type<-fct_collapse(airbnb_new.df$property_type, Apartment = c("Serviced apartment","Loft","Apartment"), House=c("Townhouse","House","Bungalow","Cottage","Villa","Tiny house","Earth house","Chalet"), 
                                          Other=c("Aparthotel","Barn","Bed and breakfast","Boat","Boutique hotel","Bus","Cabin","Camper/RV","Campsite","Casa particular (Cuba)",
                                                  "Condominium","Dome house","Farm stay","Guest suite","Guesthouse","Hostel","Hotel","Houseboat",
                                                  "Lighthouse","Minsu (Taiwan)","Nature lodge","Hut","Island","Parking Space","Plane","Ryokan (Japan)","Tent","Other","Treehouse","Yurt"))

levels(airbnb_new.df$property_type)
table(airbnb_new.df$property_type)


#levels of Room type
levels(airbnb_new.df$room_type)


#Renaming neighbourhood_cleansed to neighbourhood
airbnb_new.df <- rename(airbnb_new.df, "neighbourhood" = "neighbourhood_cleansed")

t(t(names(airbnb_new.df)))

airbnb_new.df$host_response_rate[airbnb_new.df$host_response_rate == "Unknown"]  <- 0

airbnb_new.df$minimum_nights[airbnb_new.df$minimum_nights>=365] <- 365
airbnb_new.df$maximum_nights[airbnb_new.df$maximum_nights>=365] <- 365

library(anytime)
airbnb_new.df$host_since<-anydate(airbnb_new.df$host_since)
airbnb_new.df$first_review<-anydate(airbnb_new.df$first_review)
airbnb_new.df$last_review<-anydate(airbnb_new.df$last_review)

str(airbnb_new.df)
sapply(airbnb_new.df, function(x) sum(is.na(x)))

#Removing 2 rows
airbnb_new.df <- airbnb_new.df[complete.cases(airbnb_new.df$first_review,airbnb_new.df$last_review),]

str(airbnb_new.df)
sapply(airbnb_new.df, function(x) sum(is.na(x)))

airbnb_new.df$host_response_rate=as.numeric(airbnb_new.df$host_response_rate)
str(airbnb_new.df)


## heatmap with values
t(t(names(airbnb_new.df)))
str(airbnb_new1.df)
numerical.var<- c(3,13,14,15,16,18,19,20,21,22,23,24,25,28,17)
new.df<-airbnb_new.df[,numerical.var]
str(new.df)
dim(new.df)
round(cor(new.df),2)

library(gplots)
heatmap.2(cor(new.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(new.df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(15,15),legend="col")


airbnb.df <- airbnb_new.df
# Converting categorical variable as factor
airbnb.df$host_is_superhost=as.factor(airbnb.df$host_is_superhost)
airbnb.df$property_type=as.factor(airbnb.df$property_type)
airbnb.df$neighbourhood=as.factor(airbnb.df$neighbourhood)
airbnb.df$room_type=as.factor(airbnb.df$room_type)
airbnb.df$smart_location=as.factor(airbnb.df$smart_location)
airbnb.df$instant_bookable=as.factor(airbnb.df$instant_bookable)
airbnb.df$host_identity_verified=as.factor(airbnb.df$host_identity_verified)
str(airbnb.df)

#Partitions 
set.seed(1)
## partitioning into training (60%) and validation (40%)
# Training
train.rows <- sample(rownames(airbnb.df), dim(airbnb.df)[1]*0.6)
train.data <- airbnb.df[train.rows, ]
dim(train.data)

#Validation
valid.rows <- setdiff(rownames(airbnb.df), train.rows) 
valid.data <- airbnb_new.df[valid.rows, ]
dim(valid.data)

train1.data <-train.data
valid1.data <- valid.data

selected.var<- c(3, 4, 5, 6, 7, 9, 10, 11, 12,13, 14,15,16,17,18, 19, 20, 21, 22, 23, 24,25, 28, 29, 30)
train1.data<-train1.data[,selected.var]
valid1.data<-valid1.data[,selected.var]
airbnb.df_new <- airbnb.df[,selected.var]

#Model 1 K-NN

library(dummies)
airbnb.df_new <- dummy.data.frame(airbnb.df_new, sep = "_")
names(airbnb.df_new)
train1.data <- dummy.data.frame(train1.data, sep = "_")
names(train1.data)
valid1.data <- dummy.data.frame(valid1.data, sep = "_")
names(valid1.data)



# initialize normalized training, validation data, dataset frames to originals
train.norm.df <- train1.data
valid.norm.df <- valid1.data
Airbnb.norm.df <- airbnb.df_new

# use preProcess() from the caret package to normalize the dataset.

library(caret)
options(scipen = 999)

norm.values <- preProcess(train1.data[, c(1:52,54:67) ], method=c("center", "scale")) 
head(norm.values)
train.norm.df[, c(1:52,54:67) ]<- predict(norm.values, train1.data[, c(1:52,54:67) ])
head(train.norm.df)

valid.norm.df[, c(1:52,54:67) ] <- predict(norm.values, valid1.data[,c(1:52,54:67) ])
head(valid.norm.df)
Airbnb.norm.df[, c(1:52,54:67) ] <- predict(norm.values, airbnb.df_new[,c(1:52,54:67)])
head(Airbnb.norm.df)

#Running K-NN algorithm for best k 
library(FNN)
knn.pred <-  knn.reg(train = train.norm.df[, c(1:52,54:67)], test = valid.norm.df[, c(1:52,54:67)], 
                        train.norm.df[, 53], k =40)
summary(knn.pred)
print(knn.pred)
indices <- 1:dim(valid.norm.df)[1]


plot(indices[1:50], knn.pred$pred[1:50], type = "l", col="red", ylab = "Price" ) 
lines(indices[1:50], valid.norm.df$price[1:50], col="blue")
legend(1,200 , legend=c("Predicted Value", "Actual Values"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
#mean square prediction error
MSE = mean((valid.norm.df$price - knn.pred$pred) ^ 2)
print(MSE)
RMSE = sqrt(MSE)
print(RMSE)

#We ran k-nn for different values of k. Below are the accuracy metrics for it:
#k=30 rmse:61.27
#k=20 ,rmse:60.74
#k=10 : 60.63
#K=5: rmse =61
#K=200 RMSE: 65
#K=100 RMSE:63.46
#k=300 RMSE: 66
#k=80 RMSE: 63
#k=40 RMSE: 61.62
#k= 2000, RMSE : 69

#We choosing the best k=40 


# Classification tree
#Classify till we get pure bran
library(rpart)
library(rpart.plot)
Airbnbdefault.tree <- rpart(price ~ ., data = train1.data, method="anova")
plotcp(Airbnbdefault.tree)
printcp(Airbnbdefault.tree)
summary(Airbnbdefault.tree)
prp(Airbnbdefault.tree, type = 1, extra = 1, split.font = 1, varlen = -10, digits = -1)

#CP : 0.01544279

#to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}
print(get_cp)
# prune the tree 
Airbnb_prune_tree<- prune(Airbnbdefault.tree, cp=0.015443) # from cptable   
summary(Airbnb_prune_tree)

# plot the pruned tree 
plot(Airbnb_prune_tree, uniform=TRUE, 
     main="Pruned Regression Tree for Airbnb Price")
text(Airbnb_prune_tree, use.n=TRUE, all=TRUE, cex=.8)

library(caret)
Airbnb_prune_pred <- predict(Airbnb_prune_tree, newdata = valid1.data)
RMSE(pred = Airbnb_prune_pred, obs = valid1.data$price) #best RMSE is 68

#Bootstrap Tree - constructs default 25 trees.

library(ipred)       # bagging
library(caret)       # bagging
library(mlr)

bagged_m1 <- bagging(
  formula = price ~ .,
  data    = train1.data,
  coob    = TRUE
)

bagged_m1 

# Found out accuracy on the Bootstrap tree for airbnb

library(caret)
Airbnb_prune_pred <- predict(bagged_m1, newdata = valid1.data)
RMSE(pred = Airbnb_prune_pred, obs = valid1.data$price) #best RMSE is 66 On validation data

#Random Forest approach
library(tidyverse)

train.data_Rf<-train1.data
valid.data_Rf<-valid1.data
airbnb.df_new_Rf <-airbnb.df_new

colnames(airbnb.df_new)
# Rename column_Name
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Barking and Dagenham"]<- "neighbourhood_Barking_and_Dagenham" #7
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Hammersmith and Fulham"]<-"neighbourhood_Hammersmith_and_Fulham"#13
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Kingston upon Thames"]<-"neighbourhood_Kingston_upon_Thames"#19
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Richmond upon Thames"]<-"neighbourhood_Richmond_upon_Thames"#33
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Kensington and Chelsea"]<-"neighbourhood_Kensington_and_Chelsea"
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Tower Hamlets"]<-"neighbourhood_Tower_Hamlets"
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "neighbourhood_Waltham Forest"]<-"neighbourhood_Waltham_Forest"
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) == "room_type_Entire home/apt" ]<-"room_type_Entire_home_apt"
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf) =="room_type_Hotel room"]<-"room_type_Hotel_room"                        
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf)== "room_type_Private room"]<-"room_type_Private_room"                          
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf)=="room_type_Shared room"]<-"room_type_Shared_room"
names(airbnb.df_new_Rf)[names(airbnb.df_new_Rf)=="neighbourhood_City of London"]<-"neighbourhood_CityofLondon"

names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Barking and Dagenham"]<- "neighbourhood_Barking_and_Dagenham" #7
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Hammersmith and Fulham"]<-"neighbourhood_Hammersmith_and_Fulham"#13
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Kingston upon Thames"]<-"neighbourhood_Kingston_upon_Thames"#19
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Richmond upon Thames"]<-"neighbourhood_Richmond_upon_Thames"#33
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Kensington and Chelsea"]<-"neighbourhood_Kensington_and_Chelsea"
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Tower Hamlets"]<-"neighbourhood_Tower_Hamlets"
names(train.data_Rf)[names(train.data_Rf) == "neighbourhood_Waltham Forest"]<-"neighbourhood_Waltham_Forest"
names(train.data_Rf)[names(train.data_Rf) == "room_type_Entire home/apt" ]<-"room_type_Entire_home_apt"
names(train.data_Rf)[names(train.data_Rf) =="room_type_Hotel room"]<-"room_type_Hotel_room"                        
names(train.data_Rf)[names(train.data_Rf)== "room_type_Private room"]<-"room_type_Private_room"                          
names(train.data_Rf)[names(train.data_Rf)=="room_type_Shared room"]<-"room_type_Shared_room"
names(train.data_Rf)[names(train.data_Rf)=="neighbourhood_City of London"]<-"neighbourhood_CityofLondon"

names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Barking and Dagenham"]<- "neighbourhood_Barking_and_Dagenham" #7
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Hammersmith and Fulham"]<-"neighbourhood_Hammersmith_and_Fulham"#13
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Kingston upon Thames"]<-"neighbourhood_Kingston_upon_Thames"#19
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Richmond upon Thames"]<-"neighbourhood_Richmond_upon_Thames"#33
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Kensington and Chelsea"]<-"neighbourhood_Kensington_and_Chelsea"
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Tower Hamlets"]<-"neighbourhood_Tower_Hamlets"
names(valid.data_Rf)[names(valid.data_Rf) == "neighbourhood_Waltham Forest"]<-"neighbourhood_Waltham_Forest"
names(valid.data_Rf)[names(valid.data_Rf) == "room_type_Entire home/apt" ]<-"room_type_Entire_home_apt"
names(valid.data_Rf)[names(valid.data_Rf) =="room_type_Hotel room"]<-"room_type_Hotel_room"                        
names(valid.data_Rf)[names(valid.data_Rf)== "room_type_Private room"]<-"room_type_Private_room"                          
names(valid.data_Rf)[names(valid.data_Rf)=="room_type_Shared room"]<-"room_type_Shared_room"
names(valid.data_Rf)[names(valid.data_Rf)=="neighbourhood_City of London"]<-"neighbourhood_CityofLondon"

t(t(names(airbnb.df_new_Rf)))
t(t(names(train.data_Rf)))
t(t(names(valid.data_Rf)))

library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)
Airbnb_Rf_tree_model <- randomForest(
  formula = price~ .,
  data    = train.data_Rf
) 

## variable importance plot
varImpPlot(Airbnb_Rf_tree_model)

library(caret)
Airbnb_Rf_pred <- predict(Airbnb_Rf_tree_model, newdata = valid.data_Rf) # RMSE: 52.83
RMSE(pred = Airbnb_Rf_pred, obs = valid.data_Rf$price)

#-------------------------------------------------------------------------------

# Train Data and Valid data for Multiple regression

train2.data <- train.data[,selected.var]
valid2.data <- valid.data[,selected.var]

#Multiple variable regression
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.

mvr<- lm(price ~., data=train2.data)

#  use options() to ensure numbers are not displayed in scientific notation.
#Summarize using summary()

options(scipen = 999) #avoid scientific notation, easy to read
summary(mvr)

######
###########we ran using traing now predict it to see how good the model is
library(forecast)
# use predict() to make predictions on a new set.

pred <- predict(mvr, newdata = valid2.data)

# to calculate residuals
valid.res <- data.frame(valid2.data$price, pred, residual =
                          valid2.data$price - pred)

# Histogram of residual
hist(valid.res$residual,xlab = "Residuals",main="Histogram of Residuals")


# use accuracy() to compute common accuracy measures.
accuracy(pred, valid2.data$price)
indices <- 1:dim(valid2.data)[1]
plot(indices[1:50], pred[1:50], type = "l", col="red", ylab = "Price" ) 
lines(indices[1:50], valid2.data$price[1:50], col="blue")
legend(1,200 , legend=c("Predicted Value", "Actual Values"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



#--------------------------------------------------------------------------------------------------
# Neural Network Model

library(neuralnet)
selected.variables1<- c(3,4,5,6,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30)
airbnb2.df<- airbnb.df[,selected.variables1]
airbnb2.df<-dummy.data.frame(airbnb2.df,sep='.')
str(airbnb2.df)
train3.data<-train.data[,selected.variables1]
valid3.data<-valid.data[,selected.variables1]
#Normalizing the data
maxs <- apply(airbnb2.df, 2, max)  
mins <- apply(airbnb2.df, 2, min)

scaled <- as.data.frame(scale(airbnb2.df, center = mins, scale = maxs - mins))
head(scaled)


#Scaling the data 
scaled.train.df <- scaled[train.rows,]
scaled.valid.df <- scaled[valid.rows,]

#Neural network model
nn1 <- neuralnet(price ~ ., data = scaled.train.df, linear.output=T, hidden = 2,stepmax = 10e+6)

# plot network
plot(nn1, rep="best")

# display weights 
nn1$weights

#Predicting the values
nn1.pred <- compute(nn1, scaled.valid.df)
nn1.pred

#Predictions are also scaled based on our scale. Therefore we need to scale it back to the real-world in order to compute MSE
pr.nn <- nn1.pred$net.result * (maxs['price'] - mins['price']) + mins['price']
valid.r <- scaled.valid.df$price * (maxs['price'] - mins['price']) + mins['price']

#predictions for the first couple rows.
head(pr.nn)
head(valid.r)

#Calculating MSE
RMSE1.nn <- (sum((valid.r - pr.nn)^2) / nrow(valid3.data)) ^ 0.5
RMSE1.nn

indices <- 1:dim(valid3.data)[1]
plot(indices[1:50], pred[1:50], type = "l", col="red", ylab = "Price" ) 
lines(indices[1:50], valid3.data$price[1:50], col="blue")
legend(1,200 , legend=c("Predicted Value", "Actual Values"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


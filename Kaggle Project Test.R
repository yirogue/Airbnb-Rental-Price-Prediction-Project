#First Submission
analysisData = read.csv('analysisData.csv')
model1 = lm(price~minimum_nights+review_scores_rating,data = analysisData)
# read in scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model1,newdata=scoringData)
# construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'first_submission.csv',row.names = F)

#CLEAN DATA
airbnb = read.csv('airbnb.csv')

dim(airbnb)

sum(is.na(airbnb$price))
index <- airbnb$price>0
airbnb = airbnb[index, ]
table(airbnb$price)

sum(is.na(airbnb$square_feet))
airbnb$host_response_rate[airbnb$host_response_rate == "N/A"] <- NA
airbnb$host_response_time[airbnb$host_response_time == "N/A"] <- NA

#Second Submission

model2 <- lm(price~host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy,data = airbnb)
summary(model2)

pred1 = predict(model2, newdata = scoringData)
submissionFile1 = data.frame(id = scoringData$id, price = pred1)
write.csv(submissionFile1, 'second_submission.csv', row.names = F)

sum(is.na(airbnb$reviews_per_month))

#Third Submission
airbnb <- airbnb[!is.na(airbnb$beds), ]
airbnb <- airbnb[!is.na(airbnb$reviews_per_month), ]

model3 <- lm(price~host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month,data = airbnb)
summary(model3)

scoringData$beds[is.na(scoringData$beds)] <- -1
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] <- -1

pred2 = predict(model3, newdata = scoringData)
submissionFile2 = data.frame(id = scoringData$id, price = pred2)
write.csv(submissionFile2, 'third_submission.csv', row.names = F)

#Forth Submission
sum(is.na(scoringData$availability_30))
sum(is.na(scoringData$cleaning_fee))
airbnb <- airbnb[!is.na(airbnb$cleaning_fee), ]
airbnb <- airbnb[!is.na(airbnb$security_deposit),]

model4 <- lm(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews,data = airbnb)
summary(model4)

scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- -1
scoringData$security_deposit[is.na(scoringData$security_deposit)] <- -1

pred3 = predict(model4, newdata = scoringData)
submissionFile3 = data.frame(id = scoringData$id, price = pred3)
write.csv(submissionFile3, 'forth_submission.csv', row.names = F)

#split sample to test model rmse
install.packages('caTools')
library(caTools)
set.seed(100)
train.index <- sample.split(airbnb$price, SplitRatio = .8, group = NULL)
train <- airbnb[train.index, ]    
test <- airbnb[!train.index, ]

modelt <- lm(price~require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews,data = train)
modelt <- lm(price~security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30, data = train)
modelt <- lm(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews, data = train)

summary(modelt)

train1 = train
test1 = test

sum(is.na(test$security_deposit))
index <- is.na(train1$security_deposit)
train <- train1[!index, ]

sum(is.na(train1$reviews_per_month))
test$reviews_per_month[is.na(test$reviews_per_month)] <- -1

test$security_deposit[is.na(test1$security_deposit)] <- -1

predt = predict(modelt, newdata = test)
rmse = sqrt(mean((predt - test1$price)^2))
rmse

sum(is.na(airbnb$extra_people))

#forward Stepwise Selection
start_mod = lm(price~1,data = train)
empty_mod = lm(price~1, data = train)
full_mod = lm(price~require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews, data = train)
forwardStepwise = step(start_mod, scope = list(upper=full_mod,lower=empty_mod),direction = 'forward')

summary(forwardStepwise)

require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews

#backward Stepwise Selection
start_mod = lm(price~require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews, data = train)
empty_mod = lm(price~1, data = train)
full_mod = lm(price~require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews, data = train)
backwardStepwise = step(start_mod, scope = list(upper=full_mod,lower=empty_mod),direction = 'backward')

summary(backwardStepwise)

#boost model
install.packages('gbm')
library(gbm)
install.packages('caret')
library(caret)

set.seed(100)
train1$require_guest_phone_verification <- as.factor(train1$require_guest_phone_verification)
train1$host_is_superhost <- as.factor(train1$host_is_superhost)
train1$host_identity_verified <- as.factor(train1$host_identity_verified)
train1$neighbourhood_group_cleansed <- as.factor(train1$neighbourhood_group_cleansed)
train1$room_type <- as.factor(train1$room_type)
train1$cancellation_policy <- as.factor(train1$cancellation_policy)
train1$is_business_travel_ready <- as.factor(train1$is_business_travel_ready)

set.seed(100)
trControl=trainControl(method="cv",number=10)
tuneGrid=  expand.grid(n.trees = 1000, interaction.depth = c(1,2),
                       shrinkage = (1:10)*0.01,n.minobsinnode=5)
cvBoost = train(price~require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews,data=train1,method="gbm", 
                trControl=trControl, tuneGrid=tuneGrid)

boost = gbm(price~require_guest_phone_verification+calculated_host_listings_count+review_scores_accuracy+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+is_business_travel_ready+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+availability_365+number_of_reviews, data = train1, distribution = "gaussian", n.trees = cvBoost$bestTune$n.trees, interaction.depth = cvBoost$bestTune$interaction.depth, shrinkage = cvBoost$bestTune$shrinkage, n.minobsinnode = cvBoost$bestTune$n.minobsinnode)
predBoost = predict(boost, newdata = test, n.trees = 1000)

rmse = sqrt(mean((predBoost - test$price)^2))
rmse

#tree
library(rpart)

tree = rpart(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews, data = train1)
predTree = predict(tree, newdata = test)
rmseTree = sqrt(mean((predTree - test$price)^2))
rmseTree

trControl = trainControl(method = "cv", number = 10)
tuneGrid = expand.grid(.cp = seq(0.001,0.1,0.001))
set.seed(100)
cvModel = train(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews, data = train, method = "rpart", trControl=trControl, tuneGrid=tuneGrid)

treeCV = rpart(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews, data = train, control = rpart.control(cp = cvModel$bestTune))
predTreeCV = predict(treeCV, newdata = test)
rmseCV = sqrt(mean((predTreeCV-test$price)^2))
rmseCV

install.packages('randomForest')
library(randomForest)

test1$require_guest_phone_verification <- as.factor(test1$require_guest_phone_verification)
test1$host_is_superhost <- as.factor(test1$host_is_superhost)
test1$host_identity_verified <- as.factor(test1$host_identity_verified)
test1$neighbourhood_group_cleansed <- as.factor(test1$neighbourhood_group_cleansed)
test1$room_type <- as.factor(test1$room_type)
test1$cancellation_policy <- as.factor(test1$cancellation_policy)
test1$is_business_travel_ready <- as.factor(test1$is_business_travel_ready)

set.seed(100)
bag1 = randomForest(price~require_guest_phone_verification+calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+host_is_superhost+host_identity_verified+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+cancellation_policy+beds+reviews_per_month+availability_30+availability_90+number_of_reviews, data = train1, mtry = 26, ntree = 1000)
predBag = predict(bag1, newdata = test1) 

levels(test1$is_business_travel_ready)

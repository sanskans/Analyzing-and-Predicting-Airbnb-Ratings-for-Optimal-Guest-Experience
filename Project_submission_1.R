library(tidyverse)
library(caret)
library(tree)
library(class)
library(dplyr)
library(tidyr)

airbnb <- read_csv("airbnb_all.csv") 

features<-c("accommodates", "availability_30",
            "availability_60", "bathrooms", "bed_type", "bedrooms",
            "beds", "cancellation_policy", "city_name", "cleaning_fee",
            "country_code", "extra_people",
            "guests_included", "host_acceptance_rate",
            "host_identity_verified", "host_is_superhost", "host_listings_count",
            "host_response_rate", "host_response_time", "instant_bookable",
            "is_location_exact","price",
            "property_type", "require_guest_phone_verification",
            "require_guest_profile_picture", "requires_license",
            "room_type", "security_deposit", "perfect_rating_score")
airbnb<-airbnb[,features]


#Remove dollar sign from cleaning fee
airbnb$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb$cleaning_fee))

#Convert cancellation policy and clean it
airbnb <- mutate(airbnb,
                 cancellation_policy = ifelse(
                   cancellation_policy %in% c("strict", "super_strict_30"), 'strict',  cancellation_policy))

#Remove rows containing UY and MX in country code
airbnb<-airbnb[!grepl("UY", airbnb$country_code),]
airbnb<-airbnb[!grepl("MX", airbnb$country_code),]


which(is.na(airbnb$property_type))
any(is.na(airbnb$require_guest_phone_verification))
any(is.na(airbnb$require_guest_profile_picture))
any(is.na(airbnb$requires_license))
any(is.na(airbnb$room_type))
sum(is.na(airbnb$security_deposit))
airbnb$security_deposit <- as.numeric(gsub("\\D", "", airbnb$security_deposit))

airbnb <- mutate(airbnb, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

airbnb$security_deposit <- airbnb$security_deposit / 100

airbnb <- airbnb %>%
  mutate(extra_people = as.numeric(gsub("\\$", "", extra_people)),
         charges_for_extra = as.factor(ifelse(extra_people> 0 , "YES","NO")),
         host_is_superhost= as.factor(ifelse(is.na(host_is_superhost),FALSE,host_is_superhost)),
         host_acceptance =as.factor(ifelse(is.na(host_acceptance_rate),"MISSING",(ifelse(host_acceptance_rate == "100%", "ALL","SOME")))),
         host_listings_count = ifelse(is.na(host_listings_count),mean(host_listings_count, na.rm = TRUE), host_listings_count),
         host_identity_verified =as.factor(ifelse(is.na(host_identity_verified),FALSE,host_identity_verified)),
         guests_included = as.factor(ifelse(as.numeric(gsub("\\$", "", guests_included)) > 0, "YES", "NO")),
         host_response= as.factor(ifelse(is.na(host_response_rate), "MISSING",
                                         ifelse(host_response_rate == "100%", "ALL", "SOME"))),
         host_response_time= as.factor(ifelse(is.na(host_response_time), "MISSING",
                                              ifelse(host_response_time == "within an hour" | 
                                                       host_response_time == "within a few hours" |
                                                       host_response_time == "within a day", 
                                                     "Quick", "Late"))),
         price = parse_number(price),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
         beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
         bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
         city_name= as.factor(city_name), 
         bedrooms= as.factor(bedrooms), 
         beds= as.factor(beds), 
         bathrooms= as.factor(bathrooms), 
         cancellation_policy= as.factor(cancellation_policy),
         bed_type = as.factor(bed_type),
         room_type=as.factor(room_type),
         perfect_rating_score=as.factor(perfect_rating_score), 
         property_category= as.factor(ifelse(property_type %in% c("Apartment", "Serviced apartment", "Loft"),"apartment",ifelse(property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel"), "hotel",ifelse(property_type %in% c("Townhouse", "Condominium"), "condo",ifelse(property_type %in% c("Bungalow", "House"), "house", "other"))))))


airbnb <- subset(airbnb, select = -c(extra_people,host_response_rate,host_acceptance_rate,country_code,property_type))
summary(airbnb)


log1formula <- perfect_rating_score ~ accommodates + availability_30 + availability_60 + bed_type +
  cancellation_policy + city_name + cleaning_fee + guests_included+ host_identity_verified +
  host_is_superhost + host_listings_count + host_response_time + 
  instant_bookable + is_location_exact + price + require_guest_phone_verification +
  require_guest_profile_picture + requires_license + room_type + security_deposit + 
  charges_for_extra + host_acceptance + property_category

# Do a simple partition of the data into 70% train/30% validation
set.seed(1)
train_insts <- sample(nrow(airbnb),.7*nrow(airbnb))
data_train <- airbnb[train_insts,]
data_valid <- airbnb[-train_insts,]

# Define a function that trains and predicts probabilities and classifies based on a cutoff c
tr_pred <- function(train_data, valid_data, model_formula){
  trained_model <- glm(data = train_data, model_formula, family = "binomial") 
  predictions <- predict(trained_model, newdata = valid_data, type = "response") 
  return(predictions)
}

# Define a function that uses scores to classify based on a cutoff c
classify <- function(scores, c){
  classifications <- ifelse(scores > c, "YES" , "NO") 
  return(classifications) 
}

probs1 <- tr_pred(data_train, data_valid, log1formula)
classifications1 <- classify(probs1, .5)

#actuals:
valid_actuals <- data_valid$perfect_rating_score

#predictions (classifications with cutoff of .5 from before:
valid_classifications <- as.factor(classifications1)


# confusionMatrix function from the caret package
# https://topepo.github.io/caret/measuring-performance.html
CM_1 = confusionMatrix(data = valid_classifications, #predictions
                       reference = valid_actuals, #actuals
                       positive="YES") #by default, will choose alphabetically first class
CM_1

CM_1$table

TP <- CM_1$table[2,2]

# 629 True Negatives (were predicted to be negatives, are actually negatives)
TN <- CM_1$table[1,1]

# 44 False Positives (were predicted to be positives, are actually negatives)
FP <- CM_1$table[2,1]

# 221 False Negatives (were predicted to be negatives, are actually positives)
FN <- CM_1$table[1,2]

#the confusionMatrix function also gives us some other derived metrics
CM_1$overall
CM_1$overall["Accuracy"]
as.numeric(CM_1$overall["Accuracy"])

CM_1$byClass

# We can generate lots of classification metrics based on the confusion matrix.
# These are the most important ones:

# TPR = True Positive Rate = Sensitivity = Recall
# Of all of the (actual) positives, how many did the classifier identify?
TPR <- TP/(TP+FN)
as.numeric(CM_1$byClass["Sensitivity"])

# TNR = True Negative Rate = Specificity
# Of all the (actual) negatives, how many did the classifier identify?
TNR <- TN/(TN+FP)
as.numeric(CM_1$byClass["Specificity"])

# FPR = False Positive Rate = 1-TNR
# Of all of the negatives, how many were incorrectly classified as positives?
FPR <- 1-TNR

# FNR = False Negative Rate = 1-TPR
# Of all of the positives, how many were incorrectly classified as negatives?
FNR = 1-TPR

airbnb_test <- read_csv("airbnb_test_x_2023.csv") 

features<-c("accommodates", "availability_30",
            "availability_60", "bathrooms", "bed_type", "bedrooms",
            "beds", "cancellation_policy", "city_name", "cleaning_fee",
            "country_code", "extra_people",
            "guests_included", "host_acceptance_rate",
            "host_identity_verified", "host_is_superhost", "host_listings_count",
            "host_response_rate", "host_response_time", "instant_bookable",
            "is_location_exact","price",
            "property_type", "require_guest_phone_verification",
            "require_guest_profile_picture", "requires_license",
            "room_type", "security_deposit")
airbnb_test<-airbnb_test[,features]


#Remove dollar sign from cleaning fee
airbnb_test$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb_test$cleaning_fee))

#Convert cancellation policy and clean it
airbnb_test <- mutate(airbnb_test,
                      cancellation_policy = ifelse(
                        cancellation_policy %in% c("strict", "super_strict_30"), 'strict',  cancellation_policy))

#Remove rows containing UY and MX in country code
airbnb_test<-airbnb_test[!grepl("UY", airbnb_test$country_code),]
airbnb_test<-airbnb_test[!grepl("MX", airbnb_test$country_code),]


which(is.na(airbnb_test$property_type))
any(is.na(airbnb_test$require_guest_phone_verification))
any(is.na(airbnb_test$require_guest_profile_picture))
any(is.na(airbnb_test$requires_license))
any(is.na(airbnb_test$room_type))
sum(is.na(airbnb_test$security_deposit))
airbnb_test$security_deposit <- as.numeric(gsub("\\D", "", airbnb_test$security_deposit))

airbnb_test <- mutate(airbnb_test, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

airbnb_test$security_deposit <- airbnb_test$security_deposit / 100

airbnb_test <- airbnb_test %>%
  mutate(extra_people = as.numeric(gsub("\\$", "", extra_people)),
         charges_for_extra = as.factor(ifelse(extra_people> 0 , "YES","NO")),
         host_is_superhost= as.factor(ifelse(is.na(host_is_superhost),FALSE,host_is_superhost)),
         host_acceptance =as.factor(ifelse(is.na(host_acceptance_rate),"MISSING",(ifelse(host_acceptance_rate == "100%", "ALL","SOME")))),
         host_listings_count = ifelse(is.na(host_listings_count),mean(host_listings_count, na.rm = TRUE), host_listings_count),
         host_identity_verified =as.factor(ifelse(is.na(host_identity_verified),FALSE,host_identity_verified)),
         guests_included = as.factor(ifelse(as.numeric(gsub("\\$", "", guests_included)) > 0, "YES", "NO")),
         host_response= as.factor(ifelse(is.na(host_response_rate), "MISSING",
                                         ifelse(host_response_rate == "100%", "ALL", "SOME"))),
         host_response_time= as.factor(ifelse(is.na(host_response_time), "MISSING",
                                              ifelse(host_response_time == "within an hour" | 
                                                       host_response_time == "within a few hours" |
                                                       host_response_time == "within a day", 
                                                     "Quick", "Late"))),
         price = parse_number(price),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
         beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
         bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
         city_name= as.factor(city_name), 
         bedrooms= as.factor(bedrooms), 
         beds= as.factor(beds), 
         bathrooms= as.factor(bathrooms), 
         cancellation_policy= as.factor(cancellation_policy),
         bed_type = as.factor(bed_type),
         room_type=as.factor(room_type), 
         property_category= as.factor(ifelse(property_type %in% c("Apartment", "Serviced apartment", "Loft"),"apartment",ifelse(property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel"), "hotel",ifelse(property_type %in% c("Townhouse", "Condominium"), "condo",ifelse(property_type %in% c("Bungalow", "House"), "house", "other"))))))


airbnb_test <- subset(airbnb_test, select = -c(extra_people,host_response_rate,host_acceptance_rate,country_code,property_type))
summary(airbnb_test)


tr_pred <- function(train_data, valid_data, model_formula){
  trained_model <- glm(data = train_data, model_formula, family = "binomial") 
  predictions <- predict(trained_model, newdata = valid_data, type = "response") 
  return(predictions)
}

# Define a function that uses scores to classify based on a cutoff c
classify <- function(scores, c){
  classifications <- ifelse(scores > c, "YES" , "NO") 
  return(classifications) 
}

probs1 <- tr_pred(airbnb, airbnb_test, log1formula)
classifications1 <- classify(probs1, .5)

write.csv(classifications1, "perfect_rating_score_group15.csv", row.names=FALSE)

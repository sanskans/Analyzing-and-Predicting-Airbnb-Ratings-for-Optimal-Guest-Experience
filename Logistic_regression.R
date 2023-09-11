library(tidyverse)
library(caret)
library(tree)
library(class)
library(dplyr)
library(tidyr)
library(glmnet)
options(scipen=999)

# import
airbnb <- read_csv("airbnb_all.csv") %>%
          select(accommodates, amenities, availability_30, bathrooms, bed_type, bedrooms, beds, cancellation_policy, cleaning_fee,
                 extra_people,first_review, guests_included, host_acceptance_rate, host_has_profile_pic, host_identity_verified,
                 host_is_superhost, host_listings_count, host_response_rate, host_response_time, city_name, instant_bookable,
                 is_location_exact, maximum_nights, minimum_nights, monthly_price, price, property_type,
                 require_guest_phone_verification, require_guest_profile_picture, requires_license, room_type, security_deposit, weekly_price, perfect_rating_score)

#summary(airbnb)

#Remove dollar sign from cleaning fee
airbnb$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb$cleaning_fee))

#Convert cancellation policy and clean it
airbnb <- mutate(airbnb,
                 cancellation_policy = ifelse(
                   cancellation_policy %in% c("strict", "super_strict_30"), 'strict',  cancellation_policy))

which(is.na(airbnb$property_type))
any(is.na(airbnb$require_guest_phone_verification))
any(is.na(airbnb$require_guest_profile_picture))
any(is.na(airbnb$requires_license))
any(is.na(airbnb$room_type))
sum(is.na(airbnb$security_deposit))
airbnb$security_deposit <- as.numeric(gsub("\\D", "", airbnb$security_deposit))

airbnb <- mutate(airbnb, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

airbnb$security_deposit <- airbnb$security_deposit / 100

#Remove rows containing UY and MX in country code
airbnb<-airbnb[!grepl("UY", airbnb$country_code),]
airbnb<-airbnb[!grepl("MX", airbnb$country_code),]

airbnb <- airbnb %>%
  mutate(
         bathrooms = ifelse(bathrooms>10, 10, bathrooms),
         bedrooms = ifelse(bedrooms>10, 10, bedrooms),
         beds = ifelse(beds>16, 16, beds),
         extra_people = as.numeric(gsub("\\$", "", extra_people)),
         charges_for_extra = as.factor(ifelse(extra_people> 0 , "YES","NO")),
         host_is_superhost= as.factor(ifelse(is.na(host_is_superhost),FALSE,host_is_superhost)),
         host_acceptance_rate = as.factor(ifelse(is.na(host_acceptance_rate),"MISSING",(ifelse(host_acceptance_rate == "100%", "ALL","SOME")))),
         host_listings_count = ifelse(is.na(host_listings_count),median(host_listings_count, na.rm = TRUE), host_listings_count),
         host_identity_verified =as.factor(ifelse(is.na(host_identity_verified),FALSE,host_identity_verified)),
         guests_included = as.factor(ifelse(as.numeric(gsub("\\$", "", guests_included)) > 0, "YES", "NO")),
         host_response_rate = as.factor(ifelse(is.na(host_response_rate), "MISSING",
                                               ifelse(host_response_rate == "100%", "ALL", "SOME"))),
         host_response_time= as.factor(ifelse(is.na(host_response_time), "MISSING",
                                              ifelse(host_response_time == "within an hour" |
                                                       host_response_time == "within a few hours" |
                                                       host_response_time == "within a day",
                                                     "Quick", "Late"))),
         price = as.numeric(price),
         cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
         bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
         beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
         bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
         city_name= as.factor(city_name),
         price_per_bedroom = round(price/bedrooms, 2),
         bedrooms= as.factor(bedrooms),
         beds= as.factor(beds),
         bathrooms= as.factor(bathrooms),
         cancellation_policy= as.factor(cancellation_policy),
         bed_type = as.factor(bed_type),
         room_type=as.factor(room_type),
         price = ifelse(is.na(price),0,price),
         price_per_person = round(price/accommodates, 2),
         perfect_rating_score=as.factor(perfect_rating_score),
         property_type = as.factor(ifelse(is.na(property_type), "Apartment", property_type)),
         property_category= as.factor(ifelse(property_type %in% c("Apartment", "Serviced apartment", "Loft"),"apartment",ifelse(property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel"), "hotel",ifelse(property_type %in% c("Townhouse", "Condominium"), "condo",ifelse(property_type %in% c("Bungalow", "House"), "house", "other"))))))

airbnb$amenities_n = nchar(as.character(airbnb$amenities))

summary(airbnb)

############## FEATURE SELECTION ##################################################

airbnb_for_dummy <- select(airbnb, accommodates, amenities_n, price_per_person, availability_30, bed_type,
                           cancellation_policy , cleaning_fee, guests_included,
                           host_is_superhost,host_listings_count,host_response_time,
                           instant_bookable, is_location_exact, require_guest_phone_verification,
                           require_guest_profile_picture ,room_type,security_deposit,
                           charges_for_extra, host_acceptance_rate, property_type)

# glmnet requires a matrix of dummy variables rather than factors
dummy <- dummyVars( ~ . , data = airbnb_for_dummy, fullRank = TRUE)
airbnb_dv <- predict(dummy, newdata =airbnb_for_dummy)
#view(airbnb_dv)

# remove the target variable from the matrix of features #145
airbnb_x <- airbnb_dv
#view(airbnb_x)

# movies_y is a factor
airbnb_y <- airbnb$perfect_rating_score

#I'm going to do a nested holdout here# Save the "test" data for the end to evaluate the chosen value of lambda
set.seed(1)
train <- sample(nrow(airbnb),.7*nrow(airbnb))
x_train <- airbnb_x[train,]
y_train <- airbnb_y[train]

x_valid <- airbnb_x[-train,]
y_valid <- airbnb_y[-train]

#family="binomial" yields logistic regression; family="gaussian" yields linear regression#alpha = 1 yields the lasso penalty, and alpha = 0 the ridge penalty#basic, default
glm.out.lasso <- glmnet(x_train, y_train, alpha = 1, family="binomial")

#shows coefficient values vs. lambda
plot(glm.out.lasso, xvar = "lambda")

# How do we choose the best lambda?# create a fitting curve in the validation data
#A "grid" is a list of values to try
#(called a grid because sometimes we want to try combinations of two variables)
#We usually say the method for tuning (hyper) parameters is called "grid search"
#many ways to specify a grid: either manually list the values you want to try
#or you can use the seq function to generate a large list
#this one starts at 10^-1, goes to 10^-4 in 100 steps
#I got to this grid by trial and error
grid <- 10^seq(-1,-4,length=100)
grid

#accuracy <- function(classifications, actuals){
#correct_classifications <- ifelse(classifications == actuals, 1, 0)
#acc <- sum(correct_classifications)/length(classifications)
#return(acc)}

#storage vector
accs <- rep(0, length(grid))

for(i in c(1:length(grid))){
  lam = grid[i] #current value of lambda
  
  #train a ridge model with lambda = lam
  glmout <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = lam)
  
  #make predictions as usual
  preds <- predict(glmout, newx = x_valid, type = "response")
  
  #classify and compute accuracy
  classifications_1 <- ifelse(preds > .5, "YES", "NO")
  correct_classifications <- ifelse(classifications_1 == y_valid, 1, 0)
  inner_acc <- sum(correct_classifications)/length(classifications_1)
  #inner_acc <- accuracy(classifications_1, y_valid)
  accs[i] <- inner_acc
}

#plot fitting curve - easier to read if we plot logs
plot(log10(grid), accs)

# get best-performing lambda
best_validation_index <- which.max(accs)
best_lambda <- grid[best_validation_index]
best_lambda
#print coefficients for best lambda
coef(glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda))

##################### MODEL SPECIFICAITON ####################################

log1formula <- perfect_rating_score ~ price_per_person + availability_30  + bed_type +
  cancellation_policy + city_name + cleaning_fee + guests_included+ host_identity_verified +
  host_is_superhost + host_listings_count + host_response_time +
  instant_bookable + is_location_exact + require_guest_phone_verification +
  require_guest_profile_picture + requires_license + room_type + security_deposit +
  charges_for_extra + host_acceptance_rate + property_category

log2formula <- perfect_rating_score ~ amenities_n +price_per_person + availability_30  + bed_type +
  cancellation_policy + city_name + cleaning_fee + guests_included +
  host_is_superhost + host_listings_count + host_response_time +
  instant_bookable + is_location_exact + require_guest_phone_verification +
  require_guest_profile_picture  + room_type + security_deposit +
  charges_for_extra + host_acceptance_rate + property_category + bathrooms + bedrooms + beds

#summary(airbnb)

#randomly shuffle the stroke data
airbnb_shuffle <- airbnb[sample(nrow(airbnb)),]

#define k = the number of folds
k <- 10

#separate data into k equally-sized folds
#cut() will assign "fold numbers" to each instance
folds <- cut(seq(1,nrow(airbnb_shuffle)),breaks=k,labels=FALSE)

# Make a vectors of zeros to store your performance in each fold using the rep function
log1_acc = rep(0, k)
log2_acc = rep(0, k)

log1_sensitivity = rep(0, k)
log2_sensitivity = rep(0, k)

log1_specificity = rep(0, k)
log2_specificity = rep(0, k)

classify <- function(scores, c){
  classifications <- ifelse(scores > c, "YES" , "NO")
  return(classifications)
}

#Use a for loop to repeat k times
for(i in 1:k){
  
  #Segment your data by fold using the which() function
  valid_inds <- which(folds==i, arr.ind=TRUE)
  valid_fold <- airbnb_shuffle[valid_inds, ]
  train_fold <- airbnb_shuffle[-valid_inds, ]
  
  valid_actuals <- valid_fold$perfect_rating_score
  
  # 1. train a model in the training data
  
  log1_model_cv = glm(log1formula, data = train_fold, family = "binomial")
  log2_model_cv = glm(log2formula, data = train_fold, family = "binomial")
  
  # 2. make predictions in the validation data
  
  log1_prob_cv <- predict(log1_model_cv, newdata = valid_fold, type = "response")
  log2_prob_cv <- predict(log2_model_cv, newdata = valid_fold, type = "response")
  
  # 3. classify
  
  #note: convert classifications into a factor
  #good practice to make sure levels match those in valid_actuals
  log1_classifications_cv <- factor(classify(log1_prob_cv, .5),
                                    levels = levels(valid_actuals))
  log2_classifications_cv <- factor(classify(log2_prob_cv, .5),
                                    levels = levels(valid_actuals))
  
  # 4. assess performance (accuracy, TPR, TNR)
  
  log1_CM_cv <- confusionMatrix(data = log1_classifications_cv,
                                reference = valid_actuals,
                                positive = "YES")
  log1_accuracy_cv <- as.numeric(log1_CM_cv$overall["Accuracy"])
  log1_TPR_cv <- as.numeric(log1_CM_cv$byClass["Sensitivity"])
  log1_TNR_cv <- as.numeric(log1_CM_cv$byClass["Specificity"])
  
  
  log2_CM_cv <- confusionMatrix(data = log2_classifications_cv,
                                reference = valid_actuals,
                                positive = "YES")
  log2_accuracy_cv <- as.numeric(log2_CM_cv$overall["Accuracy"])
  log2_TPR_cv <- as.numeric(log2_CM_cv$byClass["Sensitivity"])
  log2_TNR_cv <- as.numeric(log2_CM_cv$byClass["Specificity"])
  
  # 5. store performance in the appropriate place in the relevant vector
  
  log1_acc[i] <- log1_accuracy_cv
  log1_sensitivity[i] <- log1_TPR_cv
  log1_specificity[i] <- log1_TNR_cv
  
  log2_acc[i] <- log2_accuracy_cv
  log2_sensitivity[i] <- log2_TPR_cv
  log2_specificity[i] <- log2_TNR_cv

}

mean(log1_acc)
mean(log1_sensitivity)
mean(log1_specificity)

mean(log2_acc)
mean(log2_sensitivity)
mean(log2_specificity)

################################ SIMPLE PARITION LOGISTIC REGRESSION ##################################

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


probs1 <- tr_pred(data_train, data_valid, log2formula)
classifications1 <- classify(probs1, .433)

#actuals:
valid_actuals <- data_valid$perfect_rating_score

#predictions (classifications with cutoff of .5 from before:
valid_classifications <- as.factor(classifications1)


# confusionMatrix function from the caret package - VALIDATION DATA
CM_1 = confusionMatrix(data = valid_classifications, #predictions
                       reference = valid_actuals, #actuals
                       positive="YES") #by default, will choose alphabetically first class
CM_1

CM_1$table

summary(airbnb)


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
print(FPR)
print(TPR)
print(CM_1$overall["Accuracy"])



# # # # # # # # # # # # 

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


probs1 <- tr_pred(data_train, data_train, log2formula)
classifications1 <- classify(probs1, .433)

#actuals:
train_actuals <- data_train$perfect_rating_score

#predictions (classifications with cutoff of .5 from before:
train_classifications <- as.factor(classifications1)

# confusionMatrix function from the caret package - TRAINING DATA
CM_1 = confusionMatrix(data = train_classifications, #predictions
                       reference = train_actuals, #actuals
                       positive="YES") #by default, will choose alphabetically first class
CM_1

CM_1$table

#summary(airbnb)


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
print(FPR)
print(TPR)
print(CM_1$overall["Accuracy"])

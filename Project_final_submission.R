library(tidyverse)
library(caret)
library(tree)
library(class)
library(dplyr)
library(tidyr)
library(glmnet)
library(textdata)
library(tidytext)
library(ranger)
library(xgboost)
library(quanteda)
library(vip)
library(text2vec)
library(tm)
library(SnowballC)
library(vip)
library(stringr)
#install.packages("qdapTools")

library(qdapTools)

options(scipen=999)


airbnb_all <- read_csv("airbnb_all.csv")

living_wage <- read_csv("livingwage_us.csv")


airbnb_all$city_name <- tolower(airbnb_all$city_name)
living_wage$city <- tolower(living_wage$city)



airbnb <- merge(airbnb_all, living_wage, by.x = "city_name", by.y = "city", all.x = TRUE)

airbnb$amenities <- gsub(" ", "", airbnb$amenities)
airbnb$amenities <- gsub("-", "", airbnb$amenities)
airbnb$amenities <- gsub("_", "", airbnb$amenities)
airbnb$amenities <- gsub("[{()}]", "", airbnb$amenities)
airbnb <- airbnb %>%
  mutate(across(amenities, ~str_replace_all(.x, "[^[:alnum:]]"," "))) %>%
  mutate(across(amenities, ~str_to_lower(.x)))

airbnb$amenities_count <- str_count(airbnb$amenities, ",") + 1

split_amenities <- strsplit(airbnb$amenities, " ")
dummy_columns_amenities <- mtabulate(split_amenities)

dummy_columns_amenities <- select(dummy_columns_amenities,wifi, wirelessinternet, washer, tv, shampoo, petsallowed, lockonbedroomdoor,laptopfriendlyworkspace,kidfriendly, internet, iron, indoorfireplace, hangers, hairdryer, freeparkingonpremises, firstaidkit, fireextinguisher, dryer, cabletv, en, carbonmonoxidedetector, breakfast, airconditioning, V1)

# Add the dummy variables to the original dataframe


airbnb$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb$cleaning_fee))
airbnb$extra_people <- as.numeric(gsub("\\$|,","", airbnb$extra_people))



airbnb$security_deposit <- as.numeric(gsub("\\D", "", airbnb$security_deposit))
airbnb$security_deposit <- airbnb$security_deposit / 100
airbnb <- mutate(airbnb, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

airbnb <- airbnb %>%
  mutate(
    accommodates = ifelse(is.na(accommodates), median(accommodates, na.rm=TRUE), accommodates),
    availability_30 = ifelse(is.na(availability_30), median(availability_30, na.rm=TRUE), availability_30),
    availability_60 = ifelse(is.na(availability_60), median(availability_60, na.rm=TRUE), availability_60),
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
    price = parse_number(price),
    cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
    beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
    bedrooms= as.factor(bedrooms),
    beds= as.factor(beds),
    city_name= as.factor(city_name),
    bathrooms= as.factor(bathrooms),
    cancellation_policy= as.factor(cancellation_policy),
    bed_type = as.factor(bed_type),
    room_type=as.factor(room_type),
    price = ifelse(is.na(price),0,price),
    price_per_person = round(price/accommodates, 2),
    host_since = ifelse(is.na(host_since), mean(host_since, na.rm=TRUE), host_since),
    amenities_n = nchar(as.character(amenities)),
    desc_n = nchar(as.character(description)),
    desc_n = ifelse(is.na(desc_n), 0, desc_n),
    perfect_rating_score=as.factor(perfect_rating_score),
    fare_high = ifelse(one_adult_no_kids_living_wage>price_per_person, "High", "Low"),
    fare_high = ifelse(is.na(fare_high), "Missing", fare_high),
    population_2020 = ifelse(is.na(population_2020), mean(population_2020, na.rm = TRUE), population_2020),
    population_2010 = ifelse(is.na(population_2010), mean(population_2010, na.rm = TRUE), population_2010),
    pop_change = population_2020-population_2010,
    minimum_price = minimum_nights*price,
    maximum_price = maximum_nights*price,
    density = ifelse(is.na(density), mean(density, na.rm = TRUE), density),
    two_adults_both_working_no_kids_living_wage = ifelse(is.na(two_adults_both_working_no_kids_living_wage), mean(two_adults_both_working_no_kids_living_wage, na.rm = TRUE), two_adults_both_working_no_kids_living_wage),
    one_adult_no_kids_living_wage = ifelse(is.na(one_adult_no_kids_living_wage), mean(one_adult_no_kids_living_wage, na.rm = TRUE), one_adult_no_kids_living_wage),
    rank_2020 = scale(rank_2020),
    rank_2020 = ifelse(is.na(rank_2020), 2, rank_2020),
    months_diff_scale= scale(first_review, scale = FALSE),
    amenities_n = scale(amenities_n),
    density = scale(density),
    population_2020 = scale(population_2020), 
    property_type = as.factor(ifelse(is.na(property_type), "Apartment", property_type)),
    property_type= as.factor(ifelse(property_type %in% c("Apartment", "Serviced apartment", "Loft"),"apartment",ifelse(property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel"), "hotel",ifelse(property_type %in% c("Townhouse", "Condominium"), "condo",ifelse(property_type %in% c("Bungalow", "House"), "house", "other"))))))



airbnb <- cbind(airbnb, dummy_columns_amenities)


summary(airbnb)


###################

airbnb_sentiment <- airbnb %>%
  select(description, id)

cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    #stemDocument %>%
    word_tokenizer 
}

it_train = itoken(airbnb_sentiment$description, 
                  preprocessor = tolower, 
                  tokenizer = cleaning_tokenizer, 
                  ids = airbnb_sentiment$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)

vectorizer = vocab_vectorizer(vocab_small)
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == 'negative')

bing_positive <- get_sentiments("bing") %>%
  filter(sentiment == 'positive')

mydict <- dictionary(list(negative_desc = bing_negative$word, positive_desc = bing_positive$word))

vectorizer = vocab_vectorizer(vocab)
va_dfm <- as.dfm(dtm_train)
sentiments <- dfm_lookup(va_dfm, mydict, valuetype = 'fixed')
sentiments <- convert(sentiments, to = "data.frame") 

sentiments<-select(sentiments, doc_id, negative_desc, positive_desc)

airbnb <- merge(airbnb, sentiments, by.x = "id", by.y = "doc_id", all = TRUE)

airbnb <- airbnb %>%
  mutate(desc_score = positive_desc-negative_desc)




airbnb_rf <- select(airbnb,first_review, guests_included,
                    require_guest_profile_picture, room_type, security_deposit, 
                    charges_for_extra, host_is_superhost, host_listings_count, host_response_time,
                    instant_bookable,is_location_exact,require_guest_phone_verification, 
                    bed_type,cancellation_policy, city_name, property_type,host_acceptance_rate, 
                    security_deposit, price_per_person, extra_people, cleaning_fee, availability_30,availability_60, accommodates,
                    amenities_n,bathrooms,bedrooms, beds, host_since,minimum_price, 
                    requires_license,minimum_nights, maximum_nights, desc_score,density,population_2020, 
                    one_adult_no_kids_living_wage, two_adults_both_working_no_kids_living_wage, rank_2020,
                    wifi, wirelessinternet, washer, tv, shampoo, petsallowed, lockonbedroomdoor,
                    laptopfriendlyworkspace,kidfriendly, internet, iron, indoorfireplace, hangers, 
                    hairdryer, freeparkingonpremises, firstaidkit, fireextinguisher, dryer, cabletv, en, 
                    carbonmonoxidedetector, breakfast, airconditioning, V1, months_diff_scale)



##Fitting curve: tree_size
tree_sizes = c(400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1500)

va_acc <- rep(0, length(tree_sizes))
tr_acc <- rep(0, length(tree_sizes))

va_tpr <- rep(0, length(tree_sizes))
tr_tpr <- rep(0, length(tree_sizes))

va_fpr <- rep(0, length(tree_sizes))
tr_fpr <- rep(0, length(tree_sizes))

num_trees<-length(tree_sizes)

for(i in c(1:num_trees)){
  set.seed(1)
  tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
  tr_dtm <- airbnb_rf[tr_rows,]
  va_dtm <- airbnb_rf[-tr_rows,]
  
  rf.mod <- ranger(x = tr_dtm, y = airbnb[tr_rows,]$perfect_rating_score,
                   mtry=13, num.trees=tree_sizes[i],
                   importance="impurity",
                   probability = TRUE)
  
  rf_preds_training <- predict(rf.mod, data=tr_dtm)$predictions[,2]
  rf_preds <- predict(rf.mod, data=va_dtm)$predictions[,2]
  
  rf_classifications_training <- ifelse(rf_preds_training>0.495, "YES", "NO")
  rf_classifications <- ifelse(rf_preds>0.4955, "YES", "NO")
  
  rf_acc_training <- mean(ifelse(rf_classifications_training == airbnb[tr_rows,]$perfect_rating_score, 1, 0))
  rf_acc <- mean(ifelse(rf_classifications == airbnb[-tr_rows,]$perfect_rating_score, 1, 0))
  
  rf_tpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  rf_tp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR_tr <-rf_tpt/(rf_tpt+rf_fnt)
  FPR_tr<-rf_fpt/(rf_fpt+rf_tnt)
  
  TPR <-rf_tp/(rf_tp+rf_fn)
  FPR<-rf_fp/(rf_fp+rf_tn)

  tr_acc[i]<-rf_acc_training
  va_acc[i]<-rf_acc
  tr_tpr[i]<-TPR_tr
  va_tpr[i]<-TPR
  tr_fpr[i]<-FPR_tr
  va_fpr[i]<-FPR
  
  
}

plot(tree_sizes, tr_acc, type = "o", col = "blue", xlab = "Tree Size", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.7, 1))
points(tree_sizes, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(tree_sizes, tr_tpr, type = "o", col = "blue", xlab = "Tree Size", ylab = "TPR",
     main = "Fitting curve: Training and Validation TPR", ylim = c(0.4, 1))
points(tree_sizes, va_tpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(tree_sizes, tr_fpr, type = "o", col = "blue", xlab = "Tree Size", ylab = "FPR",
     main = "Fitting curve: Training and Validation FPR", ylim = c(0, 0.1))
points(tree_sizes, va_fpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

##Fitting curve: mtry
mtry = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

va_acc <- rep(0, length(mtry))
tr_acc <- rep(0, length(mtry))

va_tpr <- rep(0, length(mtry))
tr_tpr <- rep(0, length(mtry))

va_fpr <- rep(0, length(mtry))
tr_fpr <- rep(0, length(mtry))

num_mtry<-length(mtry)

for(i in c(1:num_mtry)){
  set.seed(1)
  tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
  tr_dtm <- airbnb_rf[tr_rows,]
  va_dtm <- airbnb_rf[-tr_rows,]
  
  rf.mod <- ranger(x = tr_dtm, y = airbnb[tr_rows,]$perfect_rating_score,
                   mtry=mtry[i], num.trees=500,
                   importance="impurity",
                   probability = TRUE)
  
  rf_preds_training <- predict(rf.mod, data=tr_dtm)$predictions[,2]
  rf_preds <- predict(rf.mod, data=va_dtm)$predictions[,2]
  
  rf_classifications_training <- ifelse(rf_preds_training>0.495, "YES", "NO")
  rf_classifications <- ifelse(rf_preds>0.4955, "YES", "NO")
  
  rf_acc_training <- mean(ifelse(rf_classifications_training == airbnb[tr_rows,]$perfect_rating_score, 1, 0))
  rf_acc <- mean(ifelse(rf_classifications == airbnb[-tr_rows,]$perfect_rating_score, 1, 0))
  
  rf_tpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  rf_tp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR_tr <-rf_tpt/(rf_tpt+rf_fnt)
  FPR_tr<-rf_fpt/(rf_fpt+rf_tnt)
  
  TPR <-rf_tp/(rf_tp+rf_fn)
  FPR<-rf_fp/(rf_fp+rf_tn)
  
  print(mtry[i])
  
  tr_acc[i]<-rf_acc_training
  va_acc[i]<-rf_acc
  tr_tpr[i]<-TPR_tr
  va_tpr[i]<-TPR
  tr_fpr[i]<-FPR_tr
  va_fpr[i]<-FPR
  
  
}

plot(mtry, tr_acc, type = "o", col = "blue", xlab = "mtry", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.65, 1))
points(mtry, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(mtry, tr_tpr, type = "o", col = "blue", xlab = "mtry", ylab = "TPR",
     main = "Fitting curve: Training and Validation TPR", ylim = c(0.4, 1))
points(mtry, va_tpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(mtry, tr_fpr, type = "o", col = "blue", xlab = "mtry", ylab = "FPR",
     main = "Fitting curve: Training and Validation FPR", ylim = c(0, 0.1))
points(mtry, va_fpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)



##Model Accuracy
tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
tr_dtm <- airbnb_rf[tr_rows,]
va_dtm <- airbnb_rf[-tr_rows,]


run <- function(airbnb_rf){
  set.seed(1)
  tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
  tr_dtm <- airbnb_rf[tr_rows,]
  va_dtm <- airbnb_rf[-tr_rows,]
  
  rf.mod <- ranger(x = tr_dtm, y = airbnb[tr_rows,]$perfect_rating_score,
                   mtry=13, num.trees=500,
                   importance="impurity",
                   probability = TRUE)
  
  rf_preds_training <- predict(rf.mod, data=tr_dtm)$predictions[,2]
  rf_preds <- predict(rf.mod, data=va_dtm)$predictions[,2]
  
  rf_classifications_training <- ifelse(rf_preds_training>0.4975, "YES", "NO")
  rf_classifications <- ifelse(rf_preds>0.4975, "YES", "NO")
  
  rf_acc_training <- mean(ifelse(rf_classifications_training == airbnb[tr_rows,]$perfect_rating_score, 1, 0))
  rf_acc <- mean(ifelse(rf_classifications == airbnb[-tr_rows,]$perfect_rating_score, 1, 0))
  
  rf_tpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  rf_tp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR_tr <-rf_tpt/(rf_tpt+rf_fnt)
  FPR_tr<-rf_fpt/(rf_fpt+rf_tnt)
  
  TPR <-rf_tp/(rf_tp+rf_fn)
  FPR<-rf_fp/(rf_fp+rf_tn)
  
  print(TPR)
  print(FPR)
  print(rf_acc)
  print(TPR_tr)
  print(FPR_tr)
  print(rf_acc_training)
  print(importance(rf.mod))
  
}

run(airbnb_rf)



#bagging
tree_sizes = c(400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 1300, 1350, 1400, 1450, 1500)

va_acc <- rep(0, length(tree_sizes))
tr_acc <- rep(0, length(tree_sizes))

va_tpr <- rep(0, length(tree_sizes))
tr_tpr <- rep(0, length(tree_sizes))

va_fpr <- rep(0, length(tree_sizes))
tr_fpr <- rep(0, length(tree_sizes))

num_trees<-length(tree_sizes)

for(i in c(1:num_trees)){
  set.seed(1)
  tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
  tr_dtm <- airbnb_rf[tr_rows,]
  va_dtm <- airbnb_rf[-tr_rows,]
  
  rf.mod <- ranger(x = tr_dtm, y = airbnb[tr_rows,]$perfect_rating_score,
                   mtry=13, num.trees=tree_sizes[i],
                   importance="impurity",
                   probability = TRUE)
  
  print(tree_sizes[i])
  rf_preds_training <- predict(rf.mod, data=tr_dtm)$predictions[,2]
  rf_preds <- predict(rf.mod, data=va_dtm)$predictions[,2]
  
  rf_classifications_training <- ifelse(rf_preds_training>0.495, "YES", "NO")
  rf_classifications <- ifelse(rf_preds>0.497, "YES", "NO")
  
  rf_acc_training <- mean(ifelse(rf_classifications_training == airbnb[tr_rows,]$perfect_rating_score, 1, 0))
  rf_acc <- mean(ifelse(rf_classifications == airbnb[-tr_rows,]$perfect_rating_score, 1, 0))
  
  rf_tpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  rf_tp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR_tr <-rf_tpt/(rf_tpt+rf_fnt)
  FPR_tr<-rf_fpt/(rf_fpt+rf_tnt)
  
  TPR <-rf_tp/(rf_tp+rf_fn)
  FPR<-rf_fp/(rf_fp+rf_tn)
  
  tr_acc[i]<-rf_acc_training
  va_acc[i]<-rf_acc
  tr_tpr[i]<-TPR_tr
  va_tpr[i]<-TPR
  tr_fpr[i]<-FPR_tr
  va_fpr[i]<-FPR
}

plot(tree_sizes, tr_acc, type = "o", col = "blue", xlab = "Tree Size", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.7, 1))
points(tree_sizes, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(tree_sizes, tr_tpr, type = "o", col = "blue", xlab = "Tree Size", ylab = "TPR",
     main = "Fitting curve: Training and Validation TPR", ylim = c(0.4, 1))
points(tree_sizes, va_tpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(tree_sizes, tr_fpr, type = "o", col = "blue", xlab = "Tree Size", ylab = "FPR",
     main = "Fitting curve: Training and Validation FPR", ylim = c(0, 0.1))
points(tree_sizes, va_fpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)




run <- function(airbnb_rf){
  set.seed(1)
  tr_rows <- sample(nrow(airbnb_rf),.7*nrow(airbnb_rf))
  tr_dtm <- airbnb_rf[tr_rows,]
  va_dtm <- airbnb_rf[-tr_rows,]
  
  rf.mod <- ranger(x = tr_dtm, y = airbnb[tr_rows,]$perfect_rating_score,
                   mtry=63, num.trees=500,
                   importance="impurity",
                   probability = TRUE)
  
  rf_preds_training <- predict(rf.mod, data=tr_dtm)$predictions[,2]
  rf_preds <- predict(rf.mod, data=va_dtm)$predictions[,2]
  
  rf_classifications_training <- ifelse(rf_preds_training>0.495, "YES", "NO")
  rf_classifications <- ifelse(rf_preds>0.5, "YES", "NO")
  
  rf_acc_training <- mean(ifelse(rf_classifications_training == airbnb[tr_rows,]$perfect_rating_score, 1, 0))
  rf_acc <- mean(ifelse(rf_classifications == airbnb[-tr_rows,]$perfect_rating_score, 1, 0))
  
  rf_tpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fpt <-sum(ifelse((rf_classifications_training =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fnt <-sum(ifelse((rf_classifications_training =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  rf_tp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  rf_tn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fp <-sum(ifelse((rf_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  rf_fn <-sum(ifelse((rf_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR_tr <-rf_tpt/(rf_tpt+rf_fnt)
  FPR_tr<-rf_fpt/(rf_fpt+rf_tnt)
  
  TPR <-rf_tp/(rf_tp+rf_fn)
  FPR<-rf_fp/(rf_fp+rf_tn)
  
  print(TPR)
  print(FPR)
  print(rf_acc)
  print(TPR_tr)
  print(FPR_tr)
  print(rf_acc_training)
  
}

run(airbnb_rf)






##Predict outcome on the testing file
airbnb_test_all <- read_csv("airbnb_test_x_2023.csv")

living_wage <- read_csv("livingwage_us.csv")


airbnb_test_all$city_name <- tolower(airbnb_test_all$city_name)
living_wage$city <- tolower(living_wage$city)



airbnb_test <- merge(airbnb_test_all, living_wage, by.x = "city_name", by.y = "city", all.x = TRUE)

airbnb_test$amenities <- gsub(" ", "", airbnb_test$amenities)
airbnb_test$amenities <- gsub("-", "", airbnb_test$amenities)
airbnb_test$amenities <- gsub("_", "", airbnb_test$amenities)
airbnb_test$amenities <- gsub("[{()}]", "", airbnb_test$amenities)
airbnb_test <- airbnb_test %>%
  mutate(across(amenities, ~str_replace_all(.x, "[^[:alnum:]]"," "))) %>%
  mutate(across(amenities, ~str_to_lower(.x)))

airbnb_test$amenities_count <- str_count(airbnb_test$amenities, ",") + 1

split_amenities <- strsplit(airbnb_test$amenities, " ")
dummy_columns_amenities <- mtabulate(split_amenities)

dummy_columns_amenities <- select(dummy_columns_amenities,wifi, wirelessinternet, washer, tv, shampoo, petsallowed, lockonbedroomdoor,laptopfriendlyworkspace,kidfriendly, internet, iron, indoorfireplace, hangers, hairdryer, freeparkingonpremises, firstaidkit, fireextinguisher, dryer, cabletv, en, carbonmonoxidedetector, breakfast, airconditioning, V1)

# Add the dummy variables to the original dataframe
airbnb_test$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb_test$cleaning_fee))
airbnb_test$extra_people <- as.numeric(gsub("\\$|,","", airbnb_test$extra_people))



airbnb_test$security_deposit <- as.numeric(gsub("\\D", "", airbnb_test$security_deposit))
airbnb_test$security_deposit <- airbnb_test$security_deposit / 100
airbnb_test <- mutate(airbnb_test, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

airbnb_test <- airbnb_test %>%
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
    price = parse_number(price),
    cleaning_fee = ifelse(is.na(cleaning_fee), 0, cleaning_fee),
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
    beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
    bedrooms= as.factor(bedrooms),
    beds= as.factor(beds),
    city_name= as.factor(city_name),
    bathrooms= as.factor(bathrooms),
    cancellation_policy= as.factor(cancellation_policy),
    bed_type = as.factor(bed_type),
    room_type=as.factor(room_type),
    price = ifelse(is.na(price),0,price),
    price_per_person = round(price/accommodates, 2),
    host_since = ifelse(is.na(host_since), mean(host_since, na.rm=TRUE), host_since),
    amenities_n = nchar(as.character(amenities)),
    desc_n = nchar(as.character(description)),
    desc_n = ifelse(is.na(desc_n), 0, desc_n),
    fare_high = ifelse(one_adult_no_kids_living_wage>price_per_person, "High", "Low"),
    fare_high = ifelse(is.na(fare_high), "Missing", fare_high),
    population_2020 = ifelse(is.na(population_2020), mean(population_2020, na.rm = TRUE), population_2020),
    population_2010 = ifelse(is.na(population_2010), mean(population_2010, na.rm = TRUE), population_2010),
    pop_change = population_2020-population_2010,
    minimum_price = minimum_nights*price,
    maximum_price = maximum_nights*price,
    density = ifelse(is.na(density), mean(density, na.rm = TRUE), density),
    two_adults_both_working_no_kids_living_wage = ifelse(is.na(two_adults_both_working_no_kids_living_wage), mean(two_adults_both_working_no_kids_living_wage, na.rm = TRUE), two_adults_both_working_no_kids_living_wage),
    one_adult_no_kids_living_wage = ifelse(is.na(one_adult_no_kids_living_wage), mean(one_adult_no_kids_living_wage, na.rm = TRUE), one_adult_no_kids_living_wage),
    rank_2020 = scale(rank_2020),
    rank_2020 = ifelse(is.na(rank_2020), 2, rank_2020),
    months_diff_scale= scale(first_review, scale = FALSE),
    amenities_n = scale(amenities_n),
    density = scale(density),
    population_2020 = scale(population_2020), 
    property_type = as.factor(ifelse(is.na(property_type), "Apartment", property_type)),
    property_type= as.factor(ifelse(property_type %in% c("Apartment", "Serviced apartment", "Loft"),"apartment",ifelse(property_type %in% c("Bed & Breakfast", "Boutique hotel", "Hostel"), "hotel",ifelse(property_type %in% c("Townhouse", "Condominium"), "condo",ifelse(property_type %in% c("Bungalow", "House"), "house", "other"))))))



airbnb_test <- cbind(airbnb_test, dummy_columns_amenities)


summary(airbnb_test)


###################

airbnb_test_sentiment <- airbnb_test %>%
  select(description, id)

cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    #stemDocument %>%
    word_tokenizer 
}

it_train = itoken(airbnb_test_sentiment$description, 
                  preprocessor = tolower, 
                  tokenizer = cleaning_tokenizer, 
                  ids = airbnb_test_sentiment$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)

vectorizer = vocab_vectorizer(vocab_small)
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == 'negative')

bing_positive <- get_sentiments("bing") %>%
  filter(sentiment == 'positive')

mydict <- dictionary(list(negative_desc = bing_negative$word, positive_desc = bing_positive$word))

vectorizer = vocab_vectorizer(vocab)
va_dfm <- as.dfm(dtm_train)
sentiments <- dfm_lookup(va_dfm, mydict, valuetype = 'fixed')
sentiments <- convert(sentiments, to = "data.frame") 

sentiments<-select(sentiments, doc_id, negative_desc, positive_desc)

airbnb_test <- merge(airbnb_test, sentiments, by.x = "id", by.y = "doc_id", all = TRUE)

airbnb_test <- airbnb_test %>%
  mutate(desc_score = positive_desc-negative_desc)




airbnb_test_rf <- select(airbnb_test,first_review, guests_included,
                         require_guest_profile_picture, room_type, security_deposit, 
                         charges_for_extra, host_is_superhost, host_listings_count, host_response_time,
                         instant_bookable,is_location_exact,require_guest_phone_verification, 
                         bed_type,cancellation_policy, city_name, property_type,host_acceptance_rate, 
                         security_deposit, price_per_person, extra_people, cleaning_fee, availability_30,availability_60, accommodates,
                         amenities_n,bathrooms,bedrooms, beds, host_since,minimum_price, 
                         requires_license,minimum_nights, maximum_nights, desc_score,density,population_2020, 
                         one_adult_no_kids_living_wage, two_adults_both_working_no_kids_living_wage, rank_2020,
                         wifi, wirelessinternet, washer, tv, shampoo, petsallowed, lockonbedroomdoor,
                         laptopfriendlyworkspace,kidfriendly, internet, iron, indoorfireplace, hangers, 
                         hairdryer, freeparkingonpremises, firstaidkit, fireextinguisher, dryer, cabletv, en, 
                         carbonmonoxidedetector, breakfast, airconditioning, V1, months_diff_scale)

rf.mod <- ranger(x = airbnb_rf, y = airbnb$perfect_rating_score,
                 mtry=13, num.trees=500,
                 importance="impurity",
                 probability = TRUE)

rf_preds <- predict(rf.mod, data=airbnb_test_rf)$predictions[,2]
rf_classifications <- ifelse(rf_preds>0.4975, "YES", "NO")


write.csv(rf_classifications, "perfect_rating_score_group15.csv", row.names=FALSE)


##Sentiment analysis: Just for demonstration
airbnb_sentiment <- read_csv("airbnb_all.csv") %>%
  select(house_rules, perfect_rating_score, id)

cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords(kind="en")) %>% #remove stopwords
    #stemDocument %>%
    word_tokenizer 
}

it_train = itoken(airbnb_sentiment$house_rules, 
                  preprocessor = tolower, 
                  tokenizer = cleaning_tokenizer, 
                  ids = airbnb_sentiment$id, 
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)

vectorizer = vocab_vectorizer(vocab_small)
dtm_train = create_dtm(it_train, vectorizer)
dim(dtm_train)

nrc_anger <-get_sentiments("nrc") %>%
  filter(sentiment == 'anger')
nrc_anticipation <-get_sentiments("nrc") %>%
  filter(sentiment == 'anticipation')
nrc_disgust <-get_sentiments("nrc") %>%
  filter(sentiment == 'disgust')
nrc_fear <-get_sentiments("nrc") %>%
  filter(sentiment == 'fear')
nrc_joy<-get_sentiments("nrc") %>%
  filter(sentiment == 'joy')
nrc_negative <-get_sentiments("nrc") %>%
  filter(sentiment == 'negative')
nrc_positive <-get_sentiments("nrc") %>%
  filter(sentiment == 'positive')
nrc_sadness <-get_sentiments("nrc") %>%
  filter(sentiment == 'sadness')
nrc_surprise <-get_sentiments("nrc") %>%
  filter(sentiment == 'surprise')
nrc_trust <-get_sentiments("nrc") %>%
  filter(sentiment == 'trust')

mydict <- dictionary(list(anger=nrc_anger$word, negative = nrc_anger$word, positive = nrc_anger$word, anticipation=nrc_anticipation$word, 
                          disgust=nrc_disgust$word, fear=nrc_fear$word, joy=nrc_joy$word, sadness=nrc_sadness$word, 
                          surprise=nrc_surprise$word, trust=nrc_trust$word))


vectorizer = vocab_vectorizer(vocab)
va_dfm <- as.dfm(dtm_train)
sentiments <- dfm_lookup(va_dfm, mydict, valuetype = 'fixed')
sentiments <- convert(sentiments, to = "data.frame") 





airbnb <- merge(airbnb, sentiments, by.x = "id", by.y = "doc_id", all = TRUE)


airbnb$first_review <- format(as.Date(airbnb$first_review), "%Y-%m")
library(zoo)
airbnb$first_review <- gsub("\"", "", airbnb$first_review)
airbnb$first_review <- as.yearmon(airbnb$first_review)
airbnb$months_diff <- as.integer((as.yearmon("2023-04") - airbnb$first_review) * 12)



airbnb$monthly_price <- as.numeric(gsub("\\$|,","", airbnb$monthly_price))
airbnb <- mutate(airbnb, monthly_price = ifelse(is.na(monthly_price), price*30, monthly_price))
airbnb <- mutate(airbnb, monthly_discount = (price*30)-monthly_price)


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
library(e1071)
library(stringr)
library(naivebayes)
library(nnet)
#install.packages("qdapTools")

library(qdapTools)

options(scipen=999)


airbnb_all <- read_csv("airbnb_all.csv")

living_wage <- read_csv("livingwage_us.csv")


airbnb_all$city_name <- tolower(airbnb_all$city_name)
living_wage$city <- tolower(living_wage$city)



airbnb <- merge(airbnb_all, living_wage, by.x = "city_name", by.y = "city", all.x = TRUE)


# Add the dummy variables to the original dataframe


airbnb$cleaning_fee <- as.numeric(gsub("\\$|,","", airbnb$cleaning_fee))
airbnb$extra_people <- as.numeric(gsub("\\$|,","", airbnb$extra_people))



airbnb$security_deposit <- as.numeric(gsub("\\D", "", airbnb$security_deposit))
airbnb$security_deposit <- airbnb$security_deposit / 100
airbnb <- mutate(airbnb, security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit))

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


#Create dummy variables for models that use only numeric values
airbnb_for_dummy <- select(airbnb, amenities_n , first_review,price_per_person, availability_30 , bed_type,
                           cancellation_policy , cleaning_fee, guests_included, city_name,
                           host_is_superhost,host_listings_count,host_response_time,
                           instant_bookable, is_location_exact, require_guest_phone_verification,
                           require_guest_profile_picture ,room_type,security_deposit,
                           charges_for_extra, host_acceptance_rate, property_type)

dummy <- dummyVars( ~ . , data = airbnb_for_dummy, fullRank = TRUE)
airbnb_dv <- predict(dummy, newdata =airbnb_for_dummy)
airbnb_dv <- cbind(airbnb_dv, airbnb$perfect_rating_score)




tr_rows <- sample(nrow(airbnb),.7*nrow(airbnb))
tr_dtm <- airbnb[tr_rows,]
va_dtm <- airbnb[-tr_rows,]


#BaselineModel
(table(airbnb$perfect_rating_score))
#We see that there are 70551 NO and 29430 YES. 
majority_class_model <- rep("NO", length(airbnb))
baseline_acc <- mean(ifelse(majority_class_model==airbnb$perfect_rating_score,1,0))
print(baseline_acc)

#neural network
#Fitting curves: size
size = c(1,2,3,4,5,6,7,8,9,10,11,12, 13)


va_acc <- rep(0, length(size))
tr_acc <- rep(0, length(size))

va_tpr <- rep(0, length(size))
tr_tpr <- rep(0, length(size))

va_fpr <- rep(0, length(size))
tr_fpr <- rep(0, length(size))

num_size<-length(size)

for(i in c(1:num_size)){
  nn=nnet(perfect_rating_score ~ amenities_n +price_per_person + availability_30  + bed_type +
            cancellation_policy + city_name + cleaning_fee + guests_included +
            host_is_superhost + host_listings_count + host_response_time +
            instant_bookable + is_location_exact + require_guest_phone_verification +
            require_guest_profile_picture  + room_type + security_deposit +
            charges_for_extra + host_acceptance_rate + property_type + bathrooms + bedrooms + beds
          ,data = tr_dtm, size=size[i], MaxNWts= 8000)
  
  
  nn_pred<-predict(nn, va_dtm)
  nn_classifications <- ifelse(nn_pred>0.47, "YES", "NO")
  
  nn_pred_tr <- predict(nn, tr_dtm) 
  nn_classifications_tr <- ifelse(nn_pred_tr>0.47, "YES", "NO")
  
  nn_acc <- mean(ifelse(nn_classifications==airbnb[-tr_rows,]$perfect_rating_score,1,0))
  nn_acc_tr <- mean(ifelse(nn_classifications_tr==airbnb[tr_rows,]$perfect_rating_score,1,0))

  nn_tp <-sum(ifelse((nn_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  nn_tn <-sum(ifelse((nn_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nn_fp <-sum(ifelse((nn_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nn_fn <-sum(ifelse((nn_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  nn_tp_tr <-sum(ifelse((nn_classifications_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  nn_tn_tr <-sum(ifelse((nn_classifications_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nn_fp_tr <-sum(ifelse((nn_classifications_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nn_fn_tr <-sum(ifelse((nn_classifications_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR <-nn_tp/(nn_tp+nn_fn)
  FPR<-nn_fp/(nn_fp+nn_tn)
  
  TPR_tr <-nn_tp_tr/(nn_tp_tr+nn_fn_tr)
  FPR_tr<-nn_fp_tr/(nn_fp_tr+nn_tn_tr)
  
  tr_acc[i]<-nn_acc_tr
  va_acc[i]<-nn_acc
  tr_tpr[i]<-TPR_tr
  va_tpr[i]<-TPR
  tr_fpr[i]<-FPR_tr
  va_fpr[i]<-FPR
  
}

plot(size, tr_acc, type = "o", col = "blue", xlab = "Size", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.7, 0.75))
points(size, va_acc, type = "o", col = "red")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(size, tr_tpr, type = "o", col = "blue", xlab = "Size", ylab = "TPR",
     main = "Fitting curve: Training and Validation TPR", ylim = c(0, 0.35))
points(size, va_tpr, type = "o", col = "red")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(size, tr_fpr, type = "o", col = "blue", xlab = "Size", ylab = "FPR",
     main = "Fitting curve: Training and Validation FPR", ylim = c(0, 0.15))
points(size, va_fpr, type = "o", col = "red")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)



nn=nnet(perfect_rating_score ~ amenities_n +price_per_person + availability_30  + bed_type +
          cancellation_policy + city_name + cleaning_fee + guests_included +
          host_is_superhost + host_listings_count + host_response_time +
          instant_bookable + is_location_exact + require_guest_phone_verification +
          require_guest_profile_picture  + room_type + security_deposit +
          charges_for_extra + host_acceptance_rate + property_type + bathrooms + bedrooms + beds
        ,data = tr_dtm, size=3, MaxNWts= 8000)


nn_pred<-predict(nn, va_dtm)
nn_classifications <- ifelse(nn_pred>0.47, "YES", "NO")

nn_pred_tr <- predict(nn, tr_dtm) 
nn_classifications_tr <- ifelse(nn_pred_tr>0.47, "YES", "NO")

nn_acc <- mean(ifelse(nn_classifications==airbnb[-tr_rows,]$perfect_rating_score,1,0))
nn_acc_tr <- mean(ifelse(nn_classifications_tr==airbnb[tr_rows,]$perfect_rating_score,1,0))

nn_tp <-sum(ifelse((nn_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
nn_tn <-sum(ifelse((nn_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nn_fp <-sum(ifelse((nn_classifications =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nn_fn <-sum(ifelse((nn_classifications =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))

nn_tp_tr <-sum(ifelse((nn_classifications_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
nn_tn_tr <-sum(ifelse((nn_classifications_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nn_fp_tr <-sum(ifelse((nn_classifications_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nn_fn_tr <-sum(ifelse((nn_classifications_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))

TPR <-nn_tp/(nn_tp+nn_fn)
FPR<-nn_fp/(nn_fp+nn_tn)

TPR_tr <-nn_tp_tr/(nn_tp_tr+nn_fn_tr)
FPR_tr<-nn_fp_tr/(nn_fp_tr+nn_tn_tr)

print(nn_acc)
print(TPR)
print(FPR)

print(nn_acc_tr)
print(TPR_tr)
print(FPR_tr)




# NAIVE BAYES

#fitting Curve: Laplace
laplace = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)

va_acc <- rep(0, length(laplace))
tr_acc <- rep(0, length(laplace))

va_tpr <- rep(0, length(laplace))
tr_tpr <- rep(0, length(laplace))

va_fpr <- rep(0, length(laplace))
tr_fpr <- rep(0, length(laplace))

num_laplace<-length(laplace)

for(i in c(1:num_laplace)){
  nb.mod <- naive_bayes(perfect_rating_score ~ amenities_n + first_review+price_per_person + availability_30  + bed_type +
                          cancellation_policy + city_name + cleaning_fee + guests_included +
                          host_is_superhost + host_listings_count + host_response_time +
                          instant_bookable + is_location_exact + require_guest_phone_verification +
                          require_guest_profile_picture  + room_type + security_deposit +
                          charges_for_extra + host_acceptance_rate + property_type + bathrooms + bedrooms + beds,
                        data=tr_dtm, laplase=laplace[i])
  
  nb_pred <- predict(nb.mod,  va_dtm, type = 'class') 
  nb_pred_tr <- predict(nb.mod,  tr_dtm, type = 'class') 
  
  nb_acc <- mean(ifelse(nb_pred==airbnb[-tr_rows,]$perfect_rating_score,1,0))
  nb_acc_tr <- mean(ifelse(nb_pred_tr==airbnb[tr_rows,]$perfect_rating_score,1,0))
  
  nb_tp <-sum(ifelse((nb_pred =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  nb_tn <-sum(ifelse((nb_pred =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nb_fp <-sum(ifelse((nb_pred =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nb_fn <-sum(ifelse((nb_pred =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  nb_tp_tr <-sum(ifelse((nb_pred_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  nb_tn_tr <-sum(ifelse((nb_pred_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nb_fp_tr <-sum(ifelse((nb_pred_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
  nb_fn_tr <-sum(ifelse((nb_pred_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
  
  TPR <-nb_tp/(nb_tp+nb_fn)
  FPR<-nb_fp/(nb_fp+nb_tn)
  
  TPR_tr <-nb_tp_tr/(nb_tp_tr+nb_fn_tr)
  FPR_tr<-nb_fp_tr/(nb_fp_tr+nb_tn_tr)
  
  tr_acc[i]<-nb_acc_tr
  va_acc[i]<-nb_acc
  tr_tpr[i]<-TPR_tr
  va_tpr[i]<-TPR
  tr_fpr[i]<-FPR_tr
  va_fpr[i]<-FPR
  
}

plot(laplace, tr_acc, type = "o", col = "blue", xlab = "laplace", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.719, 0.722))
points(laplace, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(laplace, tr_tpr, type = "o", col = "blue", xlab = "laplace", ylab = "TPR",
     main = "Fitting curve: Training and Validation TPR", ylim = c(0.25, 0.35))
points(laplace, va_tpr, type = "o", col = "red")
legend("right", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)

plot(laplace, tr_fpr, type = "o", col = "blue", xlab = "laplace", ylab = "FPR",
     main = "Fitting curve: Training and Validation FPR", ylim = c(0.09, 0.1))
points(laplace, va_fpr, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)
  
  

#Naive Bayes predictive model
nb.mod <- naive_bayes(perfect_rating_score ~ amenities_n + first_review+price_per_person + availability_30  + bed_type +
                        cancellation_policy + city_name + cleaning_fee + guests_included +
                        host_is_superhost + host_listings_count + host_response_time +
                        instant_bookable + is_location_exact + require_guest_phone_verification +
                        require_guest_profile_picture  + room_type + security_deposit +
                        charges_for_extra + host_acceptance_rate + property_type + bathrooms + bedrooms + beds,
                      data=tr_dtm)

nb_pred <- predict(nb.mod,  va_dtm, type = 'class') 
nb_pred_tr <- predict(nb.mod,  tr_dtm, type = 'class') 

nb_acc <- mean(ifelse(nb_pred==airbnb[-tr_rows,]$perfect_rating_score,1,0))
nb_acc_tr <- mean(ifelse(nb_pred_tr==airbnb[tr_rows,]$perfect_rating_score,1,0))

nb_tp <-sum(ifelse((nb_pred =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))
nb_tn <-sum(ifelse((nb_pred =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nb_fp <-sum(ifelse((nb_pred =="YES") & (airbnb[-tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nb_fn <-sum(ifelse((nb_pred =="NO") & (airbnb[-tr_rows,]$perfect_rating_score=="YES"), 1, 0))

nb_tp_tr <-sum(ifelse((nb_pred_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))
nb_tn_tr <-sum(ifelse((nb_pred_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nb_fp_tr <-sum(ifelse((nb_pred_tr =="YES") & (airbnb[tr_rows,]$perfect_rating_score=="NO"), 1, 0))
nb_fn_tr <-sum(ifelse((nb_pred_tr =="NO") & (airbnb[tr_rows,]$perfect_rating_score=="YES"), 1, 0))

TPR <-nb_tp/(nb_tp+nb_fn)
FPR<-nb_fp/(nb_fp+nb_tn)
print(nb_acc)
print(TPR)
print(FPR)

TPR_tr <-nb_tp_tr/(nb_tp_tr+nb_fn_tr)
FPR_tr<-nb_fp_tr/(nb_fp_tr+nb_tn_tr)
print(nb_acc_tr)
print(TPR_tr)
print(FPR_tr)







cost = c(1,2,3,4,5)

va_acc <- rep(0, length(cost))
tr_acc <- rep(0, length(cost))


num_cost<-length(cost)

for(i in c(1:num_cost)){

svm.mod <- svm(perfect_rating_score ~ amenities_n +price_per_person + availability_30  +
                 host_is_superhost +
                 + property_type ,
               data=tr_dtm, 
               kernel='linear', cost=1,cross=5,probability=TRUE)
summary(svm.mod)

svm.preds <- predict(svm.mod,va_dtm)
svm.preds_tr <- predict(svm.mod,tr_dtm)

nn_acc <- mean(ifelse(svm.preds==airbnb[-tr_rows,]$perfect_rating_score,1,0))
nn_acc_tr <- mean(ifelse(svm.preds==airbnb[tr_rows,]$perfect_rating_score,1,0))

print(cost[i])
tr_acc[i]<-nn_acc_tr
va_acc[i]<-nn_acc

}

plot(cost, tr_acc, type = "o", col = "blue", xlab = "cost", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.65, 1))
points(cost, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)




cross = c(2,3,4,5,6,7)

va_acc <- rep(0, length(cross))
tr_acc <- rep(0, length(cross))


num_cross<-length(cross)

for(i in c(1:num_cross)){
  
  svm.mod <- svm(perfect_rating_score ~ amenities_n +price_per_person + availability_30  +
                   host_is_superhost +
                   + property_type ,
                 data=tr_dtm, 
                 kernel='linear', cost=1,cross=cross[i],probability=TRUE)
  summary(svm.mod)
  
  svm.preds <- predict(svm.mod,va_dtm)
  svm.preds_tr <- predict(svm.mod,tr_dtm)
  
  nn_acc <- mean(ifelse(svm.preds==airbnb[-tr_rows,]$perfect_rating_score,1,0))
  nn_acc_tr <- mean(ifelse(svm.preds==airbnb[tr_rows,]$perfect_rating_score,1,0))
  
  print(cross[i])
  tr_acc[i]<-nn_acc_tr
  va_acc[i]<-nn_acc
  
}

plot(cross, tr_acc, type = "o", col = "blue", xlab = "cross", ylab = "Accuracy",
     main = "Fitting curve: Training and Validation Accuracies", ylim = c(0.65, 1))
points(cross, va_acc, type = "o", col = "red")
legend("bottomright", legend = c("Training", "Validation"), col = c("blue", "red"), pch = 1)



svm.mod <- svm(perfect_rating_score ~ amenities_n +price_per_person + availability_30  +
                 host_is_superhost +
                 + property_type ,
               data=tr_dtm, 
               kernel='linear', cost=1,cross=5,probability=TRUE)

svm.preds <- predict(svm.mod,va_dtm)
svm.preds_tr <- predict(svm.mod,tr_dtm)

nn_acc <- mean(ifelse(svm.preds==airbnb[-tr_rows,]$perfect_rating_score,1,0))
nn_acc_tr <- mean(ifelse(svm.preds==airbnb[tr_rows,]$perfect_rating_score,1,0))

print(nn_acc_tr)
print(nn_acc)


library(tidyverse)
library(mosaic)
library(FNN)
library(class)
library(foreach)
library(RCurl)
library(nnet)
library(dplyr)
library(knitr)


#Question 1
data(SaratogaHouses)
summary(SaratogaHouses)


#Baseline model
lm_small = lm(price ~ bedrooms + bathrooms + lotSize, data=SaratogaHouses)
coef(lm_small)

#Medium model in class
lm_medium = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
                 fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=SaratogaHouses)
coef(lm_medium)

#Hand-build model that outperforms medium model
lm_handbuild=lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + bedrooms + fireplaces*fireplaces + bathrooms + heating + fuel +sewer + waterfront + newConstruction + centralAir + bedrooms*bathrooms + rooms*bedrooms + rooms*bathrooms, data=SaratogaHouses)
coef(lm_handbuild)

# Split into training and testing sets
n = nrow(SaratogaHouses)
n_train = round(0.8*n)  # round to nearest integer
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
saratoga_train = SaratogaHouses[train_cases,]
saratoga_test = SaratogaHouses[test_cases,]

####
# Compare out-of-sample predictive performance
####


# Predictions out of sample
yhat_test_small = predict(lm_small, saratoga_test)
yhat_test_medium = predict(lm_medium, saratoga_test)
yhat_test_handbuild = predict(lm_handbuild, saratoga_test)

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

# Root mean-squared prediction error
rmse(saratoga_test$price, yhat_test_small)
rmse(saratoga_test$price, yhat_test_medium)
rmse(saratoga_test$price, yhat_test_handbuild)


# easy averaging over train/test splits
rmse_vals = do(100)*{
  
  # re-split into train and test cases
  n_train = round(0.8*n)  # round to nearest integer
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # fit to this training set
  lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
  lm2 = lm(price ~ lotSize + age + livingArea + pctCollege + bedrooms + 
             fireplaces + bathrooms + rooms + heating + fuel + centralAir, data=saratoga_train)
  lm3 = lm_handbuild=lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + bedrooms + fireplaces*fireplaces + bathrooms + rooms + heating + fuel +sewer + waterfront + newConstruction + centralAir + bedrooms*bathrooms + rooms*bedrooms + rooms*bathrooms, data=saratoga_train)
 
  # predict on this testing set
  yhat_test_small = predict(lm_small, saratoga_test)
  yhat_test_medium = predict(lm_medium, saratoga_test)
  yhat_test_handbuild = predict(lm_handbuild, saratoga_test)
  c(rmse(saratoga_test$price, yhat_test_small),
    rmse(saratoga_test$price, yhat_test_medium),
    rmse(saratoga_test$price, yhat_test_handbuild))
}

rmse_vals
colMeans(rmse_vals)

#KNN model
# construct the training and test-set feature matrices
# note the "-1": this says "don't add a column of ones for the intercept"
Xtrain = model.matrix(~ heating-1, data=saratoga_train)
Xtest = model.matrix(~ heating-1, data=saratoga_test)
Xtrain = model.matrix(~ fuel-1, data=saratoga_train)
Xtest = model.matrix(~ fuel-1, data=saratoga_test)
Xtrain = model.matrix(~ centralAir-1, data=saratoga_train)
Xtest = model.matrix(~ centralAir-1, data=saratoga_test)

# training and testing set responses
ytrain = saratoga_train$price
ytest = saratoga_test$price

# now rescale:
scale_train = apply(Xtrain, 2, sd)  # calculate std dev for each column
Xtilde_train = scale(Xtrain, scale = scale_train)
Xtilde_test = scale(Xtest, scale = scale_train)  # use the training set scales!


K_grid = seq(3, 100, by=2)

for(i in K_grid){
  knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=i)
  rmse_value = rmse(ytest, knn_model$pred)
  print(paste0("rmse is: ", rmse_value))
  print(paste0("k is:", i))
}

k_grid = seq(1,100,by=1) %>% round %>% unique
rmse_grid = foreach(K = k_grid, .combine='c') %do% {
  knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=K)
  rmse(ytest, knn_model$pred)
}

rmse_vals = do(100)*{
  
  # re-split into train and test cases
  n_train = round(0.8*n)  # round to nearest integer
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  saratoga_train = SaratogaHouses[train_cases,]
  saratoga_test = SaratogaHouses[test_cases,]
  
  # fit to this training set
  lm3 = lm_handbuild=lm(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms + heating + fuel + bedrooms + fireplaces*fireplaces + bathrooms + rooms + heating + fuel +sewer + waterfront + newConstruction + centralAir + bedrooms*bathrooms + rooms*bedrooms + rooms*bathrooms, data=saratoga_train)
  
  # predict on this testing set
  yhat_test_small = predict(lm_small, saratoga_test)
  yhat_test_medium = predict(lm_medium, saratoga_test)
  yhat_test_handbuild = predict(lm_handbuild, saratoga_test)
  c(rmse(saratoga_test$price, yhat_test))
}

which.min(rmse_grid)
k_grid[which.min(rmse_grid)]
rmse_grid[which.min(rmse_grid)]

plot(k_grid, rmse_grid, log='x',type="l",lty=1,lwd=2)




#Question 2
brca <- read.csv("~/Documents/GitHub/ECO395M/data/brca.csv")
summary(brca)


brca$cancer_hat = ifelse(brca$recall == 1, 1, 0)
brca$cancer_hat
count(brca)

threshold = sum(brca$recall == 1)/n+0.0001

# deviance function
dev_out = function(model, y, dataset) {
  probhat = predict(model, newdata=dataset, type='response')
  p0 = 1-probhat
  phat = data.frame(p0 = p0, p1 = probhat)
  rc_pairs = cbind(seq_along(y), y)
  -2*sum(log(phat[rc_pairs]))
}



#split into train and test cases
n = nrow(brca)
n_train = round(0.8*n)
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
brca_train = brca[train_cases,]
brca_test = brca[test_cases,]



# model 1
sum_dev_out_rate1 = 0
sum_overall_error_rate1 = 0
sum_false_negative_rate1 = 0
sum_false_positive_rate1 = 0

for (i in 1:50){
  n = nrow(brca)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  brca_train = brca[train_cases,]
  brca_test = brca[test_cases,]
  
  model_1= glm(recall~radiologist+age+history+symptoms+menopause+density, data=brca_train, family=binomial)
  coef(model_1) %>% round(3)
  
  phat_test_model_1 = predict(model_1, brca_test, type="response")
  brca_test$cancer_hat=ifelse(phat_test_model_1 > threshold, 1, 0)
  confusion_out_model_1 = table(y = brca_test$cancer, yhat = brca_test$cancer_hat)
  confusion_out_model_1
  
  dev_out_rate1 = dev_out(model_1, dataset = brca_test,y = brca_test$cancer)
  overall_error_rate1 = 1-sum(diag(confusion_out_model_1))/sum(confusion_out_model_1)
  false_negative_rate1 = confusion_out_model_1[2,1]/(confusion_out_model_1[2,1]+confusion_out_model_1[2,2]) 
  false_positive_rate1 = confusion_out_model_1[1,2] / (confusion_out_model_1[1,1]+confusion_out_model_1[1,2])
  
  sum_dev_out_rate1=sum_dev_out_rate1+dev_out_rate1
  sum_overall_error_rate1 = sum_overall_error_rate1+overall_error_rate1
  sum_false_negative_rate1 = sum_false_negative_rate1+false_negative_rate1
  sum_false_positive_rate1= sum_false_positive_rate1+false_positive_rate1
  
}

avg_dev_out_rate1=sum_dev_out_rate1/50
avg_overall_error_rate1 = sum_overall_error_rate1/50
avg_false_negative_rate1 = sum_false_negative_rate1/50
avg_false_positive_rate1 = sum_false_positive_rate1/50

print(avg_dev_out_rate1)
print(avg_overall_error_rate1)
print(avg_false_negative_rate1)
print(avg_false_positive_rate1)



# model 2
sum_dev_out_rate2 = 0
sum_overall_error_rate2 = 0
sum_false_negative_rate2 = 0
sum_false_positive_rate2 = 0

for (i in 1:50){
  n = nrow(brca)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  brca_train = brca[train_cases,]
  brca_test = brca[test_cases,]
  
  model_2= glm(recall~radiologist*(age+history+symptoms+menopause+density), data=brca_train, family=binomial)
  coef(model_2) %>% round(3)
  
  phat_test_model_2 = predict(model_2, brca_test, type="response")
  brca_test$cancer_hat=ifelse(phat_test_model_2 > threshold, 1, 0)
  confusion_out_model_2 = table(y = brca_test$cancer, yhat = brca_test$cancer_hat)
  confusion_out_model_2
  
  dev_out_rate2 = dev_out(model_2, dataset = brca_test,y = brca_test$cancer)
  overall_error_rate2 = 1-sum(diag(confusion_out_model_2))/sum(confusion_out_model_2)
  false_negative_rate2 = confusion_out_model_2[2,1]/(confusion_out_model_2[2,1]+confusion_out_model_2[2,2]) 
  false_positive_rate2 = confusion_out_model_2[1,2] / (confusion_out_model_2[1,1]+confusion_out_model_2[1,2])
  
  
  sum_dev_out_rate2=sum_dev_out_rate2+dev_out_rate2
  sum_overall_error_rate2 = sum_overall_error_rate2+overall_error_rate2
  sum_false_negative_rate2 = sum_false_negative_rate2+false_negative_rate2
  sum_false_positive_rate2= sum_false_positive_rate2+false_positive_rate2
  
}

avg_dev_out_rate2=sum_dev_out_rate2/50
avg_overall_error_rate2 = sum_overall_error_rate2/50
avg_false_negative_rate2 = sum_false_negative_rate2/50
avg_false_positive_rate2 = sum_false_positive_rate2/50

print(avg_dev_out_rate2)
print(avg_overall_error_rate2)
print(avg_false_negative_rate2)
print(avg_false_positive_rate2)




#Average performance of two models
avg_dev_out_rate=c(avg_dev_out_rate1, avg_dev_out_rate2)
avg_overall_error_rate=c(avg_overall_error_rate1, avg_overall_error_rate2)
avg_false_negative_rate=c(avg_false_negative_rate1, avg_false_negative_rate2)
avg_false_positive_rate=c(avg_false_positive_rate1, avg_false_positive_rate2)


performance_table=rbind(avg_dev_out_rate, avg_overall_error_rate, avg_false_negative_rate, avg_false_positive_rate)
colnames(performance_table) = c("Model 1", "Model 2")
rownames(performance_table) = c("Average Deviance Rate", "Average Overall Error Rate", "Average False Negative Rate", "Average False Positive Rate")
kable(performance_table, caption = "Average performance of Model 1 and Model 2", align = 'c')


#pick Model 1

model_1= glm(recall~radiologist+age+history+symptoms+menopause+density, data=brca_test, family=binomial)
coef(model_1) %>% round(3)

table = summary(model_1)
#kable(as.data.frame(table["coefficients"]), caption = "Recall Rate for Five Radiologists", padding = 2, align = "c")# generate the testing set
n = nrow(brca)
# get the sample
pretest_cases = sample.int(n,50,replace=FALSE)
brca_pretest = brca[pretest_cases,]
brca_sample=data.frame(brca_pretest)
# replicate the data for 5 times
brca_samplerepeat=brca_sample[rep(1:nrow(brca_sample),each=5),-1]
# assign the same patient to different doctors
brca_samplerepeat$radiologist=c("radiologist13","radiologist34","radiologist66","radiologist89","radiologist95")
# get diagnose from doctors 
yhat_recall = predict(model_1, brca_samplerepeat)
brca_samplerepeat=cbind(brca_samplerepeat,yhat_recall)
# compare doctors 
brca_predict<-brca_samplerepeat%>%
  group_by(radiologist)%>%
  summarise(Prob_recall = mean(yhat_recall))
kable(brca_predict)



#Average performance of two models
radiologist13=c(coefficients$radiologist13)
avg_overall_error_rate=c(avg_overall_error_rate1, avg_overall_error_rate2)
avg_false_negative_rate=c(avg_false_negative_rate1, avg_false_negative_rate2)
avg_false_positive_rate=c(avg_false_positive_rate1, avg_false_positive_rate2)


performance_table=rbind(radiologist13, radiologist34, radiologist66, radiologist89, radiologist95)
colnames(performance_table) = c("Recall Rate")
rownames(performance_table) = c("radiologist13", "radiologist34", "radiologist66", "radiologist89", "radiologist95")
kable(performance_table, caption = "Recall Rate for Five Radiologists", align = 'c')



#second part

rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}


rmse_vals = do(50)*{
  n = nrow(brca)
  n_train = round(0.8*n)  
  n_test = n - n_train
  
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  train_cases
  test_cases
  
  brca_train = brca[train_cases,]
  brca_test = brca[test_cases,]
  logit_A = glm(cancer ~ recall,data = brca_train, family=binomial)
  logit_B = glm(cancer ~ recall + history, data=brca_train, family=binomial)
  yhat_testA = predict(logit_A, brca_test)
  yhat_testB = predict(logit_B, brca_test)
  
  c(rmse(brca_test$cancer, yhat_testA),
    rmse(brca_test$cancer, yhat_testB))
  
}
rmse_vals
colMeans(rmse_vals)









#Question 3
online_news <- read.csv("~/Documents/GitHub/ECO395M/data/online_news.csv")
summary(online_news)



online_news$viral = ifelse(online_news$shares >1400, 1, 0)
online_news$viral
count(online_news)

online_news$null = 0
confusion_whole = table(y = online_news$viral, yhat = online_news$null)
confusion_whole
#overall error rate for null model
sum(19562/(20082+19562))


#split into train and test cases
n = nrow(online_news)
n_train = round(0.8*n)
n_test = n - n_train
train_cases = sample.int(n, n_train, replace=FALSE)
test_cases = setdiff(1:n, train_cases)
online_news_train = online_news[train_cases,]
online_news_test = online_news[test_cases,]

##regress first threshold second

# model 1
sum_overall_error_rate1 = 0
sum_true_positive_rate1 = 0
sum_false_positive_rate1 = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  lm1 = lm(shares ~ n_tokens_title + n_tokens_content + num_hrefs + 
             num_self_hrefs + num_imgs + num_videos + 
             average_token_length + num_keywords + data_channel_is_lifestyle + 
             data_channel_is_entertainment + data_channel_is_bus + 
             + data_channel_is_socmed + data_channel_is_tech + 
             data_channel_is_world + self_reference_avg_sharess + 
             weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
             weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  coef(lm1) %>% round(3)
  
  yhat_test_lm1 = predict(lm1, online_news_test)  # predicted shares
  online_news_test$viral_hat=ifelse(yhat_test_lm1 >1400, 1, 0)
  confusion_out_lm1 = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_lm1
  
  overall_error_rate1 = 1-sum(diag(confusion_out_lm1))/sum(confusion_out_lm1)
  true_positive_rate1 = confusion_out_lm1[2,2]/(confusion_out_lm1[2,1]+confusion_out_lm1[2,2]) 
  false_positive_rate1 = confusion_out_lm1[1,2] / (confusion_out_lm1[1,1]+confusion_out_lm1[1,2])
  
  sum_overall_error_rate1 = sum_overall_error_rate1+overall_error_rate1
  sum_true_positive_rate1 = sum_true_positive_rate1+true_positive_rate1
  sum_false_positive_rate1= sum_false_positive_rate1+false_positive_rate1
  
}

avg_overall_error_rate1 = sum_overall_error_rate1/50
avg_true_positive_rate1 = sum_true_positive_rate1/50
avg_false_positive_rate1 = sum_false_positive_rate1/50

print(avg_overall_error_rate1)
print(avg_true_positive_rate1)
print(avg_false_positive_rate1)

# model 2

sum_overall_error_rate2 = 0
sum_true_positive_rate2 = 0
sum_false_positive_rate2 = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  lm2 = lm(shares ~ (n_tokens_title + n_tokens_content + num_hrefs + 
                          num_self_hrefs + num_imgs + num_videos + 
                          average_token_length + num_keywords + data_channel_is_lifestyle + 
                          data_channel_is_entertainment + data_channel_is_bus + 
                          data_channel_is_socmed + data_channel_is_tech + 
                          data_channel_is_world + self_reference_avg_sharess + 
                          weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                          weekday_is_thursday + weekday_is_friday + weekday_is_saturday)^2, data=online_news_train)
  
  coef(lm2) %>% round(3)
  
  yhat_test_lm2 = predict(lm2, online_news_test)  # predicted shares
  online_news_test$viral_hat=ifelse(yhat_test_lm2 >1400, 1, 0)
  confusion_out_lm2 = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_lm2
  
  overall_error_rate2 = 1-sum(diag(confusion_out_lm2))/sum(confusion_out_lm2)
  true_positive_rate2 = confusion_out_lm2[2,2]/(confusion_out_lm2[2,1]+confusion_out_lm2[2,2]) 
  false_positive_rate2 = confusion_out_lm2[1,2] / (confusion_out_lm2[1,1]+confusion_out_lm2[1,2])
  
  sum_overall_error_rate2 = sum_overall_error_rate2+overall_error_rate2
  sum_true_positive_rate2 = sum_true_positive_rate2+true_positive_rate2
  sum_false_positive_rate2= sum_false_positive_rate2+false_positive_rate2
  
}

avg_overall_error_rate2 = sum_overall_error_rate2/50
avg_true_positive_rate2 = sum_true_positive_rate2/50
avg_false_positive_rate2 = sum_false_positive_rate2/50

print(avg_overall_error_rate2)
print(avg_true_positive_rate2)
print(avg_false_positive_rate2)


#model 3

sum_overall_error_rate3 = 0
sum_true_positive_rate3 = 0
sum_false_positive_rate3 = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  lm3 = lm(shares ~ poly(n_tokens_title, 3) + poly(num_hrefs, 2) + poly(num_imgs, 2) + poly(num_videos, 2) +
    poly(average_token_length, 3) + poly(num_keywords, 2) + poly(n_tokens_content, 2) +
    data_channel_is_lifestyle + data_channel_is_entertainment + data_channel_is_bus + 
    data_channel_is_socmed + data_channel_is_tech +
    data_channel_is_world + poly(self_reference_avg_sharess,2) + 
    weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
    weekday_is_thursday + weekday_is_friday + weekday_is_saturday + 
    poly(max_positive_polarity, 3) + poly(max_negative_polarity, 3), data=online_news_train)
  
  coef(lm3) %>% round(3)
  
  yhat_test_lm3 = predict(lm3, online_news_test)  # predicted shares
  online_news_test$viral_hat=ifelse(yhat_test_lm3 >1400, 1, 0)
  confusion_out_lm3 = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_lm3
  
  overall_error_rate3 = 1-sum(diag(confusion_out_lm3))/sum(confusion_out_lm3)
  true_positive_rate3 = confusion_out_lm3[2,2]/(confusion_out_lm3[2,1]+confusion_out_lm3[2,2]) 
  false_positive_rate3 = confusion_out_lm3[1,2] / (confusion_out_lm3[1,1]+confusion_out_lm3[1,2])
  
  sum_overall_error_rate3 = sum_overall_error_rate3+overall_error_rate3
  sum_true_positive_rate3 = sum_true_positive_rate3+true_positive_rate3
  sum_false_positive_rate3= sum_false_positive_rate3+false_positive_rate3
  
}

avg_overall_error_rate3 = sum_overall_error_rate3/50
avg_true_positive_rate3 = sum_true_positive_rate3/50
avg_false_positive_rate3 = sum_false_positive_rate3/50

print(avg_overall_error_rate3)
print(avg_true_positive_rate3)
print(avg_false_positive_rate3)


#model 4

sum_overall_error_rate4 = 0
sum_true_positive_rate4 = 0
sum_false_positive_rate4 = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  lm4 = lm(log(shares)~ n_tokens_title + n_tokens_content + num_hrefs + 
                 num_self_hrefs + num_imgs + num_videos + 
                 average_token_length + num_keywords + data_channel_is_lifestyle + 
                 data_channel_is_entertainment + data_channel_is_bus + 
                 + data_channel_is_socmed + data_channel_is_tech + 
                 data_channel_is_world + self_reference_avg_sharess + 
                 weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                 weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  coef(lm4) %>% round(3)
  
  yhat_test_lm4 = predict(lm4, online_news_test)  # predicted shares
  online_news_test$viral_hat=ifelse(yhat_test_lm4 >log(1400), 1, 0)
  confusion_out_lm4 = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_lm4
  
  overall_error_rate4 = 1-sum(diag(confusion_out_lm4))/sum(confusion_out_lm4)
  true_positive_rate4 = confusion_out_lm4[2,2]/(confusion_out_lm4[2,1]+confusion_out_lm4[2,2]) 
  false_positive_rate4 = confusion_out_lm4[1,2] / (confusion_out_lm4[1,1]+confusion_out_lm4[1,2])
  
  sum_overall_error_rate4 = sum_overall_error_rate4+overall_error_rate4
  sum_true_positive_rate4 = sum_true_positive_rate4+true_positive_rate4
  sum_false_positive_rate4= sum_false_positive_rate4+false_positive_rate4
  
}

avg_overall_error_rate4 = sum_overall_error_rate4/50
avg_true_positive_rate4 = sum_true_positive_rate4/50
avg_false_positive_rate4 = sum_false_positive_rate4/50

print(avg_overall_error_rate4)
print(avg_true_positive_rate4)
print(avg_false_positive_rate4)





#Average performance of different models
avg_overall_error_rate=c(avg_overall_error_rate1, avg_overall_error_rate2, avg_overall_error_rate3, avg_overall_error_rate4)
avg_true_positive_rate=c(avg_true_positive_rate1, avg_true_positive_rate2, avg_true_positive_rate3, avg_true_positive_rate4)
avg_false_positive_rate=c(avg_false_positive_rate1, avg_false_positive_rate2, avg_false_positive_rate3, avg_false_positive_rate4)


performance_table=rbind(avg_overall_error_rate, avg_true_positive_rate, avg_false_positive_rate)
colnames(performance_table) = c("Model 1", "Model 2", "Model 3", "Model 4")
rownames(performance_table) = c("Average Overall Error Rate", "Average True Positive rate", "Average False Positive Rate")
kable(performance_table, caption = "Average performance of different models", align = 'c')


##threshold first regress second

#model A

sum_overall_error_rateA = 0
sum_true_positive_rateA = 0
sum_false_positive_rateA = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  lmA = lm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
                num_self_hrefs + num_imgs + num_videos + 
                average_token_length + num_keywords + data_channel_is_lifestyle + 
                data_channel_is_entertainment + data_channel_is_bus + 
                + data_channel_is_socmed + data_channel_is_tech + 
                data_channel_is_world + self_reference_avg_sharess + 
                weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  coef(lmA) %>% round(3)
  
  phat_test_lmA = predict(lmA, online_news_test)  # predicted probabilities
  online_news_test$viral_hat=ifelse(phat_test_lmA >0.5, 1, 0)
  confusion_out_lmA = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_lmA
  
  overall_error_rateA = 1-sum(diag(confusion_out_lmA))/sum(confusion_out_lmA)
  true_positive_rateA = confusion_out_lmA[2,2]/(confusion_out_lmA[2,1]+confusion_out_lmA[2,2]) 
  false_positive_rateA = confusion_out_lmA[1,2] / (confusion_out_lmA[1,1]+confusion_out_lmA[1,2])
  
  sum_overall_error_rateA = sum_overall_error_rateA+overall_error_rateA
  sum_true_positive_rateA = sum_true_positive_rateA+true_positive_rateA
  sum_false_positive_rateA= sum_false_positive_rateA+false_positive_rateA
  
}

avg_overall_error_rateA = sum_overall_error_rateA/50
avg_true_positive_rateA = sum_true_positive_rateA/50
avg_false_positive_rateA = sum_false_positive_rateA/50

print(avg_overall_error_rateA)
print(avg_true_positive_rateA)
print(avg_false_positive_rateA)


#model B

sum_overall_error_rateB = 0
sum_true_positive_rateB = 0
sum_false_positive_rateB = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  glmB = glm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
              num_self_hrefs + num_imgs + num_videos + 
              average_token_length + num_keywords + data_channel_is_lifestyle + 
              data_channel_is_entertainment + data_channel_is_bus + 
              data_channel_is_socmed + data_channel_is_tech + 
              data_channel_is_world + self_reference_avg_sharess + 
              weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
              weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  coef(glmB) %>% round(3)
  
  phat_test_glmB = predict(glmB, online_news_test)  # predicted probabilities
  online_news_test$viral_hat=ifelse(phat_test_glmB >0.5, 1, 0)
  confusion_out_glmB = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_glmB
  
  overall_error_rateB = 1-sum(diag(confusion_out_glmB))/sum(confusion_out_glmB)
  true_positive_rateB = confusion_out_glmB[2,2]/(confusion_out_glmB[2,1]+confusion_out_glmB[2,2]) 
  false_positive_rateB = confusion_out_glmB[1,2] / (confusion_out_glmB[1,1]+confusion_out_glmB[1,2])
  
  sum_overall_error_rateB = sum_overall_error_rateB+overall_error_rateB
  sum_true_positive_rateB = sum_true_positive_rateB+true_positive_rateB
  sum_false_positive_rateB= sum_false_positive_rateB+false_positive_rateB
  
}

avg_overall_error_rateB = sum_overall_error_rateB/50
avg_true_positive_rateB = sum_true_positive_rateB/50
avg_false_positive_rateB = sum_false_positive_rateB/50

print(avg_overall_error_rateB)
print(avg_true_positive_rateB)
print(avg_false_positive_rateB)



#model C

# construct the training and test-set feature matrices
# note the "-1": this says "don't add a column of ones for the intercept"
Xtrain = model.matrix(~ n_tokens_title + n_tokens_content + num_hrefs + 
                        num_self_hrefs + num_imgs + num_videos + 
                        average_token_length + num_keywords + data_channel_is_lifestyle + 
                        data_channel_is_entertainment + data_channel_is_bus + 
                        + data_channel_is_socmed + data_channel_is_tech + 
                        data_channel_is_world + self_reference_avg_sharess + 
                        weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                        weekday_is_thursday + weekday_is_friday + weekday_is_saturday - 1, data=online_news_train)
Xtest = model.matrix(~ n_tokens_title + n_tokens_content + num_hrefs + 
                       num_self_hrefs + num_imgs + num_videos + 
                       average_token_length + num_keywords + data_channel_is_lifestyle + 
                       data_channel_is_entertainment + data_channel_is_bus + 
                       + data_channel_is_socmed + data_channel_is_tech + 
                       data_channel_is_world + self_reference_avg_sharess + 
                       weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                       weekday_is_thursday + weekday_is_friday + weekday_is_saturday - 1, data=online_news_test)


# training and testing set responses
ytrain = online_news_train$viral
ytest = online_news_test$viral

# now rescale:
scale_train = apply(Xtrain, 2, sd)  # calculate std dev for each column
Xtilde_train = scale(Xtrain, scale = scale_train)
Xtilde_test = scale(Xtest, scale = scale_train)  # use the training set scales!


rmse = function(y, yhat) {
  sqrt( mean( (y - yhat)^2 ) )
}

K_grid = seq(3, 100, by=2)

for(i in K_grid){
  knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=i)
  rmse_value = rmse(ytest, knn_model$pred)
  print(paste0("rmse is: ", rmse_value))
  print(paste0("k is:", i))
}

k_grid = seq(1,100,by=1) %>% round %>% unique
rmse_grid = foreach(K = k_grid, .combine='c') %do% {
  knn_model = knn.reg(Xtilde_train, Xtilde_test, ytrain, k=K)
  rmse(ytest, knn_model$pred)
}

rmse_vals = do(100)*{
  
  # re-split into train and test cases
  n_train = round(0.8*n)  # round to nearest integer
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  # fit to this training set
  KNN_lmA=lm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
                     num_self_hrefs + num_imgs + num_videos + 
                     average_token_length + num_keywords + data_channel_is_lifestyle + 
                     data_channel_is_entertainment + data_channel_is_bus + 
                     + data_channel_is_socmed + data_channel_is_tech + 
                     data_channel_is_world + self_reference_avg_sharess + 
                     weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                     weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  
  KNN_glmB = glm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
                num_self_hrefs + num_imgs + num_videos + 
                average_token_length + num_keywords + data_channel_is_lifestyle + 
                data_channel_is_entertainment + data_channel_is_bus + 
                data_channel_is_socmed + data_channel_is_tech + 
                data_channel_is_world + self_reference_avg_sharess + 
                weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)
  
  # predict on this testing set
  yhat_test_lmA = predict(lmA, online_news_test)
  yhat_test_glmB = predict(glmB, online_news_test)
  c(rmse(online_news_test$viral, online_news_test))
}

which.min(rmse_grid)
k_grid[which.min(rmse_grid)]
rmse_grid[which.min(rmse_grid)]

plot(k_grid, rmse_grid, log='x',type="l",lty=1,lwd=2)


#try K optimal model

sum_overall_error_rateC = 0
sum_true_positive_rateC = 0
sum_false_positive_rateC = 0

for (i in 1:50){
  n = nrow(online_news)
  n_train = round(0.8*n)
  n_test = n - n_train
  train_cases = sample.int(n, n_train, replace=FALSE)
  test_cases = setdiff(1:n, train_cases)
  online_news_train = online_news[train_cases,]
  online_news_test = online_news[test_cases,]
  
  KNN_c = glm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
                   num_self_hrefs + num_imgs + num_videos + 
                   average_token_length + num_keywords + data_channel_is_lifestyle + 
                   data_channel_is_entertainment + data_channel_is_bus + 
                   data_channel_is_socmed + data_channel_is_tech + 
                   data_channel_is_world + self_reference_avg_sharess + 
                   weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
                   weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train, family=binomial)
  
  coef(KNN_c) %>% round(3)
  
  phat_test_KNN_c = predict(KNN_c, online_news_test)  # predicted probabilities
  online_news_test$viral_hat=ifelse(phat_test_KNN_c >0.5, 1, 0)
  confusion_out_KNN_c = table(y = online_news_test$viral, yhat = online_news_test$viral_hat)
  confusion_out_KNN_c
  
  overall_error_rateC = 1-sum(diag(confusion_out_KNN_c))/sum(confusion_out_KNN_c)
  true_positive_rateC = confusion_out_KNN_c[2,2]/(confusion_out_KNN_c[2,1]+confusion_out_KNN_c[2,2]) 
  false_positive_rateC = confusion_out_KNN_c[1,2] / (confusion_out_KNN_c[1,1]+confusion_out_KNN_c[1,2])
  
  sum_overall_error_rateC = sum_overall_error_rateC+overall_error_rateC
  sum_true_positive_rateC = sum_true_positive_rateC+true_positive_rateC
  sum_false_positive_rateC= sum_false_positive_rateC+false_positive_rateC
  
}

avg_overall_error_rateC = sum_overall_error_rateC/50
avg_true_positive_rateC = sum_true_positive_rateC/50
avg_false_positive_rateC = sum_false_positive_rateC/50

print(avg_overall_error_rateC)
print(avg_true_positive_rateC)
print(avg_false_positive_rateC)


#Average performance of Model A, Model B, Model C
avg_overall_error_rate=c(avg_overall_error_rateA, avg_overall_error_rateB, avg_overall_error_rateC)
avg_true_positive_rate=c(avg_true_positive_rateA, avg_true_positive_rateB, avg_true_positive_rateC)
avg_false_positive_rate=c(avg_false_positive_rateA, avg_false_positive_rateB, avg_false_positive_rateC)


performance_table=rbind(avg_overall_error_rate, avg_true_positive_rate, avg_false_positive_rate)
colnames(performance_table) = c("Model A", "Model B", "Model C")
rownames(performance_table) = c("Average Overall Error Rate", "Average True Positive rate", "Average False Positive Rate")
kable(performance_table, caption = "Average performance of different models", align = 'c')


#give suggestions

glmB = glm(viral ~ n_tokens_title + n_tokens_content + num_hrefs + 
             num_self_hrefs + num_imgs + num_videos + 
             average_token_length + num_keywords + data_channel_is_lifestyle + 
             data_channel_is_entertainment + data_channel_is_bus + 
             data_channel_is_socmed + data_channel_is_tech + 
             data_channel_is_world + self_reference_avg_sharess + 
             weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + 
             weekday_is_thursday + weekday_is_friday + weekday_is_saturday, data=online_news_train)

coef(glmB) %>% round(3)
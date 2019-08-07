data = read.csv(choose.files(),header = T) # Choose final data set file

############
#Question 1#
############

#Identify the categorical and numerical variables that you will use in your model




############
#Question 2#
############

#2.	Think of an equation to use that can predict the total favorite count.

favorite_count = data$favorite_count

summary(favorite_count)

hist(favorite_count, freq = F, breaks = seq(1,4518186,0.01), xlim = c(0,4518186), ylim = c(0,0.000002))



############
#Question 3#
############

#3.	Think of an equation to use that can predict the total retweet count.

retweet = data$retweet_count
summary(retweet)

plot(retweet)
hist(retweet)



############
#Question 4#
############

#4.	Run various regressions, using different variables, to find a "good model", that can explain the data better than other models (perhaps even better than the initial equation you suggested).

install.packages("lmtest")
library(lmtest)


# REGRESSIONS FOR FAVORITE_COUNT
colnames(data)
x = data$X
Jockers = data$Jockers
McDonald = data$McDonald
Sentiword = data$Sentiword          
Senticnet = data$Senticnet 
source = data$source            
display_text_width = data$display_text_width
favorite_count = data$favorite_count    
retweet_count = data$retweet_count              
media_type = data$media_type
followers = data$followers_count 
friends = data$friends_count
listed_count = data$listed_count     
 statuses_count = data$statuses_count
 favourites_count = data$favourites_count
 verfified = data$verified
 month = as.factor(data$created_month)
 day = as.factor(data$created_day)
 hour = as.factor(data$created_hour)
 account_age = data$account_age
 hashtag_count = data$hashtag_count
 hashtag_sentiment = data$hashtag_sentiment
 

fav_test_1= lm(favorite_count~followers_count)
summary(fav_test_1) # verylow p value. Not much here. 

fav_test_2 = lm(favorite_count~followers_count+
                   retweet_count+Jockers+McDonald+Sentiword)
summary(fav_test_2)

fav_test_3 = lm(favorite_count~retweet_count+
                  followers_count+
                  display_text_width+
                  Jockers+
                  McDonald+
                  Senticnet+Sentiword)
summary(fav_test_3)


fav_test_4 =lm(favorite_count~retweet_count+
                 month+day+McDonald+Senticnet+Sentiword)
summary(fav_test_4)


fav_test_5 = lm(favorite_count~retweet_count+
                 friends_count+
                  McDonald+Sentiword+
                  Senticnet+Jockers)
summary(fav_test_5)

fav_test_6 = lm(favorite_count~day+month+hour)
summary(fav_test_6)


fav_test_7 = lm(favorite_count ~ day+month+hour
                + Jockers + McDonald+
                  Senticnet+Sentiword)

summary(fav_test_7)

## Test to find out which of the 7 favorite count linear regression is the best#
lrtest(fav_test_1,fav_test_2,fav_test_3,fav_test_4,fav_test_5,fav_test_6,fav_test_7)

# fav_test_2 and fav_test5 hold interesting results. 
#fav_test_2 is the best model to use seing how the LogLik is much closer towards 0

# REGRESSION FOR RETWEET COUNT 

retweet_test_1 = lm(retweet_count~followers_count+favourites_count+
                      McDonald+Jockers+Senticnet+
                      Sentiword)

summary(retweet_test_1)

retweet_test_2 = lm(retweet_count~followers_count+
                      day+month)
summary(retweet_test_2)

retweet_test_3<-lm(retweet_count~day+month+
                   McDonald+ Jockers+
                     Senticnet+Sentiword)

summary(retweet_test_3)


retweet_test_4 = lm(retweet_count~followers_count+
                      display_text_width+
                      month+day+Senticnet+
                      Jockers+McDonald+Sentiword)


summary(retweet_test_4 )

retweet_test_5 = lm(retweet_count~followers_count+
                      display_text_width+month+
                      McDonald)
summary(retweet_test_5)

retweet_test_6 = lm(retweet_count~followers_count+month+
                      display_text_width+hashtag_count+
                      friends_count+Jockers+McDonald+
                      Senticnet+Sentiword)
summary(retweet_test_6)

retweet_test_7 = lm (retweet_count~listed_count+followers_count+
                       friends_count+statuses_count+
                       month+Jockers+
                       McDonald+Sentiword+Senticnet)
summary(retweet_test_7)

# LINEAR REGRESSION COMPARRISON for RETWEET
lrtest(retweet_test_1,retweet_test_2,
       retweet_test_3,retweet_test_4,
       retweet_test_5,retweet_test_6,
       retweet_test_7)
#Retweet test 4 has the smallest LogLik value, byt Retweet_Test_6 has a decent PR(>Chisq)



############
#Question5#
############

#5. Once you have found a "good" model that you fit, test for heteroscadasticity. If it is present,
#then fix it.

# FAVORITE_TEST HETEROSCADASTICITY TEST
bptest(fav_test_1) 
bptest(fav_test_2)
bptest(fav_test_3)
bptest(fav_test_4)
bptest(fav_test_5)
bptest(fav_test_6)
bptest(fav_test_7)

#Heteroscadasticity is found in all of my favourite count test

#Retweet COUNT HETEROSCADASTICITY TEST
bptest(retweet_test_1)
bptest(retweet_test_2)
bptest(retweet_test_3)
bptest(retweet_test_4)
bptest(retweet_test_5)
bptest(retweet_test_6)
bptest(retweet_test_7)

#Heteroscadasticity is found in all of my retweet count test

#If we leverage
#the sandwich package

install.packages("sandwich")
library(sandwich)


coeftest(fav_test_1,vcov=vcovHC(fav_test_1,"HC1"))
summary(fav_test_1)

coeftest(fav_test_2,vcov=vcovHC(fav_test_2,"HC1"))
summary(fav_test_2)

coeftest(fav_test_3,vcov=vcovHC(fav_test_3,"HC1"))
summary(fav_test_3)

coeftest(fav_test_4,vcov=vcovHC(fav_test_4,"HC1"))
summary(fav_test_4)

coeftest(fav_test_5,vcov=vcovHC(fav_test_5,"HC1"))
summary(fav_test_5)

coeftest(fav_test_6,vcov=vcovHC(fav_test_6,"HC1"))
summary(fav_test_6)

coeftest(fav_test_7,vcov=vcovHC(fav_test_7,"HC1"))
summary(fav_test_7)


coeftest(retweet_test_1,vcov=vcovHC(retweet_test_1,"HC1"))
summary(retweet_test_1)

coeftest(retweet_test_2,vcov=vcovHC(retweet_test_2,"HC1"))
summary(retweet_test_2)

coeftest(retweet_test_3,vcov=vcovHC(retweet_test_3,"HC1"))
summary(retweet_test_3)

coeftest(retweet_test_4,vcov=vcovHC(retweet_test_4,"HC1"))
summary(retweet_test_4)

coeftest(retweet_test_5,vcov=vcovHC(retweet_test_5,"HC1"))
summary(retweet_test_5)

coeftest(retweet_test_6,vcov=vcovHC(retweet_test_6,"HC1"))
summary(retweet_test_6)

coeftest(retweet_test_7,vcov=vcovHC(retweet_test_7,"HC1"))
summary(retweet_test_7)


############
#Question 6#
############

#6. Interpret the results for each equation (one equation for favorites, one for retweets).


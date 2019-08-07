data = read.csv(file.choose(), header=T) # select ""C:/Users/JCM/Desktop/movie_data_4-7_HIT RATIO TEST_ON CHERILYNS_REGRESSION_BY_JEAN_MERCEDES.csv"
colnames(data)
par(mfrow = c(1,2))
hist(data$MODEL_PREDICTION, 
     freq = F, 
     xlim = c(10^6,10^9), 
     ylim = c(0,10^-8.25),
     col = "Blue",
     breaks = 100)

hist(data$ACTUAL.REVENUE., 
     freq = F, 
     xlim = c(10^6,10^9), 
     ylim = c(0,10^-8.25),
     col = "Blue",
     breaks = 200)

chisq.test(data$MODEL_PREDICTION,data$ACTUAL.REVENUE.)
# Our Model has a pvalue of .2448



########################
## BOOTSTRAPPING #######
########################

install.packages("boot")
library(boot)

data_bs = read.csv(file.choose(),header= T) # choose "hit_miss_ratio_dist_test_Mercedes"

model_predecitionbs = data_bs$Model.Prediction
runtime_bs= data_bs$runtime
popularity_bs =data_bs$popularity
budget_bs =data_bs$budget
adventure_bs =data_bs$id.Adventure
animation_bs =data_bs$id.Animation
war_bs=data_bs$id.War
crime_bs=data_bs$id.Crime
drama_bs = data_bs$id.Drama
history_bs = data_bs$id.History
revenue_bs = data_bs$Actual.Revenue

# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=data_bs, statistic=rsq, 
                R=1000, formula=model_predecitionbs~budget_bs+popularity_bs
                +history_bs+drama_bs+crime_bs+war_bs+animation_bs+
                  adventure_bs+runtime_bs)

# view results
summary(results)
results$seed

head(results)
plot(results)

# get 95% confidence interval 
boot.ci(results, type="norm")

# Load packages
set.seed(123)
library(randomForest)
library(ggplot2)
library(tree)
library(gbm)
library(tidyverse)
library(class)
rm(list = ls())

# Load the data
airline = read.csv('UNION.csv')
str(airline)
# Clean the data
airline_clean = airline %>%
  select(-c('F1','id','Table.Names')) %>%
  mutate(satisfaction = ifelse (satisfaction=='satisfied',1,0)) %>%
  na.omit() 

airline_clean$satisfaction = as.factor(airline_clean$satisfaction)

sum(is.na(airline_clean))

attach(airline_clean)

# Split the dataset
train = sample(1:nrow(airline_clean),nrow(airline_clean)*0.8)
airline_test = airline_clean[-train,]
airline_train = airline_clean[train,]
sat_test = airline_clean$satisfaction[-train]

# Logistic regression
log_reg1 = glm(satisfaction ~ .,data = airline_train, family = binomial)
probs1 = predict(log_reg1,newdata = airline_test, type = 'response')
test_pred1 = rep(0,25898)
test_pred1[probs1 > 0.5] = 1
table(test_pred1, sat_test)
mean(test_pred1 != sat_test)

summary(log_reg1)

# exclude insignificant variables
log_reg2 = glm(satisfaction ~.-Flight.Distance,data = airline_train, family = binomial)
probs2 = predict(log_reg2,newdata = airline_test, type = 'response')
test_pred2 = rep(0,25898)
test_pred2[probs2 > 0.5] = 1
table(test_pred2, sat_test)
mean(test_pred2 != sat_test)

# LDA,QDA 

################################# LDA model:

lda_fit01 = lda(satisfaction~., data=airline_train)
lda_fit01_pred = predict(lda_fit01, airline_test)
#Calculate the error rate of the model:
mean(lda_fit01_pred$class != sat_test) 

############################### QDA model:
qda.fit = qda(satisfaction ~., data=airline_train)
qda.fit
# Use QDA to predict:
qda.pred = predict(qda.fit, airline_test)
qda.pred

#Calculate the error rate the model obtain:
mean(qda.pred$class != sat_test)

# Bagging & Randomforest
#bagging
bag.train <- randomForest(satisfaction~.,data = airline_train,importance = T)
pred.bag <- predict(bag.train,newdata = airline_test)
table(pred.bag,sat_test)

#test error rate
(280+661)/(14353+661+280+10604)
#0.03633485

importance(bag.train)
varImpPlot(bag.train)


#randomForest
rf.train <- randomForest(satisfaction~.,data = airline_train,mtry = 5,importance = T)
pred.rf <- predict(rf.train,newdata = airline_test)
table(pred.rf,sat_test)

#test error rate
(271+629)/(14362+629+271+10636)
#0.03475172

importance(rf.train)
varImpPlot(rf.train)

# Decision tree model & Boosting
# Fit a decision tree model
tree.airline = tree(satisfaction~., data = airline_clean,subset = train)
plot(tree.airline)
head(tree.airline)
text(tree.airline,pretty = 2)
summary(tree.airline)

# Prediciton
pred.sat = predict(tree.airline,airline_test,type = 'class')
table(pred.sat,sat_test)
(589+2078)/(12555+589+2078+10676)

# Corss-validation
cv.airline = cv.tree(tree.airline, FUN = prune.tree)
plot(cv.airline$size, cv.airline$dev, type = "b")

best_tree = cv.airline$size[which.min(cv.airline$dev)]
best_tree

# Boosting
# Fit model
boost.airline =gbm(satisfaction ~., data= airline_train, distribution= "bernoulli",n.trees =5000 , interaction.depth =4)
summary(boost.airline)

# Plot
par(mfrow = c(1,2))
plot(boost.sat,i = 'Online.boarding')
plot(boost.sat,i = 'Inflight.wifi.service')

# Prediction
boost.prob = predict(boost.airline, airline_test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.5, 1, 0)
table(boost.pred,sat_test)




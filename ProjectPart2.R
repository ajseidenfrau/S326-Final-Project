#Problem 1: Regression

home_data <- read.csv("BestHomePart2.csv", header = T, as.is = T)

home_data <- subset(home_data,select = -c(ProductID, Brand))



### Part 1 ###
train_size <- 0.7*nrow(home_data)
set.seed(100)
train_index <- sample(x = 1:nrow(home_data), size = train_size, replace = F)
train_set <- home_data[train_index, ]
valid_set <- home_data[-train_index, ]



str(home_data)
cor(home_data[ ,-c(1, 2, 3)]) #removing character data columns
unique(home_data$Class)
unique(home_data$Assembly_status)



### Part 2 ###

#Linear Regression Models
M1 <- lm(formula = WeeklyDemand ~ Purchasing_cost + Selling_price + WeeklyDiscount, data = train_set)
summary(M1)

M2 <- lm(formula = WeeklyDemand ~ Class + Assembly_status, data = train_set)
summary(M2)


#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)

#Regression Trees
reg_tree <- rpart(formula = WeeklyDemand ~ Purchasing_cost + Selling_price 
                  + WeeklyDiscount, data = train_set, method = "anova")
prp(reg_tree, type = 1, extra = 1)

reg_tree2 <- rpart(formula = WeeklyDemand ~ Class + Assembly_status, data = train_set,
                   method = "anova")
prp(reg_tree2, type = 1, extra = 1)


# Predicting values for linear regression models
predM1_valid <- predict(M1, newdata = valid_set)
predM1_train <-predict(M1, newdata = train_set)

predM2_valid <- predict(M2, newdata = valid_set)
predM2_train <- predict(M2, newdata = train_set)

# Predicting values for regression trees
pred_tree_1_valid <- predict(reg_tree, newdata = valid_set)
pred_tree_1_train <- predict(reg_tree, newdata = train_set)

pred_tree_2_valid <- predict(reg_tree2, newdata = valid_set)
pred_tree_2_train <- predict(reg_tree2, newdata = train_set)



#install.packages('forecast')
library(forecast)

##Accuracy for Linear Regression Models
accuracy(predM1_valid, valid_set$WeeklyDemand)
accuracy(predM1_train, train_set$WeeklyDemand)

accuracy(predM2_valid, valid_set$WeeklyDemand)
accuracy(predM2_train, train_set$WeeklyDemand)

##Accuracy for Regression Trees
accuracy(pred_tree_1_valid, valid_set$WeeklyDemand)
accuracy(pred_tree_1_train, train_set$WeeklyDemand)


##Accuracy for Regression Tree 2
accuracy(pred_tree_2_valid, valid_set$WeeklyDemand)
accuracy(pred_tree_2_train, train_set$WeeklyDemand)


#==============================================================================

# Problem 2: Classification

### Part 1 ###
home_data$DemandLevel <- ifelse(home_data$WeeklyDemand >= quantile(home_data$WeeklyDemand, .75),
                                1, 0)

train_size <- 0.7*nrow(home_data)
set.seed(100)
train_index <- sample(x = 1:nrow(home_data), size = train_size, replace = F)
train_set <- home_data[train_index, ]
valid_set <- home_data[-train_index, ]


### Part 2 ###

#Logistic Regression Models
M3 <- glm(formula = DemandLevel ~  Selling_price + WeeklyDiscount,
          data = train_set, family = "binomial")
summary(M3)

M4 <- glm(formula = DemandLevel ~ Required_capacity + Purchasing_cost, data = train_set,
          family = "binomial")
summary(M4)

# Classification Trees
cl_tree <- rpart(formula = DemandLevel ~  Selling_price + WeeklyDiscount,
                 data = train_set, method = "class")
prp(cl_tree, type = 1, extra = 1)

cl_tree2 <- rpart(formula = DemandLevel ~ Required_capacity + Purchasing_cost, 
                  data = train_set, method = "class")
prp(cl_tree2, type = 1, extra = 1)

# Predicting logistic regression models
predM3_valid <- predict(M3, newdata = valid_set, type = "response")
predM3_train <- predict(M3, newdata = train_set, type = "response")

predM4_valid <- predict(M4, newdata = valid_set, type = "response")
predM4_train <- predict(M4, newdata = train_set, type = "response")

cutoff <- 0.5
predM3_valid_prob <- ifelse(predM3_valid > cutoff, 1, 0)
predM3_train_prob <- ifelse(predM3_train > cutoff, 1, 0)

predM4_valid_prob <- ifelse(predM4_valid > cutoff, 1, 0)
predM4_train_prob <- ifelse(predM4_train > cutoff, 1, 0)

# Predicting classification trees
pred_cltree_valid <-predict(cl_tree, newdata = valid_set, type = "class")
pred_cltree_train <-predict(cl_tree, newdata = train_set, type = "class")

pred_cltree2_valid <- predict(cl_tree2, newdata = valid_set, type = "class")
pred_cltree2_train <- predict(cl_tree2, newdata = train_set, type = "class")

#install.packages('caret')
library(caret)

# Confusion matrix for linear regression models
confusionMatrix(as.factor(predM3_valid_prob),
                as.factor(valid_set$DemandLevel),
                positive = "1")
#Accuracy: 0.9405
#Sensitivity : 0.9623          
#Specificity : 0.8705 

confusionMatrix(as.factor(predM3_train_prob),
                as.factor(train_set$DemandLevel),
                positive = "1")

# Accuracy : 0.9198
# Sensitivity : 0.8511          
# Specificity : 0.9436 

confusionMatrix(as.factor(predM4_valid_prob),
                as.factor(valid_set$DemandLevel),
                positive = "1")
#Accuracy : 0.983
#Sensitivity : 0.9955          
#Specificity : 0.9791 

confusionMatrix(as.factor(predM4_train_prob),
                as.factor(train_set$DemandLevel),
                positive = "1")
#Accuracy : 0.9736
#Sensitivity : 0.9911         
#Specificity : 0.9675


# Confusion matrix for classification trees
confusionMatrix(as.factor(pred_cltree_valid),
                as.factor(valid_set$DemandLevel),
                positive = "1")
#Accuracy : 0.9479 
#Sensitivity : 0.9643          
#Specificity : 0.9428  

confusionMatrix(as.factor(pred_cltree_train),
                as.factor(train_set$DemandLevel),
                positive = "1")
#Accuracy : 0.9394
#Sensitivity : 0.9734         
#Specificity : 0.9277

confusionMatrix(as.factor(pred_cltree2_valid),
                as.factor(valid_set$DemandLevel),
                positive = "1")
#Accuracy : 0.983
#Sensitivity : 0.9955          
#Specificity : 0.9791


confusionMatrix(as.factor(pred_cltree2_train),
                as.factor(train_set$DemandLevel),
                positive = "1")
#Accuracy : 0.9736
#Sensitivity : 0.9911         
#Specificity : 0.9675 




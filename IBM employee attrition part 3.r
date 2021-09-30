### part 3 preditions

# create data partition, confustionmatrix
install.packages("caret", dependencies = TRUE)  # must have dependencies parameter, otherwise, "Error: package e1071 is required" 
install.packages("ROCR") # visualizing classifier performance 
install.packages("ROSE") # random over sampling examples
install.packages("rpart") # Recursive Partitioning and Regression Trees
install.packages("rpart.plot") # plots rpart model

library(caret)
library(ROCR)
library(ROSE)
library(rpart)
library(rpart.plot)

########## 1. Logistic regression ##########

# 1.1 fitting model
# setting seed for reproducibility
set.seed(56789)

train <- df$Attrition %>%
    createDataPartition(p = 0.8, list = FALSE)
df_train <- df[train,]
df_test <- df[-train,]

df_train %>%
    filter(Attrition == "No") %>%
    summarize(p_no = n()/count(df_train)) 
# 0.839

df_test %>%
    filter(Attrition == "No") %>%
    summarize(p_no = n()/count(df_test)) 
# 0.840


model_3 <- glm(Attrition ~ age_bin + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_train, family = "binomial")
model_3_stepwise <- stepAIC(model_3, direction = "both", trace = FALSE)
summary(model_3_stepwise)

# Call:
# glm(formula = Attrition ~ age_bin + BusinessTravel + DistanceFromHome + 
#     EnvironmentSatisfaction + JobInvolvement + JobLevel + JobRole + 
#     JobSatisfaction + MaritalStatus + OverTime + WorkLifeBalance, 
#     family = "binomial", data = df_train)

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.1663  -0.5115  -0.2938  -0.1162   3.2143  

# Coefficients:
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      0.54966    1.03677   0.530 0.595994    
# age_bin[25,35)                  -0.88294    0.32900  -2.684 0.007281 ** 
# age_bin[35,45)                  -1.48251    0.35681  -4.155 3.25e-05 ***
# age_bin[45,55)                  -1.13896    0.42690  -2.668 0.007631 ** 
# age_bin[55,Inf)                 -0.57111    0.52415  -1.090 0.275894    
# BusinessTravelTravel_Frequently  1.55197    0.44784   3.465 0.000529 ***
# BusinessTravelTravel_Rarely      0.86033    0.41839   2.056 0.039755 *  
# DistanceFromHome                 0.03311    0.01147   2.886 0.003902 ** 
# EnvironmentSatisfaction         -0.30430    0.08763  -3.473 0.000515 ***
# JobInvolvement                  -0.62472    0.13157  -4.748 2.05e-06 ***
# JobLevel                        -0.31009    0.20630  -1.503 0.132823    
# JobRoleHuman Resources           1.82884    0.66735   2.740 0.006135 ** 
# JobRoleLaboratory Technician     1.73429    0.57166   3.034 0.002415 ** 
# JobRoleManager                   0.21193    0.96680   0.219 0.826490    
# JobRoleManufacturing Director    0.71625    0.60948   1.175 0.239928    
# JobRoleResearch Director        -0.84999    1.18836  -0.715 0.474444    
# JobRoleResearch Scientist        0.91259    0.57686   1.582 0.113650    
# JobRoleSales Executive           1.55464    0.51967   2.992 0.002776 ** 
# JobRoleSales Representative      2.15699    0.63002   3.424 0.000618 ***
# JobSatisfaction                 -0.41167    0.08592  -4.791 1.66e-06 ***
# MaritalStatusMarried             0.20579    0.27151   0.758 0.448489    
# MaritalStatusSingle              1.22701    0.27455   4.469 7.85e-06 ***
# OverTimeYes                      1.82986    0.20334   8.999  < 2e-16 ***
# WorkLifeBalance                 -0.34494    0.13548  -2.546 0.010895 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 1040.54  on 1176  degrees of freedom
# Residual deviance:  735.97  on 1153  degrees of freedom
# AIC: 783.97

# Number of Fisher Scoring iterations: 6


# vector of probabilities from the tarining data
p_train <- predict(model_3_stepwise, df_train, type = "response") # type response gives numeric results
# convert probabilities into factors with 2 levels Yes and No
p_train_attr <- as.factor(ifelse(p_train > 0.5, "Yes", "No"))
# confusion matrix for the training data
confusionMatrix(p_train_attr, df_train$Attrition)
# Confusion Matrix and Statistics

#           Reference
# Prediction  No Yes        
#        No  957 120        TN = 957        FN = 120        957+120 = 1077
#        Yes  30  70        FP = 30         TP = 70         30 +70 = 100
#                           957+30 = 987    120+70 = 190    total = 1177     
#                Accuracy : 0.8726          (TP+TN)/total = (957+70)/1177
#                  95% CI : (0.8521, 0.8911)
#     No Information Rate : 0.8386          
#     P-Value [Acc > NIR] : 0.0006482       
                                          
#                   Kappa : 0.418           
                                          
#  Mcnemar's Test P-Value : 3.68e-13        
                                          
#             Sensitivity : 0.9696          
#             Specificity : 0.3684         70/190, ~73% are mislabeled and ~37% are correctly labeled (ref Yes and pred Yes)
#          Pos Pred Value : 0.8886          
#          Neg Pred Value : 0.7000          
#              Prevalence : 0.8386      
#          Detection Rate : 0.8131          
#    Detection Prevalence : 0.9150          
#       Balanced Accuracy : 0.6690          
                                          
#        'Positive' Class : No              

p_test <- predict(model_3_stepwise, df_test, type = "response")
p_test_attr <- as.factor(ifelse(p_test > 0.5, "Yes", "No"))
confusionMatrix(p_test_attr, df_test$Attrition)
# Confusion Matrix and Statistics

#           Reference
# Prediction  No Yes
#        No  239  31
#        Yes   7  16
                                          
#                Accuracy : 0.8703          
#                  95% CI : (0.8264, 0.9066)
#     No Information Rate : 0.8396          
#     P-Value [Acc > NIR] : 0.0853192       
                                          
#                   Kappa : 0.3932          
                                          
#  Mcnemar's Test P-Value : 0.0001907       
                                          
#             Sensitivity : 0.9715          
#             Specificity : 0.3404          
#          Pos Pred Value : 0.8852          
#          Neg Pred Value : 0.6957          
#              Prevalence : 0.8396          
#          Detection Rate : 0.8157          
#    Detection Prevalence : 0.9215          
#       Balanced Accuracy : 0.6560          
                                          
#        'Positive' Class : No              
                                          

#### summary:
# 1. In both training and test data sets, model_3_stepwise can predict Attrition correctly 87% of the time. 
# However, when looking at No Information Rate, based on the features chosen, 83% of the time this model can predict correctly.
# 2. Specificity in both data sets are very low, 37% and 34% respectively, 
# we need to predict yes on actual yes, so we need to increase Specificity of the model.
# 
#### possible solution:
# 1. Change the threshold. Specificity = TN/(TN+FP), so to increase TN, which is predict "Yes" in this case,
# we can choose a lower threshold, to increase the number of predicted "Yes"s.
# 2. The sample is unbalanced with much more cases of No Attrition, oversample could solve this problem.


# 1.2 changing the threshold
# plot ROC curve
pred <- prediction(p_train, df_train$Attrition)
perf <- performance(pred, "tpr", "fpr") # true positive rate and false positive rate
plot(perf, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))
# 0.2 could be a better cutoff

# evaluate new cutoff
p_train <- predict(model_3_stepwise, df_train, type = "response")
p_train_attr1 <- as.factor(ifelse(p_train > 0.2, "Yes", "No"))
confusionMatrix(p_train_attr1, df_train$Attrition)
# Specificity : 0.7211          

p_test <- predict(model_3_stepwise, df_test, type = "response")
p_test_attr1 <- as.factor(ifelse(p_test > 0.2, "Yes", "No"))
confusionMatrix(p_test_attr1, df_test$Attrition)
# Specificity : 0.6596          

#### summary:
# specificity increased to 72% and 66% respectively for training and test data sets


# 1.3 oversample
df_over <- ovun.sample(formula = Attrition ~., data = df_train, method = "over")$data
table(df_over$Attrition) # check the number of Yes and No

# balanced model
model_4 <- glm(Attrition ~ age_bin + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_over, family = "binomial")
model_4_stepwise <- stepAIC(model_4, direction = "both", trace = FALSE)
summary(model_4_stepwise)

p_train4 <- predict(model_4_stepwise, df_over, type = "response")
# convert probabilities into factors with 2 levels Yes and No
p_train4_attr <- as.factor(ifelse(p_train4 > 0.5, "Yes", "No"))
# confusion matrix for the training data
confusionMatrix(p_train4_attr, df_over$Attrition)
# Specificity : 0.7800

p_test4 <- predict(model_4_stepwise, df_test, type = "response")
p_test4_attr <- as.factor(ifelse(p_test4 > 0.5, "Yes", "No"))
confusionMatrix(p_test4_attr, df_test$Attrition)
# Specificity : 0.7021  

#### summary:
# specificity increased to 78% and 70% for training and test data sets respectively

varImp(model_4_stepwise) %>%
    arrange(desc(Overall))
#### for the balanced logistic regression model, top 5 features for attrition are:
# 1. Overtime
# 2. MaritalStatus
# 3. Age
# 4. JobRole
# 5. JobInvolvement



########## 2. Decision tree ##########
set.seed(56789)

train <- df$Attrition %>%
    createDataPartition(p = 0.8, list = FALSE)
df_train <- df[train,]
df_test <- df[-train,]

# 2.1 fit the tree
tree_1 <- rpart::rpart(Attrition ~ age_bin + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_train, method = "class")

rpart.plot::prp(tree_1)

p_tree1 <- predict(tree_1, df_test, type = "class") # type class gives lables assigned to the values
confusionMatrix(p_tree1, df_test$Attrition)
# Specificity : 0.2340

# 2.2 tree with oversampling
df_over <- ovun.sample(formula = Attrition ~., data = df_train, method = "over")$data
tree_2 <- rpart::rpart(Attrition ~ age_bin + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_over, method = "class")
rpart.plot::prp(tree_2)
p_tree2 <- predict(tree_2, df_test, type = "class")
confusionMatrix(p_tree2, df_test$Attrition)
# Specificity : 0.4681 

#### summary:
# 1. with oversampling, specificity increased from 23% to 47%

varImp(tree_2) %>%
    arrange(desc(Overall))
#### for the balanced decision tree model, top 5 features for attrition are:
# 1. MonthlyIncome
# 2. JobRole
# 3. MaritalStatus
# 4. JobLevel
# 5. OverTime




########## 3. Random forest ##########
set.seed(56789)

train <- df$Attrition %>%
    createDataPartition(p = 0.8, list = FALSE)
df_train <- df[train,]
df_test <- df[-train,]

# 3.1
rf_1 <- randomForest::randomForest(Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_train, trControl = trainControl(method = 'cv', 10))
rf_1
# Call:
#  randomForest(formula = Attrition ~ Age + BusinessTravel + Department +      DistanceFromHome + EnvironmentSatisfaction + JobInvolvement +      JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome +      OverTime + WorkLifeBalance, data = df_train, ntree = 100,      importance = TRUE) 
#                Type of random forest: classification
#                      Number of trees: 100
# No. of variables tried at each split: 3

#         OOB estimate of  error rate: 14.7%       # (num of mislabled / total observation = (140+33)/1177) too high!
# Confusion matrix:
#      No Yes class.error
# No  954  33  0.03343465      
# Yes 140  50  0.73684211        # 140/190 too high!

# both OOB error rate and class error is too high, could be due to imbalanced data
# try over-sample the "Yes" reponses in the training data

p_rf1 <- predict(rf_1, df_test, type = "response")
confusionMatrix(p_rf1, df_test$Attrition)
# Specificity : 0.3191

# 3.2 oversample
df_over <- ovun.sample(formula = Attrition ~., data = df_train, method = "over")$data
rf_2 <- randomForest::randomForest(Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_over, trControl = trainControl(method = 'cv', 10))
rf_2

# OOB estimate of  error rate: 2.22%
# Confusion matrix:
#      No Yes class.error
# No  946  41 0.041540020
# Yes   3 989 0.003024194

# plotting error rate
plot(rf_2) # 100 trees should be good enough

# plotting mtry (num of variables at each split)
randomForest::tuneRF(df_over[,-2], df_over[,2], stepFactor = 0.5, 
                    plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)
# mtry = 6  OOB error = 1.77% 
# Searching left ...
# mtry = 12 	OOB error = 2.32% 
# -0.3142857 0.05 
# Searching right ...
# mtry = 3 	OOB error = 1.26% 
# 0.2857143 0.05 
# mtry = 1 	OOB error = 7.53% 
# -4.96 0.05 
#        mtry   OOBError
# 1.OOB     1 0.07529055
# 3.OOB     3 0.01263264        # mtry = 3 could be the best
# 6.OOB     6 0.01768570
# 12.OOB   12 0.02324406

rf_3 <- randomForest::randomForest(Attrition ~ Age + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df_over, ntree = 100, mtry = 3, trControl = trainControl(method = 'cv', 10))


p_rf3 <- predict(rf_3, df_test, type = "response")
confusionMatrix(p_rf3, df_test$Attrition)
# Specificity : 0.3404

# feature importance
varImp(rf_3) %>%
    arrange(desc(Overall))
# plot feature importance (only for rf)
varImpPlot(rf_3)

### top 5 important features:
# 1. MonthlyIncome
# 2. Age
# 3. JobRole
# 4. DistanceFromHome
# 5. OverTime

#####
# need more research on why random forest does not work here
# https://www.rpubs.com/Omar_Juarez/644317


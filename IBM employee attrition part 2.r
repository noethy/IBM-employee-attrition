### part 2 correlation and regression models

install.packages("ggcorrplot")
install.packages("car")
install.packages("MASS")

library(ggcorrplot)
library(car)
library(MASS)




########## 5. correlation between numeric variables ##########
nums <- df %>%
    select_if(is.numeric) %>%
    select(-EmployeeCount, -StandardHours) # filter out two variables without variance
correlation <- round(cor(nums), 1)
ggcorrplot(correlation, 
            type = "lower", 
            lab = TRUE)

#### 5. summary:
# 5.1 r(MonthlyIncome, JobLevel) = 1
# 5.2 r(PerformanceRating, PercentSalaryHike) = 0.8
# 5.3 r(TotalWorkingYears, MonthlyIncome) = 0.8

# 5.1 r(MonthlyIncome, JobLevel) = 1
ggplot(df, aes(x = JobLevel, y = MonthlyIncome)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# 5.2 r(PerformanceRating, PercentSalaryHike) = 0.8
ggplot(df, aes(x = factor(PerformanceRating), y = PercentSalaryHike)) +
    geom_point()

# 5.3 r(TotalWorkingYears, MonthlyIncome) = 0.8
ggplot(df, aes(x = TotalWorkingYears, y = MonthlyIncome)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)


########## 6. regression models ##########
# since our DV is binomial, use glm to fit the data
model_1 <- glm(Attrition ~ age_bin + BusinessTravel + Department + DistanceFromHome + EnvironmentSatisfaction
                            + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome
                            + OverTime + WorkLifeBalance,
                data = df, family = "binomial")
summary(model_1)

# Call:
# glm(formula = Attrition ~ age_bin + BusinessTravel + Department + 
#     DistanceFromHome + EnvironmentSatisfaction + JobInvolvement + 
#     JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + 
#     OverTime + WorkLifeBalance, family = "binomial", data = df)

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.0671  -0.5232  -0.3067  -0.1351   3.3842  

# Coefficients:
#                                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      -1.191e+01  4.084e+02  -0.029 0.976736    
# age_bin[25,35)                   -7.336e-01  2.921e-01  -2.512 0.012018 *  
# age_bin[35,45)                   -1.497e+00  3.212e-01  -4.660 3.16e-06 ***
# age_bin[45,55)                   -1.227e+00  3.876e-01  -3.167 0.001543 ** 
# age_bin[55,Inf)                  -5.429e-01  4.827e-01  -1.125 0.260707    
# BusinessTravelTravel_Frequently   1.609e+00  3.812e-01   4.221 2.43e-05 ***
# BusinessTravelTravel_Rarely       8.070e-01  3.541e-01   2.279 0.022652 *  
# DepartmentResearch & Development  1.243e+01  4.084e+02   0.030 0.975709    
# DepartmentSales                   1.210e+01  4.084e+02   0.030 0.976366    
# DistanceFromHome                  3.690e-02  1.001e-02   3.685 0.000228 ***
# EnvironmentSatisfaction          -3.771e-01  7.728e-02  -4.879 1.07e-06 ***
# JobInvolvement                   -5.657e-01  1.155e-01  -4.898 9.66e-07 ***
# JobLevel                         -2.968e-01  2.799e-01  -1.060 0.288966    
# JobRoleHuman Resources            1.405e+01  4.084e+02   0.034 0.972560    
# JobRoleLaboratory Technician      1.394e+00  4.626e-01   3.013 0.002583 ** 
# JobRoleManager                    1.460e-01  8.332e-01   0.175 0.860944    
# JobRoleManufacturing Director     9.635e-02  5.065e-01   0.190 0.849121    
# JobRoleResearch Director         -1.244e+00  9.176e-01  -1.356 0.175169    
# JobRoleResearch Scientist         5.698e-01  4.690e-01   1.215 0.224372    
# JobRoleSales Executive            1.471e+00  1.087e+00   1.354 0.175746    
# JobRoleSales Representative       2.268e+00  1.136e+00   1.997 0.045854 *  
# JobSatisfaction                  -3.820e-01  7.538e-02  -5.067 4.03e-07 ***
# MaritalStatusMarried              2.950e-01  2.444e-01   1.207 0.227306    
# MaritalStatusSingle               1.190e+00  2.468e-01   4.823 1.41e-06 ***
# MonthlyIncome                     5.582e-05  7.464e-05   0.748 0.454507    
# OverTimeYes                       1.809e+00  1.801e-01  10.043  < 2e-16 ***
# WorkLifeBalance                  -3.543e-01  1.183e-01  -2.996 0.002739 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 1298.6  on 1469  degrees of freedom
# Residual deviance:  938.5  on 1443  degrees of freedom
# AIC: 992.5

# Number of Fisher Scoring iterations: 14

# check for multicollinearity
vif(mod = model_1) 
# vif = 1/(1-R^2), how much a xi is determined by all the other x (s jointly) in the model
# vif > 5 is too large
#                                 GVIF Df GVIF^(1/(2*Df))
# age_bin                 1.536808e+00  4        1.055182
# BusinessTravel          1.083222e+00  2        1.020186
# Department              3.655649e+07  2       77.757293
# DistanceFromHome        1.043203e+00  1        1.021373
# EnvironmentSatisfaction 1.052334e+00  1        1.025833
# JobInvolvement          1.038395e+00  1        1.019017
# JobLevel                9.038053e+00  1        3.006336 # vif too large
# JobRole                 1.670629e+08  8        3.265352
# JobSatisfaction         1.050977e+00  1        1.025171
# MaritalStatus           1.072945e+00  2        1.017758
# MonthlyIncome           9.911154e+00  1        3.148199 # vif too large
# OverTime                1.151219e+00  1        1.072949
# WorkLifeBalance         1.035052e+00  1        1.017375

# improve model_1 by dropping some variables based on AIC (Akaike Information Criteria), using a stepwise method
model_2 <- stepAIC(model_1, direction = "both", trace = FALSE)
summary(model_2)

# Call:
# glm(formula = Attrition ~ age_bin + BusinessTravel + DistanceFromHome + 
#     EnvironmentSatisfaction + JobInvolvement + JobRole + JobSatisfaction + 
#     MaritalStatus + OverTime + WorkLifeBalance, family = "binomial", 
#     data = df)

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.0700  -0.5247  -0.3111  -0.1379   3.3462  

# Coefficients:
#                                  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      0.266429   0.797435   0.334 0.738298    
# age_bin[25,35)                  -0.731379   0.290722  -2.516 0.011878 *  
# age_bin[35,45)                  -1.515219   0.315104  -4.809 1.52e-06 ***
# age_bin[45,55)                  -1.297450   0.372884  -3.480 0.000502 ***
# age_bin[55,Inf)                 -0.587914   0.471037  -1.248 0.211984    
# BusinessTravelTravel_Frequently  1.604687   0.380491   4.217 2.47e-05 ***
# BusinessTravelTravel_Rarely      0.814511   0.353408   2.305 0.021181 *  
# DistanceFromHome                 0.035815   0.009968   3.593 0.000327 ***
# EnvironmentSatisfaction         -0.378237   0.077220  -4.898 9.67e-07 ***
# JobInvolvement                  -0.567939   0.115141  -4.933 8.12e-07 ***
# JobRoleHuman Resources           1.694361   0.534958   3.167 0.001539 ** 
# JobRoleLaboratory Technician     1.507131   0.413393   3.646 0.000267 ***
# JobRoleManager                  -0.080597   0.618751  -0.130 0.896363    
# JobRoleManufacturing Director    0.082165   0.506117   0.162 0.871035    
# JobRoleResearch Director        -1.211344   0.840435  -1.441 0.149492    
# JobRoleResearch Scientist        0.694895   0.417795   1.663 0.096264 .  
# JobRoleSales Executive           1.134536   0.406530   2.791 0.005258 ** 
# JobRoleSales Representative      2.049004   0.466509   4.392 1.12e-05 ***
# JobSatisfaction                 -0.385220   0.075340  -5.113 3.17e-07 ***
# MaritalStatusMarried             0.296903   0.244037   1.217 0.223746    
# MaritalStatusSingle              1.183095   0.246258   4.804 1.55e-06 ***
# OverTimeYes                      1.809123   0.180060  10.047  < 2e-16 ***
# WorkLifeBalance                 -0.353296   0.118208  -2.989 0.002801 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

#     Null deviance: 1298.58  on 1469  degrees of freedom
# Residual deviance:  940.68  on 1447  degrees of freedom
# AIC: 986.68

# Number of Fisher Scoring iterations: 6

vif(model_2)
#                             GVIF Df GVIF^(1/(2*Df))
# age_bin                 1.340243  4        1.037285
# BusinessTravel          1.074834  2        1.018205
# DistanceFromHome        1.035026  1        1.017362
# EnvironmentSatisfaction 1.051234  1        1.025297
# JobInvolvement          1.034095  1        1.016905
# JobRole                 1.370746  8        1.019905
# JobSatisfaction         1.048892  1        1.024154
# MaritalStatus           1.069843  2        1.017021
# OverTime                1.152211  1        1.073411
# WorkLifeBalance         1.032564  1        1.016151

# features have been reduced from 13 to 10
# no problem with multicollinearity
# model_2 improved with lower AIC scores

#### summary
# 1. Age(-). Younger employees (<25) are more likely to quit IBM.
# 2. BusinessTravel(+). Business travel(regardless of frequency) leads to higher attrition.
# 3. DistanceFromHome (+). Farther away from work leads to higher attrition.
# 4. EnvironmentSatisfaction(-). When employees are less satisfied with the environment, they are more likely to quit.
# 5. JobInvolvement (-). When employees are less involved with their job, they are more likely to quit.
# 6. JobRole(?). Compared to Healthcare Representative (7%), employees in HR, Lab tech, Research Scientist, sales (executive and
#                representative) roles are more likely to quit.
#               EDA shows manager (5%), manufactruing direcitor (7%) and research director (3%) all have low relative attrition rate
# 7. JobSatisfaction (-). Less satisfied with their jobs, more likely to quit.
# 8. MaritalStatus(?). People who are single are more likely to leave IBM than divorced.
#                   EDA shows relative attrition rate does not differ much between divorced (10%) and married (12%).
# 9. OverTime(+). People work more overtime are more likely to leave.
# 10. WorkLifeBalance(-). People who have exprienced worse wlb are more likely to leave.
# data and project: https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset

### part 1 EDA plots
setwd ("/Users/Cheryl/Dropbox/personal documents/jobs/interview/Technical interview materials/R/IBM attrition case study")

install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("skimr")
install.packages("tidyverse")
install.packages("moderndive")
install.packages("infer")

library(dplyr)
library(readr)
library(ggplot2)
library(skimr)
library(tidyverse)
library(moderndive)
library(infer)

raw <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
glimpse(raw)
raw <- raw %>%
    mutate_if(is.character, factor)

# visualize attrition yes vs no
raw %>%
    filter(!is.na(Attrition)) %>%
    group_by(Attrition) %>%
    summarize(n_attrition = n()) %>%
ggplot(aes(x = Attrition, y = n_attrition, fill = Attrition)) +
    geom_bar(stat = "identity") + # have to add stat = "identity" when providing count
    geom_text(aes(label = n_attrition), vjust = -1) + # add numbers to each bar
    labs(x = "Employee Attrition",
        y = "Amount",
        title = "Employee Attrition (Amount)")
# calculate attrition rate
raw %>%
    specify(response = Attrition, success = "Yes") %>%
    calculate(stat = "prop")
# attrition rate is 16.1%



########## 1. the effect of gender ##########
# 1.1 attrition rate by gender
raw %>%
    specify(Attrition ~ Gender, success = "Yes") %>%
    calculate(stat = "diff in props", order = c("Male", "Female"))
# males are 2.21% higher in attrition rate than females

# number of attrition by gender
raw%>%
    group_by(Gender, Attrition) %>%
    summarize(count = n()) %>%
    ggplot(aes(x = Gender, y = count, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = count), vjust = -1, position = position_dodge(width = 1)) +
        labs(y = "Number of Attrition")
# cannot really tell, need to look at percentage of attrion by gender
raw%>%
    group_by(Gender, Attrition) %>%
    summarize(count = n()) %>%
    mutate(attr_per_gen = as.numeric(paste0(round(count/sum(count), 2) * 100))) %>%
    ggplot(aes(x = Gender, y = attr_per_gen, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_gen), vjust = -1, position = position_dodge(width = 1)) +
        labs(y = "Relative Percentage (%) of Attrition")
# attrtion rate does not differ much between gender

# 1.2 average Age by gender
raw %>%
    select(Gender, Age) %>%
    group_by(Gender) %>%
    summarize(mean_age = mean(Age))
ggplot(raw, aes(x = Age)) +
    geom_density(aes(fill = Gender)) + # different gender has different color
    facet_wrap(~ Gender) +
    geom_vline(aes(xintercept = mean(Age)), color = "red", linetype = "dashed") # add a vertical line with mean age for each gender
# age in male and female have similar distribution and similar average

# 1.3 Department by gender
class(raw$Department) # Department is a character variable
unique(raw$Department) # 3 departments, "Sales", "Research & Development" and "Human Resources" 
ggplot(raw, aes(x = Department, fill = Gender)) +
    geom_bar(position = "dodge")

# 1.4 JobLevel by gender
class(raw$JobLevel) # numeric factor
unique(raw$JobLevel) # levels 1-5
ggplot(raw, aes(x = JobLevel, fill = Gender)) +
    geom_bar(position = "dodge")

# 1.5 MonthlyIncome by gender
range(raw$MonthlyIncome) # [1009, 19999]
raw %>%
    group_by(Gender) %>%
    summarize(month_mean = mean(MonthlyIncome),
            month_median = median(MonthlyIncome))
ggplot(raw, aes(x = Gender, y = MonthlyIncome, fill = Gender)) +
    geom_boxplot()

# 1.6 PerformanceRating by gender
unique(raw$PerformanceRating) # 3 & 4
ggplot(raw, aes(x = factor(PerformanceRating), fill = Gender)) +
    geom_bar(position = "dodge") +
        scale_x_discrete("Performace Rating", labels = c("1" = "Low" , 
                                                    "2" = "Good" , 
                                                    "3" = "Excellent", 
                                                    "4" = "Outstanding"), position = "bottom")

# 1.7 JobSatisfaction by gender
ggplot(raw, aes(x = factor(JobSatisfaction), fill = Gender)) + # convert JobSatisfaction from numeric to factor
    geom_bar(position = "dodge") +
    scale_x_discrete("Job Satisfaction", labels = c("1" = "Low" , 
                                                    "2" = "Medium" , 
                                                    "3" = "High", 
                                                    "4" = "Very High"), position = "bottom")



########## 2. the effect of personal facotrs: age (age_bin), education, MaritalStatus  ##########
# age might have a differnt way of affecting attrition

# 2.1 age_bin
df_age <- raw %>%
    mutate(age_bin = cut(Age,
                        breaks = c(0, 25, 35, 45, 55, Inf),
                        right = FALSE, .keep = "all")
            ) 
df_age %>%
    group_by(age_bin, Attrition) %>%
    summarize(attr_n_ageb = n()) %>%
    ggplot(aes(x = age_bin, y = attr_n_ageb, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge")
# attrition percentage by age bin
df_age %>%
    group_by(age_bin, Attrition) %>%
    summarize(attr_n_ageb = n()) %>%
    mutate(attr_per_ageb = as.numeric(paste0(round(attr_n_ageb/sum(attr_n_ageb), 2) * 100))) %>%
    ggplot(aes(x = age_bin, y = attr_per_ageb, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_ageb), position = position_dodge(width = 1), vjust = -1) +
        labs(x = "Age",
            y = "Relative Percentage (%) of Attrition")
 # employees between age [0,25) have the highest attrition rate 39%

# 2.2 education
# label education level with a new variable edu_level
df <- df_age %>%
    mutate(edu_level = ifelse(Education == 1, "Below College",
                        ifelse(Education == 2, "College",
                        ifelse(Education == 3, "Bachelor",
                        ifelse(Education == 4, "Master", "Doctor")))))
# visualize attrition count by education level
df %>%
    group_by(edu_level, Attrition) %>%
    summarize(attr_n_edul = n()) %>%
    ggplot(aes(x = reorder(edu_level, -attr_n_edul), y = attr_n_edul, fill = Attrition)) + # desc order by the numeric value of num_attrition
    geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_n_edul), position = position_dodge(width = 1), vjust = -1) + # position is needed to place each number on top of its own bar
        labs(x = "Education Level",
        y = "Number of Attritions",
        title = "Number of Attritions by Education Level")

# visualize attrition percentage by education level
df %>%
    select(Attrition, edu_level) %>%
    group_by(edu_level, Attrition) %>%
    summarize(attr_n_edul = n()) %>%
    mutate(attr_per_edul = as.numeric(paste0(round(attr_n_edul/sum(attr_n_edul), 2) * 100))) %>% # paste0 generates character instead of numeric
    ggplot(aes(x = reorder(edu_level, -attr_per_edul), y = attr_per_edul, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_edul), position = position_dodge(width = 1), vjust = -1) + # position is needed to place each number on top of its own bar
        labs(x = "Education Level",
            y = "Relative Percentage (%) of Attrition",
            title = "Percentage (%) of Attritions by Education Level")
# relative percentage of attrition seems not affected by education level

# 2.3 MaritalStatus
df %>%
    group_by(MaritalStatus, Attrition) %>%
    summarize(attr_n_ms = n()) %>%
    mutate(attr_per_ms = as.numeric(paste0(round(attr_n_ms/sum(attr_n_ms), 2) * 100))) %>%
    ggplot(aes(x = MaritalStatus, y = attr_per_ms, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_ms), position = position_dodge(width = 1), vjust = -1) +
        labs(x = "Marital Status",
            y = "Relative Percentage (%) of Attrition")
# single employees have higher attrition rate

#### 2. summary:
# 2.1 age_bin: employees between age [0,25) have the highest attrition rate 39%
# 2.2 MaritalStatus: single employees have higher attrition rate




########## 3. the effect of objective measures, BusinessTravel, DistanceFromHome, 
# JobLevel, JobRole, MonthlyIncome, NumCompaniesWorked, OverTime, StandardHours ##########
# 3.1 BusinessTravel
class(df$BusinessTravel) # character
unique(df$BusinessTravel) # "Travel_Rarely", "Travel_Frequently", "Non-Travel"   
df %>%
    group_by(BusinessTravel, Attrition) %>%
    summarize(attr_n_bt = n()) %>%
    mutate(attr_per_bt = as.numeric(paste0(round(attr_n_bt/sum(attr_n_bt), 2) * 100))) %>%
    ggplot(aes(x = BusinessTravel, y = attr_per_bt, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_bt), position = position_dodge(width = 1), vjust = -1) +
        labs(x = "Business Travel",
            y = "Relative Percentage (%) of Attrition")
# business travel more, % attritrion higher

# 3.2 DistanceFromHome
class(df$DistanceFromHome) # numeric
range(df$DistanceFromHome) # 1 29
# distribution of distance from home
ggplot(df, aes(x = DistanceFromHome))+
    geom_density()
# strongly right skewed, median could be a better measure of center
# use median DistanceFromHome of people who did not quit (attrition = no) as baseline
df %>%
    filter(Attrition == "No") %>%
    summarize(med_distance = median(DistanceFromHome))
# 7
df %>%
    mutate(distance = ifelse(DistanceFromHome > 7, "Above Average", "Below Average")) %>%
    group_by(distance, Attrition) %>%
    summarize(attr_n_dfh = n()) %>%
    mutate(attr_per_dfh = as.numeric(paste0(round(attr_n_dfh/sum(attr_n_dfh), 2) * 100))) %>%
    ggplot(aes(x = distance, y = attr_per_dfh, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_dfh), position = position_dodge(width = 1)) +
        labs(x = "Distance From Home (Median for those who did not quit is 7)",
            y = "Relative Percentage (%) of Attrition")
# those who live farther than average(median) has a higher attrition rate, diff 5%


# 3.3 JobLevel, this could nested within Department
class(df$JobLevel) # numeric
unique(df$JobLevel) # 1 2 3 4 5
df %>%
    group_by(Department, JobLevel, Attrition) %>%
    ggplot(aes(x = JobLevel, fill = Attrition)) +
        geom_bar(position = position_dodge(preserve = "single")) +
        labs(y = "Attrition Count") +
        facet_wrap(~ Department)
# attrition percentage by jl
df %>%
    group_by(Department, JobLevel, Attrition) %>%
    summarize(n_jl = n()) %>%
    mutate(attr_per_jl = as.numeric(paste0(round(n_jl/sum(n_jl), 2) * 100 ))) %>%
    ggplot(aes(x = JobLevel, y = attr_per_jl, fill = Attrition)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
        geom_text(aes(label = attr_per_jl), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition") +
        facet_wrap(~ Department)
# in R&D and sales, % attrition is higher when job level is lower, highest % attrition in sales when job level = 1


# 3.4 JobRole
class(df$JobRole) # character
unique(df$JobRole)
# [1] "Sales Executive"           "Research Scientist"        "Laboratory Technician"     "Manufacturing Director"   
# [5] "Healthcare Representative" "Manager"                   "Sales Representative"      "Research Director"        
# [9] "Human Resources"   
df %>%
    group_by(JobRole, Attrition) %>%
    ggplot(aes(x = JobRole, fill = Attrition, label = NULL)) +
        geom_bar(position = position_dodge(preserve = "single")) +
        labs(y = "Attrition Count")
# attrition percentage by jr
df %>%
    group_by(JobRole, Attrition) %>%
    summarize(n_jr = n()) %>%
    mutate(attr_per_jr = as.numeric(paste0(round(n_jr/sum(n_jr), 2) * 100 ))) %>%
    ggplot(aes(x = JobRole, y = attr_per_jr, fill = Attrition)) +
        geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
        geom_text(aes(label = attr_per_jr), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# % attrition highest in sales representative


# 3.5 MonthlyIncome, this could nested within Department
df %>%
    ggplot(aes(x = MonthlyIncome)) +
        geom_density()
# strongly right skewed, use median as measure of center
df %>%
    filter(Attrition == "No") %>%
    summarize(med_mi = median(MonthlyIncome))
# 5204
df %>% 
    mutate(milevel = ifelse(MonthlyIncome > 5204, "Above Average", "Below Average")) %>%
    group_by(Department, milevel, Attrition) %>%
    summarize(attr_n_mi = n()) %>%
    mutate(attr_per_mi = as.numeric(paste0(round(attr_n_mi/sum(attr_n_mi), 2) * 100))) %>%
    ggplot(aes(x = milevel, y = attr_per_mi, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_mi), position = position_dodge(width = 1)) +
        labs(x = "Monthly Income (Median for those who did not quit is 5204)",
            y = "Relative Percentage (%) of Attrition") +
        facet_wrap(~Department)
# people quit more when their salary is below average(median)
# in HR and R&D (diff = (%attrition below) - (%attrition above) median are 18% and 13% respectively)
# in sales, the diff is 8%


df %>%
    group_by(Department, Attrition) %>%
    summarize(mean_income = mean(MonthlyIncome)) %>%
    ggplot(aes(x = Department, y = mean_income, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(y = "Average Monthly Income in USD")

# 3.6 NumCompaniesWorked
class(df$NumCompaniesWorked) # numeric
sort(unique(df$NumCompaniesWorked)) # 0 1 2 3 4 5 6 7 8 9
df %>%
    group_by(NumCompaniesWorked, Attrition) %>%
    summarize(n_ncw = n()) %>%
    mutate(attr_per_ncw = as.numeric(paste0(round(n_ncw/sum(n_ncw), 2) * 100))) %>%
    ggplot(aes(x = factor(NumCompaniesWorked), y = attr_per_ncw, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_ncw), position = position_dodge(width = 1), vjust = -1) +
        labs(x = "Number of Companies Worked",
            y = "Relative Percentage (%) of Attrition")
# not quite relevant


# 3.7 OverTime
# attrition count by overtime
class(df$OverTime) # character
unique(df$OverTime) # "Yes" "No"

df %>%
    ggplot(aes(x = OverTime, fill = Attrition)) +
        geom_bar(position = "dodge")
# attrition percentage by ot
df %>%
    group_by(OverTime, Attrition) %>%
    summarize(n_ot = n()) %>%
    mutate(attr_per_ot = as.numeric(paste0(round(n_ot/sum(n_ot), 2) * 100))) %>%
    ggplot(aes(x = OverTime, y = attr_per_ot, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = attr_per_ot), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# overtime affects % attrition, more work hours led to higher % attrition


# 3.8 StandardHours
class(df$StandardHours) # numeric
range(df$StandardHours) # 80 80
# cannot affect attrition

#### 3. summary:
# 3.1 BusinessTravel: business travel more, % attritrion higher
# 3.1 DistanceFromHome: those who live farther than average(median) has a higher attrition rate, diff 5%
# 3.2 MonthlyIncome: people quit more when their salary is below average(median)
# 3.3 JobLevel: in R&D and sales, % attrition is higher when job level is lower, highest % attrition in sales when job level = 1
# 3.4 JobRole: % attrition highest in sales representative
# 3.5 OverTime: more work hours led to higher % attrition



########## 4. the effect of subjective measures, EnvironmentSatisfaction, JobInvolvement, JobSatisfaction, PerformanceRating, 
#WorkLifeBalance ##########
# 4.1 EnvironmentSatisfaction
class(df$EnvironmentSatisfaction) # numeric
unique(df$EnvironmentSatisfaction) # 1 2 3 4
ggplot(df, aes(x = factor(EnvironmentSatisfaction), fill = Attrition)) +
    geom_bar(position = "dodge") +
    scale_x_discrete("Environment Satisfaction", labels = c("1" = "Low" , 
                                                            "2" = "Medium" , 
                                                            "3" = "High", 
                                                            "4" = "Very High"), position = "bottom")
# attrition percentage by es
df %>%
    group_by(EnvironmentSatisfaction, Attrition) %>%
    summarize(n_es = n()) %>%
    mutate(attr_per_es = as.numeric(paste0(round(n_es/sum(n_es), 2) * 100))) %>%
    ggplot(aes(x = factor(EnvironmentSatisfaction), y = attr_per_es, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete("Environment Satisfaction", labels = c("1" = "Low" , 
                                                                "2" = "Medium" , 
                                                                "3" = "High", 
                                                                "4" = "Very High"), position = "bottom") +
        geom_text(aes(label = attr_per_es), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# people tend to leave when es is low, no diff among the other three levels


# 4.2 JobInvolvement
class(df$JobInvolvement) # numeric
unique(df$JobInvolvement) # 1 2 3 4
ggplot(df, aes(x = factor(JobInvolvement), fill = Attrition)) +
    geom_bar(position = "dodge") +
    scale_x_discrete("Job Involvement", labels = c("1" = "Low", 
                                                  "2" = "Medium", 
                                                  "3" = "High", 
                                                  "4" = "Very High"), position = "bottom")
# attrition percentage by ji
df %>%
    group_by(JobInvolvement, Attrition) %>%
    summarize(n_ji = n()) %>%
    mutate(attr_per_ji = as.numeric(paste0(round(n_ji/sum(n_ji), 2) * 100))) %>%
    ggplot(aes(x = factor(JobInvolvement), y = attr_per_ji, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete("Job Involvement", labels = c("1" = "Low", 
                                                    "2" = "Medium", 
                                                    "3" = "High", 
                                                    "4" = "Very High"), position = "bottom") +
        geom_text(aes(label = attr_per_ji), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# when people are more involved in their jobs, they are less likely to leave


# 4.3 JobSatisfaction
class(df$JobSatisfaction) # numeric
unique(df$JobSatisfaction) # 1 2 3 4
ggplot(df, aes(x = factor(JobSatisfaction), fill = Attrition)) +
    geom_bar(position = "dodge") +
    scale_x_discrete("Job Satisfaction", labels = c("1" = "Low" , 
                                                    "2" = "Medium" , 
                                                    "3" = "High", 
                                                    "4" = "Very High"), position = "bottom") 
# attrition percentage by js
df %>%
    group_by(JobSatisfaction, Attrition) %>%
    summarize(n_js = n()) %>%
    mutate(attr_per_js = as.numeric(paste0(round(n_js/sum(n_js), 2) * 100))) %>%
    ggplot(aes(x = factor(JobSatisfaction), y = attr_per_js, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete("Job Satisfaction", labels = c("1" = "Low" , 
                                                        "2" = "Medium" , 
                                                        "3" = "High", 
                                                        "4" = "Very High"), position = "bottom") +
        geom_text(aes(label = attr_per_js), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# when js is the lowest, % attrition is the highest


# 4.4 PerformanceRating
class(df$PerformanceRating) # numeric
unique(df$PerformanceRating) # 3 4
ggplot(df, aes(x = factor(PerformanceRating), fill = Attrition)) +
    geom_bar(position = position_dodge()) +
    scale_x_discrete("Performance Rating", labels = c("1" = "Low" , 
                                                    "2" = "Good" , 
                                                    "3" = "Excellent", 
                                                    "4" = "Outstanding"), position = "bottom", drop = FALSE)
# attrition percentage by pr
df %>%
    group_by(PerformanceRating, Attrition) %>%
    summarize(n_pr = n()) %>%
    mutate(attr_per_pr = as.numeric(paste0(round(n_pr/sum(n_pr), 2) * 100))) %>%
    ggplot(aes(x = factor(PerformanceRating), y = attr_per_pr, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete("Performance Rating", labels = c("1" = "Low" , 
                                                        "2" = "Good" , 
                                                        "3" = "Excellent", 
                                                        "4" = "Outstanding"), position = "bottom", drop = FALSE) +
        geom_text(aes(label = attr_per_pr), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# no difference


# 4.5 WorkLifeBalance
class(df$WorkLifeBalance) # numeric
unique(df$WorkLifeBalance) # 1 2 3 4
ggplot(df, aes(x = factor(WorkLifeBalance), fill = Attrition)) +
    geom_bar(position = position_dodge()) +
    scale_x_discrete("WorkLifeBalance", labels = c("1" = "Bad" , 
                                                    "2" = "Good" , 
                                                    "3" = "Better", 
                                                    "4" = "Best"), position = "bottom", drop = FALSE) +
    labs(x = "WorkLife Balance")
# attrition percentage by wlb
df %>%
    group_by(WorkLifeBalance, Attrition) %>%
    summarize(n_wlb = n()) %>%
    mutate(attr_per_wlb = as.numeric(paste0(round(n_wlb/sum(n_wlb), 2) * 100))) %>%
    ggplot(aes(x = factor(WorkLifeBalance), y = attr_per_wlb, fill = Attrition)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_x_discrete("WorkLifeBalance", labels = c("1" = "Bad" , 
                                                        "2" = "Good" , 
                                                        "3" = "Better", 
                                                        "4" = "Best"), position = "bottom", drop = FALSE) +
        geom_text(aes(label = attr_per_wlb), position = position_dodge(width = 1), vjust = -1) +
        labs(y = "Relative Percentage (%) of Attrition")
# wlb = bad, % attrition highest


#### 4. summary:
# 4.1 EnvironmentSatisfaction: people tend to leave when es is low, no diff among the other three levels
# 4.2 JobInvolvement: when people are more involved in their jobs, they are less likely to leave
# 4.3 JobSatisfaction: when js is the lowest, % attrition is the highest
# 4.4 WorkLifeBalance: wlb = bad, % attrition highest

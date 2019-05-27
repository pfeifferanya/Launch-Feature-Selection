library(readxl)
data <- read_excel("Oliver/Hack Cville Internship/Countdown/Mall_Customers.xlx.xlsx")
data = data[,-1]
data
install.packages('tidyverse')
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

names(data) <- c("gender", "age",'income','score')

full.model <- lm(score ~., data = data)
summary(full.model)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
forward.model = step.model <- stepAIC(full.model, direction = "forward", 
                                      trace = FALSE)
backwards.model = step.model <- stepAIC(full.model, direction = "backward", 
                                        trace = FALSE)
summary(backwards.model) 
BIC(step.model)
#Full model has gerndermale, age, and anual income: adjR2 = 0.09496,
  #F pval = 4.91e-0.5, AIC = 1854.093, BIC = 1870.585
#Step model has intercept and age: adj_r2 = 0.1026, Fstat pval = 2.25e-0.6,
  #AIC = 1850.436, BIC = 1860.331
#Forward: same as full model
#Backwards: same as stepwise

interaction.model = lm(score ~ age + gender*income, data=data)
summary(int.model)



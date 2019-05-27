install.packages("dplyr")
install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(ggplot2)

mallData <- read.csv("Mall_Customers.csv", header = TRUE)
mallData <- select(mallData,-c(CustomerID,Gender))
names(mallData) <- c("Age", "AnnualIncome", "SpendingScore")
mallData
ageRangeList2 <- c(0,0,0,0,0,0,1,0,2,0,2,1,2,0,1,0,1,0,2,1,1,0,1,0,2,0,1,1,1,0,2,0,2,0,1,
                   0,1,2,0,1,1,2,1,0,2,2,1,2,1,0,2,1,2,1,2,0,1,1,1,2,1,2,1,2,1,0,2,0,2,1,
                   0,0,0,0,2,2,2,1,0,2,0,1,2,1,0,2,0,0,0,0,1,2,1,0,2,1,2,1,0,2,1,2,1,2,1,
                   1,2,1,2,1,1,1,1,0,0,0,2,1,2,1,2,1,2,0,2,1,2,0,0,1,1,2,1,0,1,0,0,2,1,2,
                   0,2,1,2,1,0,0,0,2,1,2,1,2,2,2,2,1,1,2,1,0,0,0,0,2,1,2,0,0,2,1,2,0,1,2,
                   1,2,1,2,1,2,2,2,0,0,0,1,2,1,0,2,2,1,2,0,1,0,2,0,1)

mallData$AgeRange <- ageRangeList2

ggplot(mallData, aes(x=AnnualIncome, y=SpendingScore, color=AgeRange)) + geom_point(shape=2)

res.aov2 <- aov(SpendingScore ~ AgeRange + AnnualIncome, data = mallData)
summary(res.aov2)

mallData['Age']
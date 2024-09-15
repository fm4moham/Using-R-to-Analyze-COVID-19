rm(list=ls()) #Removes all variables stored previously
library(Hmisc) #Hmisc command

data <- read.csv("C:/Users/Farah/OneDrive/Desktop/uni/y3/3b/Courses/projects/COVID R/COVID19_line_list_data.csv")
describe(data )

#Cleaned up the death column
data$death_dummy <- as.integer(data$death != 0)
#Death rate
sum(data$death_dummy) / nrow(data)

#Age
# claim: people who die are older
dead= subset(data, death_dummy ==1)
alive = subset(data,death_dummy ==0)

#We remove unknown entries and find the 
#mean alive and dead ages
mean(dead$age, na.rm= TRUE)
mean(alive$age, na.rm= TRUE)

#checking if this is statistically significant
t.test(alive$age, dead$age, alternative="two.sided", conf.level= 0.95)
t.test(alive$age, dead$age, alternative="two.sided", conf.level= 0.99)

#Since the p-value < 0.05, we reject the hypothesis
#Therefore this is statistically significant

#Gender
#claim: gender has no effect
men= subset(data,gender =="male")
women= subset(data, gender== "female")
mean(men$death_dummy, na.rm= TRUE) #8.4%!
mean(women$death_dummy, na.rm= TRUE) #3.7%!

#checking if this is statistically significant
t.test(men$death_dummy,women$death_dummy, alternative="two.sided", conf.level= 0.95)

#With 95% confidence: men have from 0.8-7.8% higher chance of death from COVID
#p-value = 0.002 <0.05, so this is statistically 
#significant 

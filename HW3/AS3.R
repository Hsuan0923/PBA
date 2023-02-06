require(dplyr)
require(data.table)
require(lubridate)

# 1) Import the csv file into R and present the descriptive statistics
data<-read.csv("/Users/hsuan/Desktop/PBA/HW3/banksalary.csv")
str(data)
summary(data)

# data$Gender <- gsub("Male","0",data$Gender)
# data$Gender <- gsub("Female","1",data$Gender)
# data$Gender <- as.numeric(data$Gender)
# class(data$Gender)

# 2) Use a t-test and explain the results as well as your interpretation
data$Salary <- substr(data$Salary,2,7)
data$Salary <- gsub(",","",data$Salary)
data$Salary <- as.numeric(data$Salary)
class(data$Salary)

# data$Gender <- as.factor(data$Gender)
# data[,shapiro.test(Salary), Gender]
ansari.test(Salary ~ Gender, data)
t.test(Salary ~ Gender, data=data, var.equal=FALSE)
print("Base on the low p-value, we can reject the null hypothesis leading us to conclude that there is a significant difference in average salary between female employees and male employees.")

# 3) Transform EducLev, JobGrade, Gender, and PCJob into several dummy variables
edu_results <- fastDummies::dummy_cols(data$EducLev)
knitr::kable(edu_results)
jobgr_results <- fastDummies::dummy_cols(data$JobGrade)
knitr::kable(jobgr_results)
gen_results <- fastDummies::dummy_cols(data$Gender)
knitr::kable(gen_results)
pcj_results <- fastDummies::dummy_cols(data$PCJob)
knitr::kable(pcj_results)

# 4) Estimate a multiple regression model to strengthen/bolster the plaintiff’s justification, then write a report explaining your results.
require(stargazer)
data$EducLev <- as.factor(data$EducLev)
data$JobGrade <- as.factor(data$JobGrade)
data$PCJob <- as.factor(data$PCJob)
data$Gender <- as.factor(data$Gender)
result <- lm(Salary ~ EducLev+JobGrade+scale(YrsExper)+scale(Age)+Gender+scale(YrsPrior)+PCJob, data = data)
stargazer(result, type = "text")
summary(result)

require(jtools)
plot_summs(result)
print("R-squared is 0.765 and it means how good your regression model is.")
print("The meaning of the t-values is whether this variable have a significant effect on the target Y.")
print("The meaning of the coefficients is the effect of this variable holding all other explanatory variables constant.")

require(ggplot2)
plot_summs(result) + xlim(130, 150)

ggplot(data, aes(x=Salary, fill=Gender)) +
  geom_histogram() + 
  facet_wrap(.~Gender)

# 5) Do these data provide evidence that there is discrimination against female employees in terms of salary?
# Yes, because GenderMale is significant(**) which means that Gender have a affect on Salary and Male earns more money than Female.

# Extra Credit:
# a.
# Interaction terms may arise when considering the relationship among three or more variables, and describes a situation in which the effect of one causal variable on an outcome depends on the state of a second causal variable.
result2 <- lm(Salary ~ EducLev+JobGrade+scale(YrsExper)+scale(Age)+Gender+scale(YrsPrior)+PCJob+Gender*JobGrade, data = data)
stargazer(result2, type = "text")
summary(result2)
plot_summs(result2)
# I choose Gender and JobGrade to see whether their interaction will make some difference compare to the original model
# This result show that if you are Male and have the highest JobGrade, you'll earn more money. 
# b.
# After putting this interaction to my model, I found that the coefficient of Gender*JobGrade is higher than any other variable!
# BTW, Gender variable is no longer significant which means it is JobGrade that makes it significant on the original model.








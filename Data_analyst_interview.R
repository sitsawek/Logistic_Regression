telco = read.csv("Telco.csv")
str(telco)
summary(telco)
#I try to understand data and see 7043 observation with 22 variables.
#Output is Churn.
# 1) What is current churn rate of this Telco company?
c = table(telco$Churn)
barplot(c/nrow(telco),main = "Current churn rate")
table(telco$Churn)
1869/(5171+1869)
# Ans = 26.54%
# 2) What is median of tenure of the customers?
summary(telco$tenure)
# Ans = Median is 29.

# 3) What is the difference in churn rate of female senior citizens who use Fibre Optic and DSL?
# All female senior citizens use as fc.
table(telco$SeniorCitizen)
fc = subset(telco,gender == "Female"& SeniorCitizen == "1")
dim(fc)
table(fc$InternetService)
# Female senior citizens use fibre optic as ff.
ff = subset(telco,gender == "Female"& SeniorCitizen == "1"&InternetService == "Fiber optic")
dim(ff)
# Female senior citizens use DSL as fd.
fd = subset(telco,gender == "Female"& SeniorCitizen == "1"&InternetService == "DSL")
dim(fd)
barplot(table(ff$Churn)/nrow(ff),main = "Female senior citizen with fiber optic")
table(ff$Churn)
199/(225+199)
#Churn rate of ff is 46.93%
barplot(table(fd$Churn)/nrow(fd),main = "Female senior citizen with DSL")
table(fd$Churn)
38/(85+38)
#Churn rate of fd is 30.89%
# Ans = Different is 16.04% (fiber opitic more than DSL)

# 4) Can Fiber optic customers opt out for phone service?
barplot(table(telco$PhoneService,telco$InternetService),main = "Internet service with phone service")
# No fiber optic still available with phone service. 

# 5) Which services do not have tech support?
barplot(table(telco$TechSupport,telco$PhoneService),main = "Tech support with phone service")
barplot(table(telco$TechSupport,telco$MultipleLines),main = "Tech support with multiple lines")
barplot(table(telco$TechSupport,telco$InternetService),main = "Tech support with internet service")
# Ans = No internet services are no tech support.

# 6) Is this statement true? And why do you think so?
#“The churn rate increases as the customers pay higher monthly charges”
pred_t1 = predict(mod1,type = "response")
pt1 = as.numeric(pred_t1)
hist(pt1)
str(pt1)
m = (train$MonthlyCharges)
cor(m,pt1)
plot(m,pt1,main = "Correlation between predict and monthly charges")

# Ans = It not true because correlation between prediction and monthly charge around 0.36 it quite low correlation.

# 7) Do you think additional services such as Online backup, Online Security, and tech support are important from customers’ perspective? Why do you think so?
barplot(table(telco$Churn,telco$OnlineBackup)/nrow(telco),main = "OnlineBackup")
table(telco$Churn,telco$OnlineBackup)/nrow(telco)
barplot(table(telco$Churn,telco$OnlineSecurity)/nrow(telco),main = "OnlineSecurity")
table(telco$Churn,telco$OnlineSecurity)/nrow(telco)
barplot(table(telco$Churn,telco$TechSupport)/nrow(telco),main = "TechSupport")
table(telco$Churn,telco$TechSupport)/nrow(telco)
# Ans From data we can see churn rate of no additional services more than proportion churn rate of have additional services.

# 8) Would you recommend the Telco company to target customers who are singles? Why do you think so?
p = subset(telco,Partner == "Yes")
np = subset(telco,Partner == "No")
barplot(table(p$Churn)/nrow(p),main = "With partner")
table(p$Churn)/nrow(p)
barplot(table(np$Churn)/nrow(np),main = "No partner")
table(np$Churn)/nrow(np)
# Ans = No because no partner are more churn rate than have partner.

# 9) Which service you think has serious problems and would recommend the executive of this Telco company to look into?
summary(mod1)
# Ans = Multiple service are significant with churn rate.

# 10) What features/attributes you think are the most predictive to churn behaviour? List the top 3 features with back up analysis.
summary(mod1)
# Ans = Most features predictive is tenure,Contract,TotalCharges.

#First i choose to split data before build model to check after build will model can generalize.
table(telco$SeniorCitizen,telco$InternetService)
library(caTools)
#And set seed to same answers in data.
set.seed(123)
split = sample.split(telco$Churn,SplitRatio = 0.7)
train = subset(telco,split == TRUE)
test = subset(telco,split == FALSE)
# I will delete customerID because it insignificant for compute in model.
nonvars = c("customerID")
train = train[,!(names(train)%in%nonvars)]
test = test[,!(names(test)%in%nonvars)]
# Delete missing values.
train = na.omit(train)
test = na.omit(test)
#Use logistic regresstion to build model 1 with all features.
mod1 = glm(Churn~.,data = train,family = binomial)
summary(mod1)
pred1 = predict(mod1,newdata = test,type = "response")
table(test$Churn,pred1 >= 0.5)
#Accuracy of mod1 is 79.99%
(1383+302)/(1383+164+259+302)
#Now i will build model 2 with features are most 3 significant.
mod2 = glm(Churn~tenure+Contract+TotalCharges,data = train,family = binomial)
summary(mod2)
pred2 = predict(mod2,newdata = test,type = "response")
table(test$Churn,pred2 >= 0.5)
(1326+290)/(1326+221+271+290)
#Accuracy of mod2 is 76.66%

#Find ROCR curve
library(ROCR)
pred_ROCR = prediction(pred2,test$Churn)
perf_ROCR = performance(pred_ROCR,"tpr","fpr")
plot(perf_ROCR,colorize = TRUE)
as.numeric(performance(pred_ROCR,"auc")@y.values)
# AUC is 0.80.
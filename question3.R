
"""
age gender height_cm weight_kg `body fat_%` diastolic systolic gripForce sit and bend forward…¹ `sit-ups counts`
"""


#install.packages("tidyverse")
#install.packages("ggcorrplot")
#install.packages("corrplot") 
#install.packages("caret") 
#install.packages("lattice")
#install.packages("ggplot2") 
#install.packages("stringi") 
#install.packages("lmtest")
library(tidyverse)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(lattice)
library(ggplot2)
library(caret)
library(lmtest)
"a) Create a data frame considering only the variables: body fat, grip force, sit ups counts, gender
and age categories. Consider the numerical variables in the data set and find the best simple
linear regression model to predict the broad jump variable. Test the assumptions and use
transformations if it is required. Explain why the model you find is the best simple linear
regression model and interpret the model coefficients. (2.5p)
"
filePath<- 'bodyPerformance.csv'
dataTypes<- cols( 
  'gender'= col_character()    ,
  'age'=col_double()           , 
  'body fat_%'=col_double()    , 
  'gripForce'=col_double()     ,   
  'sit-ups counts'=col_double(),
  'broad jump_cm'=col_double() , 
)
columns<-c('gender','age', 'body fat_%', 'gripForce', 'sit-ups counts', 'broad jump_cm' )
df <- read_csv(filePath ,col_types = dataTypes, col_select = columns)
df
variablesToExamine <- df %>% select('broad jump_cm', 'body fat_%', 'gripForce' , 'sit-ups counts', )
cor_matrix <- cor(variablesToExamine) 
corrplot(cor_matrix, method = "color")
cor_matrix

fat  <- lm(`broad jump_cm` ~ `body fat_%`, data = df)
grip <- lm(`broad jump_cm` ~  `gripForce`, data = df)
sit  <- lm(`broad jump_cm` ~ `sit-ups counts`, data = df)
summary(fat)
summary(grip)
summary(sit)
abline(fat, col ="red", lwd =2)
abline(grip, col ="green", lwd =2)
abline(sit, col ="blue", lwd =2)
"
  ASSUMPTIONS
  Normality of the Error Term 
"
"
Lineality
"
plot(fat)
plot(grip)
plot(sit)
fatRes <- residuals(fat)
gripRes <- residuals(grip)
sitRes <- residuals(sit)
hist(residuals(fat))
hist(residuals(grip))
hist(residuals(sit))
#Assumption 1 Normality of the Error Term 
qqnorm(residuals(fat)) 
qqnorm(residuals(grip)) 
qqnorm(residuals(sit)) 
#Assumption 2 Homogeneity of Variance 
plot(residuals(fat))
plot(residuals(grip))
plot(residuals(sit))
# Breusch Pagan Test 
bptest(fat)
bptest(grip)
bptest(sit)
#Assumption 3  The independence of errors ### 
dwtest(fat, alternative = "two.sided")
dwtest(grip, alternative = "two.sided")
dwtest(sit, alternative = "two.sided")
confint(fat)
confint(grip)
confint(sit)

"b) Fit a multiple linear regression model with the most important two (numerical) variables. Use
transformations if it is needed and test all the assumptions. Then compare this model to the
simple linear regression model that you fit in (a). Which one is a better model? Why? (2.5p)"


multiModel  <- lm(`broad jump_cm` ~ `sit-ups counts` + `gripForce`, data = df)

" ASSUMPTIONS 1
  Normality of the Error Term Normality of the Error Term
"
#Linearity
plot(multiModel)
qqnorm(residuals(multiModel)) 
summary (multiModel)
# Assumption 2 Homogeneity of Variance  
plot(residuals(multiModel))
#Breusch-Pagan test 
bptest(multiModel)

#Assumption 3  The independence of errors ### 
dwtest(multiModel, alternative = "two.sided")
confint(multiModel)
"
c) Now we will add a factor to the regression model you have chosen in section (b)
i) First, add age categories (age_cat) as a factor. Interpret the model coefficients and
overall summary of the model.
"
df$age_cat <- cut(df$age,
                  breaks = c(0, 30, 40, 50, Inf),
                  labels = c("<30", "30-39", "40-49", "50+"),
                  right = FALSE
                  )
model_age <- lm(`broad jump_cm` ~ `sit-ups counts` + `gripForce` + `age_cat`, data = df)
summary(model_age)  
"
ii) Add gender to the model that you had chosen in the previous step in (b). Interpret the
model coefficients and overall summary of this model similar to the previous case. (3p)"

df$gender_cat <- as.factor(df$gender)
levels(df$gender_cat)
genderMultiModel <- lm(`broad jump_cm` ~ `sit-ups counts`+ `gripForce` + gender_cat, data = df)
summary(genderMultiModel)

"
d)Compare the model in section (b) with the two models that had constructed with an additional
factor in (c). Which one would you choose? Why? (1p)
"
summary(multiModel)
summary(genderMultiModel)
summary(model_age)
anova(multiModel, model_age)
anova(multiModel, genderMultiModel)
"
e)Test the validity of the final model that you choose. (1p)
"
#Lineality
plot(genderMultiModel)
#Normality of the error 
qqnorm(residuals(fat)) 
#Homogeniety of variance
bptest(genderMultiModel)
#Independence of errors
dwtest(genderMultiModel, alternative = "two.sided")
summary(genderMultiModel)
# Testing 
set.seed(500)
n <- nrow(df)
trainSample<-sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[trainSample, ]
test_data  <- df[-trainSample, ]
train_model <- lm(`broad jump_cm` ~ `sit-ups counts`+ `gripForce` + gender_cat, data = train_data)
summary(train_model)

?predict
yhat<-predict(train_model, test_data, interval="prediction")
yhat
y<-test_data$`broad jump_cm`
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)
mse
RMSE<-sqrt(mse)
RMSE
### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test_data)))
RMSE1

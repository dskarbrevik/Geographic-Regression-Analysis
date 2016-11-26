#Name: Data exploration for w271 Final Project.
#Date: November 2, 2016
#Author: Nick Chen

library(xlsx)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)

#Load the data
setwd('~/Desktop/UC Berkeley/Applied Regression and Time Series Analysis/Lab 3/Health and Diet Data/')
getwd()

diet.data <- read.csv("diet-forcsv - Sheet 1.csv")

setwd('~/Desktop/UC Berkeley/Applied Regression and Time Series Analysis/Lab 3/Data Validation/')
getwd()

data.validation.country.mapping <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Country_Mapping")
data.validation.life_expect <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Life Expectancy")
data.validation.growth_rate <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Population Growth Rate")

#*************************************
#Univariate EDA

#Wine consumption
#Summary statistics for variablees of interest
summary(diet.data$Wine..kcal.day.)
sum(diet.data$Wine..kcal.day. == 0)

#There are 32 countries with zero wine consumption.
wine.hist <- ggplot(data = diet.data, aes(x = Wine..kcal.day.))
wine.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Wine Calories per Day") + labs(y = "Number of Countries")

#life expectancy
summary(diet.data$Life.expectancy.at.birth..years..both.sexes)

#The life expectancy variable shows a negative skew.
life.expect.all.hist <- ggplot(data = diet.data, aes(x = Life.expectancy.at.birth..years..both.sexes))
life.expect.all.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Life Expectancy at Birth") + labs(y = "Number of Countries")


#Alcoholic beverages calories per day
summary(diet.data$Alcoholic.Beverages..kcal.day.)
sum(diet.data$Alcoholic.Beverages..kcal.day. < 0)

#Like wine, there are a lot of countries with zero or very little consumption of alcoholic beverages.
Alcoholic.bevs.cals.hist <- ggplot(data = diet.data, aes(x = Alcoholic.Beverages..kcal.day.))
Alcoholic.bevs.cals.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Alchoholic Beverages Calories per Day") + labs(y = "Number of Countries")

#GNP per capita
summary(diet.data$Gross.national.income.per.capita..PPP.international...)

#GNP histogram
GNP.hist <- ggplot(data = diet.data, aes(x = Gross.national.income.per.capita..PPP.international...))
GNP.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of GNP") + labs(y = "Number of Countries")


#Like wine, there are a lot of countries with zero or very little consumption of alcoholic beverages.
Alcoholic.bevs.cals.hist <- ggplot(data = diet.data, aes(x = Alcoholic.Beverages..kcal.day.))
Alcoholic.bevs.cals.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Alchoholic Beverages Calories per Day") + labs(y = "Number of Countries")


#*************************************
#Multivariate EDA

#Correlations
#correlation between wine consumption and alcoholic beverage consumption per day and life expectancy at birth is quite large at .496.
cor(diet.data$Wine..kcal.day., diet.data$Life.expectancy.at.birth..years..both.sexes)
cor(diet.data$Alcoholic.Beverages..kcal.day., diet.data$Life.expectancy.at.birth..years..both.sexes)

#look at correlation between wine / alcohol consumption and GNP to see if the above result appears to be a result of income
#There are very high correlations between wine and alcohol consumption with GNP, both being above 0.6.
cor(diet.data$Gross.national.income.per.capita..PPP.international..., diet.data$Wine..kcal.day.)
cor(diet.data$Gross.national.income.per.capita..PPP.international..., diet.data$Alcoholic.Beverages..kcal.day.)

wine.gnp.scatter <- ggplot(data = diet.data, aes(x = Gross.national.income.per.capita..PPP.international..., y = Wine..kcal.day.))
wine.gnp.scatter + geom_point(colour = "navy") + ggtitle("Scatterplot of GNP and Wine Consumption per Day")


#*************************************
#Data validation
#Merge country code with validation datasets.
data.validation.growth_rate <- merge(data.validation.growth_rate, data.validation.country.mapping[,c("Population.Growth.Rate", "Country_main_dataset")], by.x = "Country.Name", by.y = "Population.Growth.Rate")
data.validation.life_expect <- merge(data.validation.life_expect, data.validation.country.mapping[,c("Country_Life_Expectancy", "Country_main_dataset")], by.x = "Country", by.y = "Country_Life_Expectancy")

#Merge validating data into the main country dataset.
diet.data <- merge(diet.data, data.validation.growth_rate[,c("Country_main_dataset", "Growth_rate_2000", "Growth_rate_2005", "Growth_rate_2010")], by.x = "Countries", by.y = "Country_main_dataset")
diet.data <- merge(diet.data, data.validation.life_expect[,c("Country_main_dataset", "Life_Expectancy")], by.x = "Countries", by.y = "Country_main_dataset")

#Now compare data validation sources to main dataset
#Life expectancy
diet.data$Life_Expectancy_pct_diff <- (diet.data$Life.expectancy.at.birth..years..both.sexes - diet.data$Life_Expectancy) / diet.data$Life.expectancy.at.birth..years..both.sexes

summary(diet.data$Life_Expectancy_pct_diff)
hist(diet.data$Life_Expectancy, main = "Data Validation Life Expectancy Distribution")
hist(diet.data$Life.expectancy.at.birth..years..both.sexes, main = "Data Validation Original Life Expectancy Distribution")
hist(diet.data$Life_Expectancy_pct_diff, main = "Percent Difference Life Expectancy")

#Life expectancy in the original dataset appears to be systematically lower than the 2016 life expectancies downloaded from the CIA factbook.
#This makes sense given that we believe the life expectancy in the original data to be from an earlier period, likely 2000 - 2005 based on the other variables, and that we expect life expectancy to increase over time.
Growth.rate.examination <- diet.data[,c("Countries", "Growth_rate_2000", "Growth_rate_2005", "Growth_rate_2010", "Population.annual.growth.rate....")]

Growth.rate.examination$Growth_rate_pct_diff_2000 <- (Growth.rate.examination$Population.annual.growth.rate.... - Growth.rate.examination$Growth_rate_2000) / Growth.rate.examination$Population.annual.growth.rate....
Growth.rate.examination$Growth_rate_pct_diff_2005 <- (Growth.rate.examination$Population.annual.growth.rate.... - Growth.rate.examination$Growth_rate_2005) / Growth.rate.examination$Population.annual.growth.rate....
Growth.rate.examination$Growth_rate_pct_diff_2010 <- (Growth.rate.examination$Population.annual.growth.rate.... - Growth.rate.examination$Growth_rate_2010) / Growth.rate.examination$Population.annual.growth.rate....

#Summary statistics of each growth rate
summary(Growth.rate.examination$Population.annual.growth.rate....)
summary(Growth.rate.examination$Growth_rate_2000)
summary(Growth.rate.examination$Growth_rate_2005)
summary(Growth.rate.examination$Growth_rate_2010)

#Histograms of percent difference with each known year growth rate
summary(Growth.rate.examination$Growth_rate_pct_diff_2000)
hist(Growth.rate.examination$Growth_rate_pct_diff_2000, main = "Histogram of Growth Rate % Diff with 2000 Growth Rate")
hist(Growth.rate.examination$Growth_rate_pct_diff_2005, main = "Histogram of Growth Rate % Diff with 2005 Growth Rate")
hist(Growth.rate.examination$Growth_rate_pct_diff_2010, main = "Histogram of Growth Rate % Diff with 2010 Growth Rate")

#Histograms of each growth rate
hist(Growth.rate.examination$Population.annual.growth.rate...., main = "Histogram of Original Growth Rate")
hist(Growth.rate.examination$Growth_rate_2000, main = "Histogram of 2000 Growth Rate")
hist(Growth.rate.examination$Growth_rate_2005, main = "Histogram of 2005 Growth Rate")
hist(Growth.rate.examination$Growth_rate_2010, main = "Histogram of 2010 Growth Rate")

#Correlation between main dataset growth rate and year 2000 growth rate 
cor(Growth.rate.examination$Population.annual.growth.rate...., Growth.rate.examination$Growth_rate_2010)

#The population growth rate distribution from the original dataset looks the most similar to the 2000 population growth rate.
#This makes sense and is a good sign of data validation given that other variables appear to be measures of this time period.

#***************************
#Model building - Test H0: Average daily Wine / Alcohol consumption has no impact on life expectancy.


#Model 1 - parsimonious model - life expectancy ~ wine
#Start with a simple linear regression and build up from there comparing models along the way.
wine.model.1 <- lm(Life.expectancy.at.birth..years..both.sexes ~ Wine..kcal.day., data = diet.data)
summary(wine.model.1)

plot(wine.model.1)

bptest(wine.model.1)
durbinWatsonTest(wine.model.1)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(wine.model.1, vcov = vcovHC)

#Comment on parsimonious model.
#The first model shows that wine consumption at the country level has quite a strong relationship with life expectancy.
#The coefficient estimate for wine is .195 which is statistically significant at the p < .001 level. The statistical significance of the estimate holds when heteroskedasticity robust standard errors are used.
#The wine consumption variable is measured in calories, so the interpretation of this coefficient is that one additional calorie of wine consumption per day across the population is associated with a 0.19 year increase in life expectancy.
#A glass of wine has about 120 calories so this coeficcient indicates that on average a population that drinks one additional glass of wine per day is expected to have a life expectancy of about 22.8 years greater, all else equal.
#However, this interpretation relies on the assumption that there is a linear relationship between average wine consumption and population life expectancy which may or may not be true.
#The diagnostic residuals vs. fitted values plot shows that heteroskedasticity may be a problem. Part of this result is caused by the fact that there are so many countries where average wine consumption is zero.
#As a result, we may want to use the generalized alcohol consumption variable that has fewer observations of zero.
#The Breusch pagan test confirms that heteroskedasticity of errors is borderline problematic.
#The Durbin Watson test also gives a statistically significant result which means we should reject the null hypothesis of the test that the errors are not correlated. This is a bit of a strange result that we may want to look into further.
#Our theoretical foundation could also support the use of the generalized alcohol consumption variable as the main independent variable in the model as it may be able to extend our hypothesis to cultures where wine consumption is not common, but instead other alcoholic beverages are consumed at group meals.   


#Model 2 - parsimonious model using alcohol consumption- life expectancy ~ alcohol
alc.model.1 <- lm(Life.expectancy.at.birth..years..both.sexes ~ Alcoholic.Beverages..kcal.day., data = diet.data)
summary(alc.model.1)

plot(alc.model.1)

bptest(alc.model.1)
durbinWatsonTest(alc.model.1)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.1, vcov = vcovHC)

#Comment on the second parsimonious model.
#The coefficient estimate for alcohol consumption is .065 indicating that for a country where average daily alcohol consumption across the population is 1 calorie higher is expected to have a higher life expectancy by .065 years, holding all else equal.
#This coefficient is statistically significant at p < .001 level using heteroskedasticity robust errors.
#Again, the diagnostic residuals vs. fitted values plot shows that heteroskedasticity may continue be a problem.
#The Breusch-Pagan test however yields a non-statistically significant result which means that we fail to reject the null hypothesis that the variance of the errors is stable across levels of fitted values.
#The Durbin-Watson test again shows that the errors are correlated. We should be sure to keep an eye on this after adding controls to the model.


#Model 3 - alcohol consumption with control for GNP - life expectancy ~ alcohol + GNP
alc.model.2 <- lm(Life.expectancy.at.birth..years..both.sexes ~ Alcoholic.Beverages..kcal.day. + Gross.national.income.per.capita..PPP.international..., data = diet.data)
summary(alc.model.2)

plot(alc.model.2)

bptest(alc.model.2)
durbinWatsonTest(alc.model.2)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.2, vcov = vcovHC)

#Comment on the model including a wealth control.
#This model drastically changes the impact of alcoholic beverage consumption on life expectancy.
#The coefficient estimate of the impact of alcoholic beverage consumption on life expectancy decreases to .006 and is no longer statistically significant.
#Heteroskedasticity of errors and serial correlation continu to be a problem.
#The residuals vs. fitted plot also seems to show a violation of the zero conditional mean assumption.
#The presence of heteroskedasticity of errors and a violation of the zero conditional mean assumption may indicate a non-linear relationship in the population.


#Model 4 - non-linear alcohol consumption with control for GNP - life expectancy ~ alcohol^2 + alcohol + GNP
alc.model.3 <- lm(Life.expectancy.at.birth..years..both.sexes ~ I(Alcoholic.Beverages..kcal.day.^2) + Alcoholic.Beverages..kcal.day. + Gross.national.income.per.capita..PPP.international..., data = diet.data)
summary(alc.model.3)

plot(alc.model.3)

bptest(alc.model.3)
durbinWatsonTest(alc.model.3)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.3, vcov = vcovHC)

#Comment on model including non-linear effect of alcohol consumption
#Including a non-linear effect of alcohol consumption in the model does not improve the problems with heteroskedasticity and serial correlation of errors.
#The alcoholic beverage consumption coefficient estimates are still not significant.
#The residuals vs. fitted values plot shows heteroskedasticity of errors and the Breusch-Pagan test confirms the errors are heteroskedastic.
#Therefore, we need to be sure to use heteroskedasticity robust standard errors to assess statistical significance of the coefficient estimates in the model.
#The Durbin-Watson test shows that serial correlation remains a problem

##NOTE FOR NEXT TEAM MEMBER TO PICK UP ANALYSIS - WHAT DO DO ABOUT SERIAL CORRELATION.


#Model 5 - log transformation of alcohol consumption with control for GNP - life expectancy ~ log(alcohol) + GNP

#First, remove observations of zero alcoholic beverage consumption so can implement a log transformation.
diet.data.2 <- diet.data[diet.data$Alcoholic.Beverages..kcal.day. > 0, ]

#Estimate the model
alc.model.4 <- lm(Life.expectancy.at.birth..years..both.sexes ~ log(Alcoholic.Beverages..kcal.day.) + Gross.national.income.per.capita..PPP.international..., data = diet.data.2)
summary(alc.model.4)

plot(alc.model.4)

bptest(alc.model.4)
durbinWatsonTest(alc.model.4)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.4, vcov = vcovHC)

#Comment on the model
#Including a log transformation for alcoholic beverage consumption does not fix the problem of heteroskedasticity of errors as evidenced by the residuals vs. fitted values plot and the Breusch Pagan test.
#The Durbin Watson test also shows that serial correlation of errors remains a problem.


#Model 6 - log transformation of alcohol consumption with control for log transformation of GNP - life expectancy ~ log(alcohol) + log(GNP)

#Estimate the model
alc.model.5 <- lm(Life.expectancy.at.birth..years..both.sexes ~ log(Alcoholic.Beverages..kcal.day.) + log(Gross.national.income.per.capita..PPP.international...), data = diet.data.2)
summary(alc.model.5)

plot(alc.model.5)

bptest(alc.model.5)
durbinWatsonTest(alc.model.5)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.5, vcov = vcovHC)

#Comment on the model.
#Using a log transformation on GNP and alcohol consumption makes sense because each of these variables is positively skewed.
#After making these transformations, the Durbin-Watson test shows that serially correlated errors seems to have been solved.
#The residuals vs. fitted values plot shows that heteroskedasticity of errors continues to be a problem. The Breusch-Pagan test confirms this result.
#Using heteroskedasticity robust standard errors, the coefficients are both statistically significant.
#The coefficient estimate on log(alcohol consumption) is -1.271 which is statistically significant (p = .012 using heteroskedasticity robust errors).
#The interpretation of this coefficient is that a one percent increase in alcohol consumption corresponds with a decrease of 1.271 years in life expectancy while holding GNP equal.

























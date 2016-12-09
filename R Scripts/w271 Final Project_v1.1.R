#Name: Data exploration for w271 Final Project.
#Date: November 2, 2016
#Author: Nick Chen, Johnny Yeo, Rama Thamman, David Skarbrevik

library(xlsx)
library(ggplot2)
library(lmtest)
library(car)
library(sandwich)

#Research Question:
# Do people who live in cultures that drink more wine and eat more dessert live longer lives on average?

# Perhaps a better question: Is alcohol consumption a valid proxy/indirect indicator of health in a country? 

# Justification of using our dataset: Because our data has many indicators of health (GNP, life expectancy, obesity, calorie intake, etc.) as well as data on the amount of aclohol consumption of many countries, this is a great dataset to test if alcohol consumption is a valid indirect proxy for health in the way that GNP is.

### Dataset Used: World Nutrition Data
#.	Dataset has one "observation per country, but more than 50 countries 
#.	We don't have a lot of information on how this data was acquired, but a way to validate it is by crosschecking with another more documented dataset 
#(http://data.worldbank.org/datacatalog/Healthnutritionandpopulationstatistics). 
#.	Check that total calorie consumption is greater than or equal to the sum of calories in all of the individual food categories.

##Motivation/Thoughts about this research: 
#.	Some research has seemed to show that moderate wine consumption seems to be associated with a lower incidence of heart disease and potentially 
#longer life spans (http://www.telegraph.co.uk/news/health/11066516/Glassofwinewithdinnerhelpsyoulivelongerscientistsclaim.html).                                                                                                                                               

#.	Critiques of this research are that the wine's effect on health outcomes is not a causal effect, but instead that the true underlying effect is more related to stronger social ties and wine consumption is simply associated with having a strong social network and taking more meals with family and friends.

#.	The idea behind the main research question is to investigate the idea that cultures where people take more meals with family and friends have better health outcomes.

#.	We will use wine consumption and potentially sugar consumption in some form to serve as proxy variables for eating more family meals or meals with friends.
#.	The idea behind this is that you are more likely to drink wine or eat dessert with your meal when you are eating with friends or family.
#.	Other research has indicated that strong social networks are an important factor in living a healthy life (http://uncnews.unc.edu/2016/01/04/socialnetworksasimportantasexerciseanddietacrossthespanofourlives/).


##Exploratory Data Analysis, Domain Research, and Potential Model
#.	Look at correlations between wine / alcohol and sugar consumption and life expectancy
#.	Check that the data makes sense 
#.	Will want to consider the possibility of nonlinear effects of wine / alcohol and sugar consumption on life expectancy at birth
#.	Control for obesity prevalence
#.	Consider interaction term between wine / alcohol consumption and sugar consumption
#.	Perhaps one or the other could have a negative effect individually, but moderate consumption of both may be an even better proxy for taking more meals with family and friends than moderate consumption of one or the other




#Load the data
#setwd('~/Desktop/UC Berkeley/Applied Regression and Time Series Analysis/Lab 3/Health and Diet Data/')
#setwd('C:/Users/rthamman/Dropbox (Personal)/Berkeley/Courses/W271/Labs/Lab 3/Git/Data')
#setwd('~/Documents/MIDS/w271/w271_final_proj/W271_Lab3/Data')
#getwd()

diet.data <- read.csv("diet-forcsv - Sheet 1.csv")

data.validation.country.mapping <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Country_Mapping")
data.validation.life_expect <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Life Expectancy")
data.validation.growth_rate <- read.xlsx("Data from Third Parties for Validation.xlsx", sheetName = "Population Growth Rate")

#*************************************

#Missing values check
na.check = sapply(diet.data, function(x) sum(is.na(x))) # check specifically for NA values
if(sum(na.check) == 0) 
{
        cat("No NA values in this data.")
} else {
na.check
cat("There are a total of", sum(na.check), "NAs in the data.")
}

cat("Number of rows: ",nrow(diet.data))
cat("Number of complete cases: ",nrow(diet.data[complete.cases(diet.data),]))

#There are no missing values

#Univariate EDA

#Wine consumption
#Summary statistics for variables of interest
summary(diet.data$Wine..kcal.day.)
sum(diet.data$Wine..kcal.day. == 0)

#There are 32 countries with zero wine consumption. This could be because of bottom coding to cover for null values.
wine.hist <- ggplot(data = diet.data, aes(x = Wine..kcal.day.))
wine.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Wine Calories per Day") + labs(y = "Number of Countries")

#life expectancy
summary(diet.data$Life.expectancy.at.birth..years..both.sexes)

#The life expectancy variable shows a negative skew (because no one lives to be 160).
life.expect.all.hist <- ggplot(data = diet.data, aes(x = Life.expectancy.at.birth..years..both.sexes))
life.expect.all.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Life Expectancy at Birth") + labs(y = "Number of Countries")


#Alcoholic beverages calories per day
summary(diet.data$Alcoholic.Beverages..kcal.day.)
sum(diet.data$Alcoholic.Beverages..kcal.day. == 0)

#Like wine, there are a lot of countries with zero or very little consumption of alcoholic beverages.
Alcoholic.bevs.cals.hist <- ggplot(data = diet.data, aes(x = Alcoholic.Beverages..kcal.day.))
Alcoholic.bevs.cals.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of Alchoholic Beverages Calories per Day") + labs(y = "Number of Countries")

#GNP per capita
summary(diet.data$Gross.national.income.per.capita..PPP.international...)

#GNP histogram
GNP.hist <- ggplot(data = diet.data, aes(x = Gross.national.income.per.capita..PPP.international...))
GNP.hist + geom_histogram(fill = "navy", colour = "white") + ggtitle("Histogram of GNP") + labs(y = "Number of Countries")

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

#diet.data$Alcoholic.Beverages..kcal.day. > 0
wine.gnp.scatter <- ggplot(data = diet.data, aes(x = Gross.national.income.per.capita..PPP.international..., y = Wine..kcal.day.))
wine.gnp.scatter + geom_point(colour = "navy") + ggtitle("Scatterplot of GNP and Wine Consumption per Day")

#further analysis of correlation between wine / alcohol consumption and life expectancy at birth
i = 52
wine.box <- boxplot(diet.data[i], main = "Boxplot of Wine Consumtion (kcal/day)")
df <- cbind(diet.data[i], diet.data$Countries, diet.data$Life.expectancy.at.birth..years..both.sexes)
names(df) <- c("wine_consumption", "countries", "life_expectancy")
ordered_df <- df[order(df[1]),]
ordered_df[ordered_df$wine_consumption > wine.box$stats[5],] 

#Given the boxplot, these are the countries with "outlier-level" wine consumption, and their life expectancy.
#Every country with high wine consumption has a life expectancy of over 70.
#It is important to also notice, however, that all of these countries (minus Argentina) are a part of Europe,
#where wine consumption is on average higher than the rest of the world. 
#Given these results, despite the high correlation, it's hard to tell whether we see any good indication that greater wine consumption leads to longer life.

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

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan test has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
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
#Despite the statistically significant coefficient estimate, there is by no means any evidence of any casual relationship between wine consumption and life expectancy at this point. 
#It is interesting to see that there is a relationship of some sort between the two variables, but this could be just a result of two variables affected caused by a third variable, or simply a phenomena due to chance, or any other reasonable scenario that can be thought up at this point.

#Model 1.1 - Sensitivity analysis - Healthy life expectancy ~ wine 
#This analysis is to test if Healthy life expectancy is a proxy for Life expectancy
#There is a high correlation between Healthy life expectancy and Life expectance at birth
cor(diet.data$Healthy.life.expectancy..HALE..at.birth..years..both.sexes, diet.data$Life.expectancy.at.birth..years..both.sexes)

#Start with a simple linear regression and build up from there comparing models along the way.
wine.model.1.1 <- lm(diet.data$Healthy.life.expectancy..HALE..at.birth..years..both.sexes ~ Wine..kcal.day., data = diet.data)
summary(wine.model.1.1)
plot(wine.model.1.1)
bptest(wine.model.1.1)
durbinWatsonTest(wine.model.1.1)

#Comment on using Healthy life expectancy instead of Life expectancy
#Outcome of the analysis is very similar to Model #1. This validates the data Healty life expectancy and Life expectancy are consistent.

#Model 2 - parsimonious model using alcohol consumption- life expectancy ~ alcohol
alc.model.1 <- lm(Life.expectancy.at.birth..years..both.sexes ~ Alcoholic.Beverages..kcal.day., data = diet.data)
summary(alc.model.1)

plot(alc.model.1)

bptest(alc.model.1)
durbinWatsonTest(alc.model.1)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan test has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
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
#Heteroskedasticity of errors and correlation continue to be a problem.
#The residuals vs. fitted plot also seems to show a violation of the zero conditional mean assumption.
#The presence of heteroskedasticity of errors and a violation of the zero conditional mean assumption may indicate a non-linear relationship in the population.
#Including wealth as a control seems to have pulled away what had seemed to be a strong linear relationship between alcohol consumption and life expectancy. 
#This is a reasonable result, as it seems that wealth would be a key driver for both alcohol consumption and life expectancy.
#Adding the wealth control, therefore, reveals that alcohol consumption in an of itself may not be as strongly relate with life expectancy as previously suspected.


#Model 4 - non-linear alcohol consumption with control for GNP - life expectancy ~ alcohol^2 + alcohol + GNP
alc.model.3 <- lm(Life.expectancy.at.birth..years..both.sexes ~ I(Alcoholic.Beverages..kcal.day.^2) + Alcoholic.Beverages..kcal.day. + Gross.national.income.per.capita..PPP.international..., data = diet.data)
summary(alc.model.3)

plot(alc.model.3)

bptest(alc.model.3)
durbinWatsonTest(alc.model.3)

#Look at coefficient estimates with heteroskedasticity robust standard errors because the Breusch-Pagan teset has a marginally significant result suggesting that heteroskedasticity of errors may be a problem..
coeftest(alc.model.3, vcov = vcovHC)

#Comment on model including non-linear effect of alcohol consumption
#Including a non-linear effect of alcohol consumption in the model does not improve the problems with heteroskedasticity and correlation of errors.
#The alcoholic beverage consumption coefficient estimates are still not significant.
#The residuals vs. fitted values plot shows heteroskedasticity of errors and the Breusch-Pagan test confirms the errors are heteroskedastic.
#Therefore, we need to be sure to use heteroskedasticity robust standard errors to assess statistical significance of the coefficient estimates in the model.
#The Durbin-Watson test shows that correlation remains a problem

##NOTE FOR NEXT TEAM MEMBER TO PICK UP ANALYSIS - WHAT TO DO ABOUT CORRELATION.


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
#The Durbin Watson test also shows that correlation of errors remains a problem.


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
#After making these transformations, the Durbin-Watson test shows that correlated errors seems to have been solved.
#The residuals vs. fitted values plot shows that heteroskedasticity of errors continues to be a problem. The Breusch-Pagan test confirms this result.
#Using heteroskedasticity robust standard errors, the coefficients are both statistically significant.
#The coefficient estimate on log(alcohol consumption) is -1.271 which is statistically significant (p = .012 using heteroskedasticity robust errors).
#The interpretation of this coefficient is that a one percent increase in alcohol consumption corresponds with a decrease of 1.271 years in life expectancy while holding GNP equal.
#While this model outputs a statistically significant coefficient estimate of alcohol consumption, now its relationship with life expectancy is reversed, making it fairly suspect that there is a real meaningful relationship between the two variables. 
#In contrast, the wealth control, the GNP, still retained a similar relationship with life expectancy, which is consistent with its coefficient estimates in previous models.


#Conclusion

#TBD

# Pros

# TBD

#Cons:

# Data Collection Constraints:

#.	Health expectancy estimates based on selfreported health status information are generally not comparable across countries due to differences in survey instruments 
#and cultural differences in reporting of health. 

#.	Comparability problems with self-report health status data relate not only to differences in survey design and methods, but more fundamentally to
#unmeasured differences in expectations and norms for health references

#.	The meaning that different populations attach to the labels used for each of the response categories, such as mild, moderate or severe, in self-reported questions can vary greatly.

#.	Calculation of healthy life expectancy at birth is based on age-specific death rates for a particular calendar period together with severity-adjusted health state prevalences by age. 

# Data Collection Constraint Mitigation:

#.	To mitigate the risk, data is validated against another datasource (http://data.worldbank.org/data-catalog/health-nutrition-and-population-statistics). Analysis is outlined in Data Validation Section above.


# Control Variables:

#.	We expect positive linear relationship between wine consumption and life expectancy only to a certain extent, beyond that there will be other negative implications. 
#We need a control variable to balance that out. For example, a variable that captures negative impact on life expectancy when more calories are consumed.

#create interesting subset to data
DavidSubset = diet.data[, c("Countries", "Alcoholic.Beverages..kcal.day.", "Gross.national.income.per.capita..PPP.international...", "Life.expectancy.at.birth..years..both.sexes", "Systolic.blood.pressure..adults.aged.15.and.above..men..mmHg.", "Obesity.prevalence..men....", "Mean.total.cholesterol..men..mg.dl...2005")]
summary(DavidSubset)
colnames(DavidSubset) = c("Countries", "alcohol_consumption", "GNP_capita", "life_expectancy", "blood_pressure", "obesity_pcnt", "cholesterol_mean_total")

hist(DavidSubset$cholesterol_mean_total, breaks=30)
hist(DavidSubset$blood_pressure, breaks=30)
hist(DavidSubset$obesity_pcnt, breaks=30)


cor(DavidSubset$alcohol_consumption, DavidSubset$GNP_capita)

alcohol.gnp.scatter <- ggplot(data = DavidSubset, aes(x = GNP_capita, y = alcohol_consumption))
alcohol.gnp.scatter + geom_point(colour = "navy") + ggtitle("Scatterplot of GNP and Alcohol Consumption per Day")
DavidSubset

# added a few health indicator variables
davidmodel = lm(life_expectancy ~ GNP_capita + blood_pressure + obesity_pcnt + alcohol_consumption + cholesterol_mean_total, data = DavidSubset)
summary(davidmodel)

# Alcohol is strongly linked to GNP but strongly not linked to life expectancy... which is interesting.
# Alcohol consumption may be a good proxy for being a wealthy country, thus it is a good indication of healthy life or life expectancy. 
# This is maybe similar to the idea of looking at the size of a country's entertainment industry as a proxy for its health/success/happiness.

 





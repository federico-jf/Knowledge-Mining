# EPPS 6323: Knowledge Mining
# Final Project Knowledge Mining
# Dr. Karl Ho
# Student: Federico Ferrero

# clear your memory
rm(list=ls())

# set your working directory path
setwd("C:/Users/feder/Desktop/km_project")

############################################
############################################
############################################
############################################
############################################
###### ARGENTINE APRENDER EVALUATION #######
############################################
############################################
############################################
############################################
############################################


# Reading in the data
mydata <- read.delim("/Users/feder/Desktop/km_project/aprender_cordoba_dataset.txt")

# names of variables
names(mydata)

# Performance by Sector and Ambit
par(mfrow=c(2, 2))
boxplot(ldesemp~sector,data=mydata, main="Language Performance by Sector", sub="1= Public  2= Private",
        xlab="Sector", ylab="Language Performance", col="orange")

boxplot(ldesemp~ambito,data=mydata, main="Language Performance by Ambit", sub="1= Urban  2= Rural",
        xlab="Ambit", ylab="Language Performance", col="orange")

boxplot(mdesemp~sector,data=mydata, main="Math Performance by Sector", sub="1= Public  2= Private",
        xlab="Sector", ylab="Math Performance", col="lightblue")

boxplot(mdesemp~ambito,data=mydata, main="Math Performance by Ambit", sub="1= Urban  2= Rural",
        xlab="Ambit", ylab="Math Performance", col="lightblue")

dev.off()

# Performance by Gender
par(mfrow=c(1, 2))

counts1 <- table(mydata$ldesemp, mydata$gender)
barplot(counts1, main="Languaje Performance Level by Gender", sub="1= Male 2= Female 3= Other",
        xlab="Gender", col=c("red","orange","lightblue","forestgreen"))

counts2 <- table(mydata$mdesemp, mydata$gender)
barplot(counts2, main="Math Performance Level by Gender", sub="1= Male 2= Female 3= Other",
        xlab="Gender", col=c("red","orange","lightblue","forestgreen"))
        
dev.off()

# Performance by School Repetition and Students Who Work
par(mfrow=c(2, 2))
boxplot(ldesemp~repitencia_dicotomica,data=mydata, main="Language Performance by School Repetition", sub="1= Repeated School Grade  2= Non Repeated School Grade  3= No answer",
        xlab="School Repetition", ylab="Language Performance", col="pink")

boxplot(mdesemp~repitencia_dicotomica,data=mydata, main="Math Performance by School Repetition", sub="1= Repeated School Grade  2= Non Repeated School Grade  3= No answer",
        xlab="School Repetition", ylab="Math Performance", col="pink")

boxplot(ldesemp~trabaja_fuera_hogar,data=mydata, main="Language Performance by Students Who Work", sub="1= Yes  2= No  3= No answer",
        xlab="Students Who Work", ylab="Language Performance", col="darkkhaki")

boxplot(mdesemp~trabaja_fuera_hogar,data=mydata, main="Math Performance by Students Who Work", sub="1= Yes  2= No  3= No answer",
        xlab="Students Who Work", ylab="Math Performance", col="darkkhaki")

dev.off()

# Performance by Socioeconomic level

par(mfrow=c(1, 2))

counts1 <- table(mydata$ldesemp, mydata$isocioa)
barplot(counts1, main="Languaje Performance Level by Socioeconomic Level", sub="-1=Non Answer, 1= Low 2= Medium 3= High",
        xlab="Socioeconomic Level", col=c("red","orange","lightblue","forestgreen"))

counts2 <- table(mydata$mdesemp, mydata$isocioa)
barplot(counts2, main="Math Performance Level by Socioeconomic Level", sub="-1=Non Answer, 1= Low 2= Medium 3= High",
        xlab="Socioeconomic Level", col=c("red","orange","lightblue","forestgreen"))

# percentages in tables
prop.table(counts1)
prop.table(counts2)

dev.off()

# correlations using subset of variables (until 15 variables)
library(ggplot2)
library(GGally)
mydata[,c("ldesemp","mdesemp","gender")]
mydata[,c("ldesemp","mdesemp","gender","sector","ambito","isocioa")]

# initial regressions
fit1=lm(mdesemp~ female + factor(sector)+ factor(ambito) + factor(isocioa), data=mydata)
summary(fit1)

fit2=lm(ldesemp~ female + factor(sector)+ factor(ambito) + factor(isocioa),data=mydata)
summary(fit2)

# create a table with  model outputs
library('stargazer')
stargazer(list(fit1, fit2),
          title = "Comparing Regression models", 
          covariate.labels = 'Female', 
          out="table3.txt")

# FINDING BEST MODEL for Argentine Aprender Evaluation
# Reading in the data
mydata <- read.delim("/Users/feder/Desktop/km_project/aprender_cordoba_dataset_to_subset.txt")

# save for me 1 best model per subset size for language performance and math performance
library(leaps)
?regsubsets
leaps1<- regsubsets(ldesemp ~., data= mydata, nbest=1, method = "backward") 
summary(leaps1)


leaps2<- regsubsets(mdesemp ~., data= mydata, nbest=1, method = "forward") 
summary(leaps2)

# How many predictors are the optimal number when predicting language performance?
leaps_summary1 <- summary(leaps1)
require(tidyverse);require(ggplot2);require(ggthemes);

data_frame(Cp = leaps_summary1$cp,
           BIC = leaps_summary1$bic,
           AdjR2 = leaps_summary1$adjr2) %>%
        mutate(id = row_number()) %>%
        gather(value_type, value, -id) %>%
        ggplot(aes(id, value, col = value_type)) +
        geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
        facet_wrap(~ value_type, scales = 'free') + scale_x_continuous(breaks = 1:10)

# 5 seems to be the better number of predictors for the model when predicting language performance
# (with high AdjR2 and low BIC and Cp)

# plot statistics by subset size (rsq, cp, adjr2, bic, rss) when predicting language performance
library(car)

subsets(leaps1, statistic="bic", xlim=c(-100,120), legend = FALSE)
subsets(leaps1, statistic="cp", xlim=c(-100,120), legend = FALSE)
subsets(leaps1, statistic="adjr2", xlim=c(-100,100), legend=FALSE)

# 5 best predictors for language performance: 
# 1) mdesemp= Math performance
# 2) ap22= Do you receive payment for the job you do outside your home?
# 3) a39_01= How difficult are the following activities for you? Understanding a text
# 4) isocioa= Student's socio-economical index
# 5) sobreedad= Extra age

# How many predictors are the optimal number when predicting math performance?
leaps_summary2 <- summary(leaps2)

data_frame(Cp = leaps_summary2$cp,
           BIC = leaps_summary2$bic,
           AdjR2 = leaps_summary2$adjr2) %>%
        mutate(id = row_number()) %>%
        gather(value_type, value, -id) %>%
        ggplot(aes(id, value, col = value_type)) +
        geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
        facet_wrap(~ value_type, scales = 'free') + scale_x_continuous(breaks = 1:10)

# 8 seems to be the better number of predictors for the model when predicting math performance
# (with high AdjR2 and low BIC and Cp)

# 7 best predictors for language performance (one was deleted because it wasn't conceptually relevant): 
# 1) ldesemp= Language performance
# 2) sector= Sector (either public or private)
# 3) gender= Gender
# 4) ap26= Absenteeism. So far this year, how many times have you missed school?
# 5) ap39_02= How difficult do you find the following activities? Writing a text
# 6) ap40_01= To what extent do you agree with the following statements? I enjoy studying Mathematics
# 7) isocia= Student's socio-economical index

# plot statistics by subset size (rsq, cp, adjr2, bic, rss) when predicting math performance
library(car)
subsets(leaps2, statistic="bic", xlim=c(-100,120), legend = FALSE)
subsets(leaps2, statistic="cp", xlim=c(-100,120), legend = FALSE)
subsets(leaps, statistic="adjr2", xlim=c(-100,100), legend=FALSE)

# running regressions according to the results obtained in subset selection

# initial regressions
rightfit_lang=lm(ldesemp~ mdesemp + ap22 + factor(isocioa) + sobreedad, data=mydata)
summary(rightfit_lang)

rightfit_math=lm(mdesemp~ ldesemp + factor(sector)+ factor(gender) +
                         ap26 + ap39_02 + ap40_01 + factor(isocioa),data=mydata)
summary(rightfit_math)

######################################
######################################
######################################
######################################
######################################
###### GRADUATE admission case #######
######################################
######################################
######################################
######################################
######################################

# clear memory
rm(list=ls())

# reading in the data
mydata <- read.delim("/Users/feder/Desktop/km_project/admission_predict.txt")

# names of variables
names(mydata)

# checking for linearity (for each predictor against the dep variable) with scatterplots

par(mfrow=c(3,3))

scatter.smooth(x=mydata$GRE.Score, y=mydata$Chance.of.Admit, main="Chance of Admission ~ GRES Scores")  

scatter.smooth(x=mydata$TOEFL.Score, y=mydata$Chance.of.Admit, main="Chance of Admission ~ TOEFL Scores") 

scatter.smooth(x=mydata$University.Rating, y=mydata$Chance.of.Admit, main="Chance of Admission ~ University Ranking") 

scatter.smooth(x=mydata$SOP, y=mydata$Chance.of.Admit, main="Chance of Admission ~ Statement of Purpose Strength") 

scatter.smooth(x=mydata$LOR, y=mydata$Chance.of.Admit, main="Chance of Admission ~ Letter of Recommendation Strength") 

scatter.smooth(x=mydata$CGPA, y=mydata$Chance.of.Admit, main="Chance of Admission ~ Undergraduate GPA") 

scatter.smooth(x=mydata$Research, y=mydata$Chance.of.Admit, main="Chance of Admission ~ Research Experience") 

# Checking for outliers with boxplots

par(mfrow=c(2, 3))  
boxplot(mydata$Chance.of.Admit, main="Chance of Admission", sub=paste("Outlier rows: ", boxplot.stats(mydata$Chance.of.Admit)$out))  
boxplot(mydata$GRE.Score, main="GRE Scores", sub=paste("Outlier rows: ", boxplot.stats(mydata$GRE.Score)$out))  
boxplot(mydata$TOEFL.Score, main="TOEFL Scores", sub=paste("Outlier rows: ", boxplot.stats(mydata$TOEFL.Score)$out))  
boxplot(mydata$University.Rating, main="University Ranking", sub=paste("Outlier rows: ", boxplot.stats(mydata$University.Rating)$out))  
boxplot(mydata$SOP, main="Statement of Purpose Strength", sub=paste("Outlier rows: ", boxplot.stats(mydata$SOP)$out))  
boxplot(mydata$LOR, main="Letter of Recommendation Strength", sub=paste("Outlier rows: ", boxplot.stats(mydata$LOR)$out))  

# Checking for normality with density plot

library(e1071)
par(mfrow=c(3, 3))
plot(density(mydata$Chance.of.Admit), main="Chance of Admission", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$Chance.of.Admit), 2)))
polygon(density(mydata$Chance.of.Admit), col="lightblue")
plot(density(mydata$GRE.Score), main="GRE Scores", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$GRE.Score), 2)))
polygon(density(mydata$GRE.Score), col="lightblue")
plot(density(mydata$TOEFL.Score), main="TOEFL Scores", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$TOEFL.Score), 2)))
polygon(density(mydata$TOEFL.Score), col="lightblue")
plot(density(mydata$University.Rating), main="University Ranking", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$University.Rating), 2)))
polygon(density(mydata$University.Rating), col="lightblue")
plot(density(mydata$SOP), main="Statement of Purpose Strength", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$SOP), 2)))
polygon(density(mydata$SOP), col="lightblue")
plot(density(mydata$LOR), main="Letter of Recommendation Strength", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$LOR), 2)))
polygon(density(mydata$LOR), col="lightblue")
plot(density(mydata$Research), main="Research", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$Research), 2)))
polygon(density(mydata$Research), col="lightblue")

# checking correlations: scatterplot matrix
library(ggplot2)
library(GGally)
ggpairs(mydata)

# Regression analysis
fit1=lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + LOR + CGPA + Research, mydata)
summary(fit1) # University Ranking and SOP are statistically insignificant, f-statistic lower


fit2=lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + CGPA + Research, mydata)
summary(fit2)

# create a table with  model outputs
library('stargazer')
stargazer(list(fit1, fit2),
          title = "Testing regression models", 
          covariate.labels = 'GRE.Score', 
          out="table1.txt")

# 3D visualization
library(plotly)
plot <- plot_ly(mydata, 
                     x = ~CGPA, 
                     y = ~Chance.of.Admit, 
                     z = ~Research, 
                     type = "scatter3d", 
                     size = 0.02)
plot

# Subset: finding the best model for the data. Save for me 1 best model per subset size
library(leaps)
leaps<- regsubsets(Chance.of.Admit ~., data= mydata, nbest=1, method = "exhaustive")
summary(leaps)

# plot statistics by subset size (rsq, cp, adjr2, bic, rss)
dev.off()
library(car)
par(mfrow=c(2, 2))

subsets(leaps, statistic="bic")
subsets(leaps, statistic="cp")
subsets(leaps, statistic="adjr2")

# clustering
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## Create cluster using k-means, k = 3
library(factoextra)
?eclust
admission.km <- eclust(mydata, "kmeans", k.max=3)


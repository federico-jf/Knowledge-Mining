# Final Project Knowledge Mining
# Federico Ferrero

# clear your memory
rm(list=ls())

# set your working directory path
setwd("C:/Users/feder/Desktop/km_project")

# Argentine Aprender Evaluation

# Reading in the data
mydata <- read.delim("/Users/feder/Desktop/km_project/aprender_cordoba_dataset.txt")

# names of variables
names(mydata)

# checking for linearity (for each predictor against the dep variable) with scatterplots
scatter.smooth(x=mydata$sector, y=mydata$ldesemp,  xlab="Sector", ylab="Language Performance",
               sub="1= Public  2= Private",
               main="Language performance ~ Sector (Public/Private)")  

# Boxplots
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
mydata[,c("ldesemp","mdesemp","gender","sector","ambito","isocioa")]

# regressions
fit1=lm(mdesemp~ female + factor(sector)+ factor(ambito) + factor(isocioa),data=mydata)
summary(fit1)


# GRADUATE admission case
# clear your memory
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

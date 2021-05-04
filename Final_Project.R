# EPPS 6323: Knowledge Mining
# Final Project Knowledge Mining
# Academic Analytics: Predictions around Argentine “Aprender” National Evaluation
# Dr. Karl Ho
# Student: Federico Ferrero

# clear your memory
rm(list=ls())

# set your working directory path
setwd("C:/Users/feder/Desktop/km_project")

# Reading in the data
mydata <- read.delim("/Users/feder/Desktop/km_project/aprender_cordoba_dataset.txt")


# names of variables
names(mydata)

# scatterplot ldesmp~mdesemp
scatterplot(ldesemp ~ mdesemp, data=mydata,
            xlab="Math Performance", ylab="Language Performance",
            main="Language Performance by Math Performance")

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
barplot(counts1, main="Language Performance Level by Gender", sub="1= Male 2= Female 3= Other",
        xlab="Gender", col=c("red","orange","lightblue","forestgreen"), ylim=c(0,20000))
    

counts2 <- table(mydata$mdesemp, mydata$gender)
barplot(counts2, main="Math Performance Level by Gender", sub="1= Male 2= Female 3= Other",
        xlab="Gender", col=c("red","orange","lightblue","forestgreen"),
        legend.text = c("Low", "Basic", "Satisf.", "Adv."), ylim=c(0,20000))


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
barplot(counts1, main="Language Performance by Socioeconomic Level", sub="-1=Non Answer, 1= Low 2= Medium 3= High",
        xlab="Socioeconomic Level", col=c("red","orange","lightblue","forestgreen"), ylim=c(0,20000))
        

counts2 <- table(mydata$mdesemp, mydata$isocioa)
barplot(counts2, main="Math Performance by Socioeconomic Level", sub="-1=Non Answer, 1= Low 2= Medium 3= High",
        xlab="Socioeconomic Level", col=c("red","orange","lightblue","forestgreen"),
        legend.text = c("Low", "Basic", "Satisf.", "Adv."), ylim=c(0,20000))

# percentages in tables
prop.table(counts1)
prop.table(counts2)

dev.off()

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

# plot statistics by subset size (rsq, cp, adjr2, bic, rss) when predicting math performance
library(car)
subsets(leaps2, statistic="bic", xlim=c(-100,120), ylim=c(-13500,-9300), legend = FALSE)
subsets(leaps2, statistic="cp", xlim=c(-100,120), legend = FALSE)
subsets(leaps2, statistic="adjr2", xlim=c(-100,100), legend=FALSE)

# 7 best predictors for language performance (one was deleted because it wasn't conceptually relevant): 
# 1) ldesemp= Language performance
# 2) sector= Sector (either public or private)
# 3) gender= Gender
# 4) ap26= Absenteeism. So far this year, how many times have you missed school?
# 5) ap39_02= How difficult do you find the following activities? Writing a text
# 6) ap40_01= To what extent do you agree with the following statements? I enjoy studying Mathematics
# 7) isocia= Student's socio-economical index

# running regressions according to the results obtained in subset selection

rightfit_lang=lm(ldesemp~ mdesemp + ap22 + ap39_01 +factor(isocioa) + sobreedad, data=mydata)
summary(rightfit_lang)

rightfit_math=lm(mdesemp~ ldesemp + factor(sector)+ factor(gender) +
                         ap26 + ap39_02 + ap40_01 + factor(isocioa),data=mydata)
summary(rightfit_math)

# create a table with  model outputs
library('stargazer')
stargazer(list(rightfit_lang, rightfit_math),
          title = "Comparing Regression models outputs", 
          out="table1.txt")

# Decision trees for Language Performance Prediction
library("rpart")
library("rpart.plot")
library("rattle")

# AER Package (AER: Applied Econometrics with R)
library(AER)

# Subset data including predictor variables
tree_base <- subset(mydata, select = c(ldesemp, mdesemp, ap22, isocioa, sobreedad))

# rename variables
names(tree_base)[1] <- "lang_perf"
names(tree_base)[2] <- "math_perf"
names(tree_base)[3] <- "job_pay"
names(tree_base)[4] <- "socioeconom"
names(tree_base)[5] <- "overage"


# create new dataset without missing data
tree_base <- na.omit(tree_base)

# Recode ldesemp as dummy (1 for satisfactory and advanced, 0 for basic and below)

tree_base$lang_perf <- ifelse(tree_base$lang_perf >= "3", 1, 0);
set.seed(1001)

# Order data by row number
new_tree_base <- tree_base[sample(nrow(tree_base)),]

# Indexing for training data (selection of 70 % of data)
t_idx <- sample(seq_len(nrow(tree_base)), size = round(0.70 * nrow(tree_base)))

# Build train and test data
traindata <- new_tree_base[t_idx,]
testdata <- new_tree_base[ - t_idx,]

# Decision tree model
dtree_lang <- rpart::rpart(formula = lang_perf ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001)) # complexity parameter

# Plot Decision tree 
rattle::fancyRpartPlot(dtree_lang, type = 1, main = "Decision tree: Language Performance", caption = "Accomplish at Least Satisfactory Language Performance Level" )

resultdt <- predict(dtree_lang, newdata = testdata, type = "class")

# Confusion matrix
cm_langdt <- table(testdata$lang_perf, resultdt, dnn = c("Actual", "Predicted"))
cm_langdt

# Predicted Accomplish rate
cm_langdt[4] / sum(cm_langdt[, 2])

# Predicted Not Accomplish rate 
cm_langdt[1] / sum(cm_langdt[, 1])

# Accuracy
accuracydt <- sum(diag(cm_langdt)) / sum(cm_langdt)
accuracydt

# install.packages("party") 
library(party)

# Conditional Inference Tree: Language Performance
cit <- ctree(lang_perf~ ., data = traindata)
plot(cit, main = "Conditional Inference Tree")

# Confusion matrix
cm_langcit = table(testdata$lang_perf, round(predict(cit, newdata = testdata)), dnn = c("Actual", "Predicted"))
cm_langcit

# Predicted Approval rate
cm_langcit[4] / sum(cm_langcit[, 2])

# Predicted Denial rate
cm_langcit[1] / sum(cm_langcit[, 1])

# Accuracy
accuracycit <- sum(diag(cm_langcit)) / sum(cm_langcit)
accuracycit

# Decision trees for Math Performance Prediction
# Subset data including predictor variables
tree_base <- subset(mydata, select = c(mdesemp, ldesemp, sector, gender, ap26,
                                            ap39_02, ap40_01, isocioa))

# rename variables
names(tree_base)[1] <- "math_perf"
names(tree_base)[2] <- "lang_perf"
names(tree_base)[5] <- "absent"
names(tree_base)[6] <- "dif_writing"
names(tree_base)[7] <- "enjoy_math"
names(tree_base)[8] <- "socioeconom"

# create new dataset without missing data
tree_base <- na.omit(tree_base)

# Recode ldesemp as dummy (1 for satisfactory and advanced, 0 for basic and below)

tree_base$math_perf <- ifelse(tree_base$math_perf >= "3", 1, 0);
set.seed(1001)

# Order data by row number
new_tree_base <- tree_base[sample(nrow(tree_base)),]

# Indexing for training data (selection of 70 % of data)
t_idx <- sample(seq_len(nrow(tree_base)), size = round(0.70 * nrow(tree_base)))

# Build train and test data
traindata <- new_tree_base[t_idx,]
testdata <- new_tree_base[ - t_idx,]

# Decision tree model
dtree_math <- rpart::rpart(formula = math_perf ~ ., data = traindata, method = "class", control = rpart.control(cp = 0.001)) # complexity parameter

# Plot Decision tree 
rattle::fancyRpartPlot(dtree_math, type = 1, main = "Decision tree: Math Performance", caption = "Accomplish at Least Satisfactory Math Performance Level" )

resultdt <- predict(dtree_math, newdata = testdata, type = "class")

# Confusion matrix
cm_mathdt <- table(testdata$math_perf, resultdt, dnn = c("Actual", "Predicted"))
cm_mathdt

# Predicted Accomplish rate
cm_mathdt[4] / sum(cm_mathdt[, 2])

# Predicted Not Accomplish rate 
cm_mathdt[1] / sum(cm_mathdt[, 1])

# Accuracy
accuracydt <- sum(diag(cm_mathdt)) / sum(cm_mathdt)
accuracydt

# Conditional Inference Tree: Math Performance
cit <- ctree(math_perf~ ., data = traindata)
plot(cit, main = "Conditional Inference Tree")

# Confusion matrix
cm_mathcit = table(testdata$math_perf, round(predict(cit, newdata = testdata)), dnn = c("Actual", "Predicted"))
cm_mathcit

# Predicted Approval rate
cm_mathcit[4] / sum(cm_mathcit[, 2])

# Predicted Denial rate
cm_mathcit[1] / sum(cm_mathcit[, 1])

# Accuracy
accuracycit <- sum(diag(cm_mathcit)) / sum(cm_mathcit)
accuracycit



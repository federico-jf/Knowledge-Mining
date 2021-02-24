# Assignment 3
rm(list=ls())
library(haven)


TEDS_2016 <- read_stata("https://github.com/datageneration/home/blob/master/DataProgramming/data/TEDS_2016.dta?raw=true")
#create subset

TEDS_subset <- subset(TEDS_2016, select = c("Tondu","female","DPP","age","income","edu","Taiwanese","Econ_worse"))


#change labels

TEDS_subset$Tondu<-as.numeric(TEDS_subset$Tondu,labels=c("Unification now",
                                                         "Status quo, unif. in future",
                                                         "Status quo, decide later", 
                                                         "Status quo forever",
                                                         "Status quo, indep. in future", 
                                                         "Independence now",
                                                         "No response"))
# regression
fit= lm(Tondu ~ age + income + edu, data = TEDS_subset)
summary(fit)


# regplot
(regplot= plot(lm(Tondu ~ age + income + edu, data = TEDS_subset)))

# 7 categories in the dependent variable (Tondu). To improve: turn it into logistic regression.


fit=lm(Tondu~age+edu+income,data=TEDS_subset)
library(ggplot2)
ggplot(data = TEDS_2016) + 
  geom_point(mapping = aes(x = Party , y = income )) + 
  facet_wrap(~ female, nrow = 2)

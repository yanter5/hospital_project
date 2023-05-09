library(car)
library(olsrr)
library(tidyverse)
library(effects)

setwd("/users/coleyant/desktop/STAT 334")
hospitals <- read.csv("Hospitals.csv")

tempHosp <- hospitals
tempHosp$Region <- factor(tempHosp$Region)
tempHosp$School <- factor(tempHosp$School)

#FULL MODEL
model1 <- lm(LenStay ~ Age + InfRisk + Culture + XRay + School + Region + Beds + Census + Nurses + Services, data = tempHosp)
summary(model1)
Anova(model1)
#####

#CHECKING ASSUMPTIONS
residualPlots(model1,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(model1,envelope=FALSE,pch=16,id=FALSE)

##Normality
#No normality issues

##Linearity
#No problems with linearity

##Equal Variance
#All graphs look good

##Not a time series, we assume it is a random sample and thus independence assumption is met.

#####

#LEVERAGE
model1$leverage <- hatvalues(model1)
plot(model1$leverage,type="b",pch=20)

n = nrow(tempHosp)
p = 10
leverage.cutoff <- 3*p/n

high.leverage.rows <- which(model1$leverage >= leverage.cutoff)
rownames(tempHosp)[high.leverage.rows]
#Rows 8 and 112 are high leverage points.

#####

#COOK'S DIST
model1$Cook <- cooks.distance(model1)
plot(model1$Cook,type="b",pch=20)

influence.cutoff <- qf(.5,p,n-p)
influence.cutoff

high.influence.rows <- which(model1$Cook >= influence.cutoff)
rownames(tempHosp)[high.influence.rows]
#None exceed cook's distance cutoff, no influential points

#####

#OUTLIERS
model1.ESR <- rstudent(model1)

hosp.extreme.outliers <- which(abs(model1.ESR) >= 3)
rownames(tempHosp)[hosp.extreme.outliers]
#Obs 47 is an extreme outlier

####

#CHECKING VIFS // MODIFYING VARIABLES
ols_vif_tol(model1)
cor(tempHosp$Beds, tempHosp$Census)
cor(tempHosp$Census, tempHosp$Nurses)

#We see a .981 correlation between beds and census, so we removed the beds variable from the model.

model1b <- lm(LenStay ~ Age + InfRisk + Culture + XRay + School + Region + Census + Nurses + Services, data = tempHosp)
ols_vif_tol(model1b)

#There is moderate multicollinearity between Census and Nurses, so we will transform the variables.

tempHosp$NursePerson <- tempHosp$Nurses / tempHosp$Census

#We now proceed with a variable that has the ratio for average nurses/average daily hospital patients.

model2 <- lm(LenStay ~ Age + InfRisk + Culture + XRay + School + Region + Census + NursePerson + Services, data = tempHosp)
ols_vif_tol(model2)

#This fixed the problems with multicollinearity.

#####

###SELECTING VARIABLES/MODEL
select = ols_step_best_subset(model2)
select

#Models 6 and 7 appear to be the best. They have some of the highest Adj. R-square values while also having the lowest Cp, AIC, and SBC. We will continue with these as possible models.

best_subsets_model1 = lm(LenStay ~ Age + InfRisk + XRay + Region + Census + NursePerson, data = tempHosp)
best_subsets_model2 = lm(LenStay ~ Age + InfRisk + XRay + Region + Census + NursePerson + Services, data = tempHosp)

Anova(best_subsets_model1)
Anova(best_subsets_model2)

#Services is not significant in this model, so we proceed with our reduced model as best_subsets_model_1.


# Assumptions on reduced model
residualPlots(best_subsets_model1,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(best_subsets_model1,envelope=FALSE,pch=16,id=FALSE)

# All assumptions look good. The slight tail off is not enough to consider normality issues because it is only 4 values of the 113 in the dataset.


#Stepwise Analysis
n = 113

step1 <- step(best_subsets_model1,scope=~.^2,direction="both",trace=1,k=log(n))
summary(step1)

#We run the stepwise regression with all possible interactions. Only the InfRisk:Census interaction comes back as significant.

#Check Assumptions/VIFs with this model
residualPlots(step1,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(step1,envelope=FALSE,pch=16,id=FALSE)

#Assumptions still look good.

ols_vif_tol(step1)

#There is extreme multicollinearity present, between the interaction term and original variable. We will center all the variables to fix this.

# Centering the variables
tempHosp$cInfRisk <- tempHosp$InfRisk - mean(tempHosp$InfRisk)
tempHosp$cCensus <- tempHosp$Census - mean(tempHosp$Census)
tempHosp$cXRay <- tempHosp$XRay - mean(tempHosp$XRay)
tempHosp$cNursePerson <- tempHosp$NursePerson - mean(tempHosp$NursePerson)


final_model = lm(LenStay ~ cInfRisk + cXRay + Region + cCensus + cNursePerson + cInfRisk:cCensus, data = tempHosp)
summary(final_model)
Anova(final_model)

#### Comparing models
test_model = lm(LenStay ~ Age + InfRisk + Culture + XRay + School + Region + Beds + Census + Nurses + Services + InfRisk:Census + NursePerson, data = tempHosp)

anova(final_model, test_model)

#P-value is fairly high so we can conclude that our model is more accurate than the full model.

# Final model influential observations
final_model$Cook <- cooks.distance(final_model)
plot(final_model$Cook,type="b",pch=20)

influence.cutoff <- qf(.5,p,n-p)
influence.cutoff

high.influence.rows.final <- which(final_model$Cook >= influence.cutoff)
rownames(tempHosp)[high.influence.rows.final]

#There are  no influential points in this model. We decide that this is the final model.






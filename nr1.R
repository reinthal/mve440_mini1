cat("\014")
setwd("/chalmers/users/maglinds/CAS/Year_1/BigData/Assignment1")

library(party)
library(Ecdat)
library(randomForest)
library(ggRandomForests)
library(forestFloor)
library(plyr) 
library(corrplot)
#library(rfPermute)

#Is the data imbalanced or not? (wrt sex)
data(DoctorAUS, package = "Ecdat")
sampleDoctorAUS <- DoctorAUS[sample(1:nrow(DoctorAUS), 200, replace = FALSE),]
count(DoctorAUS,'sex')
#no

# Basic Scatterplot Matrix, to see correlations between features
allPairs <- pairs(~age+income+actdays+hscore+doctorco+
        prescrib,data=DoctorAUS,main="Simple Scatterplot Matrix")

#Create a random forest to classify the sex of patients
output.forest <- randomForest(sex~., data = sampleDoctorAUS, proximity = TRUE)

#Plot error curve
plot(gg_error(output.forest))

#Plot ROC-curve. (what is this, really? And why can't we classify the sex of patients?)
rfsrc_DoctorAUS <- rfsrc(chcond ~ ., data = DoctorAUS)
gg_dta <- gg_roc(rfsrc_DoctorAUS, which.outcome = 1)
plot(gg_dta)

#Proximity Plot
MDSplot(output.forest, sampleDoctorAUS$sex, k=2, pch=c(24,3), 
        palette = c(4,4))

#Partial dependency
partialPlot(output.forest, DoctorAUS, actdays, "versicolor")

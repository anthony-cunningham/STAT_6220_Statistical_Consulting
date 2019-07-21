## STAT 6220 Assignment 8: Classification and ROC Curves ##

# Reading Data In
mammo <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 8/mammographic.csv", header = TRUE)

# Recode "Shape" as a factor
mammo$Shape <- as.factor(mammo$Shape)

# Drop "BI.RADS" variable
library(dplyr)
mammo1 <- select(mammo, -BI.RADS)

# 1 USING CROSS-VALIDATION TO SELECT MODEL

# Input all Variables you Want to Use in Cross-Validation
vars <- c("Age", "Shape", "Margin", "Density")

# Sets Up a Model Specification Matrix, where each row represents a candidate model
nvars <- length(vars)
list.to.expand = vector(mode = "list", length = nvars)
for(i in 1:nvars){
  list.to.expand[[i]] = c(0,1)
}
model.spec.matrix <- expand.grid(list.to.expand)
model.spec.matrix <- as.matrix(model.spec.matrix)
colnames(model.spec.matrix) <- vars

attach(mammo1)
library(boot)

## Enter your Y Variable as a Quoted String
y_var <- c("Severity")

## Enter your dataset name as is
dataset <- mammo1
cv.err <- c()   # initialize cv.err to NULL

n_models <- nrow(model.spec.matrix)
for(i in 1:n_models){
  if(i == 1){
    glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""), family = binomial)
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
  if(i != 1){
    row <- as.vector(model.spec.matrix[i,])
    ref <- seq(1:nvars)[as.logical(row)]
    model_vars <- vars[ref]
    glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""), family = binomial)
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
}

model.spec.df <- as.data.frame(model.spec.matrix)
model_id <- c(seq(1:n_models))
model.spec.df <- cbind(model_id, model.spec.df, cv.err)
colnames(model.spec.df) <- c("Model_ID", "Age", "Shape", "Margin", "Density", "CV_Errors")

## Top 3 Models, according to PRESS
top_n(model.spec.df, -3, CV_Errors)

## Which Model is Model 8?
model.spec.df[8,]

# 2 CALCULATING PREDICTED PROBABILITIES

## Create a Vector of Predicted Probabilities
## Initialize "y_hats" vector
y_hats1 <- NULL
for(i in 1:nrow(mammo1)){
  mammo.glm <- glm(Severity ~ Age + Shape + Margin, data = mammo1[-i,], family = binomial)
  prob <- predict(mammo.glm, newdata = mammo1[i,], type = "response")
  y_hats1 <- append(y_hats1, prob)
}
head(y_hats1)
tail(y_hats1)

# 3 EVALUATING OUR MODEL USING ROC CURVE

library(ROCR)

## Make Predictions Using our y_hats
pred <- prediction(y_hats1, Severity)

## Obtain True-Postive and False-Postive Rates
perf <- performance(pred,"tpr","fpr") 

## Obtain AUC
auc <- performance(pred,"auc")@y.values[[1]]
auc

## Calculating Sensitivity and Specificity
pred.outcome <- round(y_hats1)
pred.table <- table(Severity, pred.outcome)
sens <- pred.table[2,2]/(pred.table[2,1] + pred.table[2,2])
sens
spec <- pred.table[1,1]/(pred.table[1,1] + pred.table[1,2])
spec

## Calculate Overall Misclassification Rate with Threshold 0.5

misclass <- (pred.table[1,2] + pred.table[2,1])/nrow(mammo1)
misclass

## Plot ROC Curve of True-Positive vs False-Positive Rates w/ Sensitivity and Specificity Calcs
plot(perf, main="ROC Curve for Logistic Regression Classifier")
points(x = (1 - spec), y = sens, col = "red", pch = 19, cex = 2)

# 4 RECREATE OUR ANALYSIS USING "BI-RADS" VARIABLE INCLUDED

# Input all Variables you Want to Use in Cross-Validation
vars <- c("Age", "Shape", "Margin", "Density", "BI.RADS")

# Sets Up a Model Specification Matrix, where each row represents a candidate model
nvars <- length(vars)
list.to.expand = vector(mode = "list", length = nvars)
for(i in 1:nvars){
  list.to.expand[[i]] = c(0,1)
}
model.spec.matrix <- expand.grid(list.to.expand)
model.spec.matrix <- as.matrix(model.spec.matrix)
colnames(model.spec.matrix) <- vars

attach(mammo)

## Enter your Y Variable as a Quoted String
y_var <- c("Severity")

## Enter your dataset name as is
dataset <- mammo
cv.err <- c()   # initialize cv.err to NULL

n_models <- nrow(model.spec.matrix)
for(i in 1:n_models){
  if(i == 1){
    glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""), family = binomial)
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
  if(i != 1){
    row <- as.vector(model.spec.matrix[i,])
    ref <- seq(1:nvars)[as.logical(row)]
    model_vars <- vars[ref]
    glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""), family = binomial)
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
}

model.spec.df <- as.data.frame(model.spec.matrix)
model_id <- c(seq(1:n_models))
model.spec.df <- cbind(model_id, model.spec.df, cv.err)
colnames(model.spec.df) <- c("Model_ID", "Age", "Shape", "Margin", "Density", "BI-RADS", "CV_Errors")

## Top 5 Models, According to PRESS
top_n(model.spec.df, -5, CV_Errors)

## Which Model is Model 20?
model.spec.df[20,]

# Part (B)
# CALCULATING PREDICTED PROBABILITIES

## Create a Vector of Predicted Probabilities
## Initialize "y_hats" vector
y_hats2 <- NULL
for(i in 1:nrow(mammo1)){
  mammo.glm <- glm(Severity ~ Age + Shape + BI.RADS, data = mammo[-i,], family = binomial)
  prob <- predict(mammo.glm, newdata = mammo[i,], type = "response")
  y_hats2 <- append(y_hats2, prob)
}

# Part (C)
# EVALUATING OUR MODEL USING ROC CURVE

## Make Predictions Using our y_hats
pred <- prediction(y_hats2, Severity)

## Obtain True-Postive and False-Postive Rates
perf <- performance(pred,"tpr","fpr") 

## Obtain AUC
auc <- performance(pred,"auc")@y.values[[1]]
auc

## Calculating Sensitivity and Specificity
pred.outcome <- round(y_hats2)
pred.table <- table(Severity, pred.outcome)
sens <- pred.table[2,2]/(pred.table[2,1] + pred.table[2,2])
sens
spec <- pred.table[1,1]/(pred.table[1,1] + pred.table[1,2])
spec

## Calculate Overall Misclassification Rate with Threshold 0.5
misclass <- (pred.table[1,2] + pred.table[2,1])/nrow(mammo1)
misclass

## Plot ROC Curve of True-Positive vs False-Positive Rates w/ Sensitivity and Specificity Calcs
plot(perf, main="ROC Curve for New Logistic Regression Classifier")
points(x = (1 - spec), y = sens, col = "red", pch = 19, cex = 2)

# 5 COMPARE CLASSIFIERS

## Plot with Bootstrapped CI's
library(pROC)
roc_model1 <- plot.roc(Severity, y_hats1,  main="Model 1 ROC Curve With 95% CIs", percent=TRUE, ci=TRUE)
ci_model1 <- ci.se(roc_model1, specificities=seq(0, 100, 5))

roc_model2 <- plot.roc(Severity, y_hats2,  main="Model Comparison: ROC Curve With 95% CIs", percent=TRUE, ci=TRUE)
ci_model2 <- ci.se(roc_model2, specificities=seq(0, 100, 5))

plot(ci_model1, type="shape", col="#ff000030") 
plot(ci_model2, type="shape", col="#0000ff20") 

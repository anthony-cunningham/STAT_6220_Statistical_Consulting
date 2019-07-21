## ASSIGNMENT 6: CROSS-VALIDATION FOR VARIABLE SELECTION ##

library(readr)
if (! file.exists("salary_data.csv"))
  download.file("http://homepage.stat.uiowa.edu/~rdecook/stat6220/datasets/salary_data.csv", "salary_data.csv")

## Note: File Directory Path will differ; use your file location
salary <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 6/salary_data.csv", header=TRUE)

## 1

## Insert all Var's you want to use
vars <- c("Rank_Code", "Gender", "LogACG")

## Sets Up a Model Specification Matrix, where each row represents a candidate model
nvars <- length(vars)
list.to.expand = vector(mode = "list", length = nvars)
for(i in 1:nvars){
  list.to.expand[[i]] = c(0,1)
}
model.spec.matrix <- expand.grid(list.to.expand)
model.spec.matrix <- as.matrix(model.spec.matrix)
colnames(model.spec.matrix) <- vars

## 2

attach(salary)
library(boot)

## Enter your Y Variable as a Quoted String
y_var <- c("logSalary")

## Enter your dataset name as is
dataset <- salary
cv.err <- c()   # initialize cv.err to NULL

n_models <- nrow(model.spec.matrix)
for(i in 1:n_models){
  if(i == 1){
  glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""))
  cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
  cv.err <- rbind(cv.err, cv.err_i[1])
  }
  if(i != 1){
  row <- as.vector(model.spec.matrix[i,])
  ref <- seq(1:nvars)[as.logical(row)]
  model_vars <- vars[ref]
  glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""))
  cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
  cv.err <- rbind(cv.err, cv.err_i[1])
  }
}

## 3

model.spec.df <- as.data.frame(model.spec.matrix)
model_id <- c(seq(1:n_models))
model.spec.df <- cbind(model_id, model.spec.df, cv.err)
colnames(model.spec.df) <- c("Model_ID", "Rank_Code", "Gender", "LogACG", "Engineering", "CV_Errors")
library(ggplot2)

## Create a Dot Plot of CV_Errors vs Models 1-8; Ordered from Small to Large
ggplot(model.spec.df, aes(x = reorder(as.factor(Model_ID), CV_Errors), y = CV_Errors)) + geom_point(size = 2)

## Which Model is Model 8?
model.spec.df[8,]

filter(model.spec.df, CV_Errors < 0.05)

## 5

AIC_vect <- c()
BIC_vect <- c()
for(i in 1:n_models){
  if(i == 1){
    glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""))
    aic <- AIC(glm_model)
    bic <- BIC(glm_model)
    AIC_vect <- rbind(AIC_vect, aic[1])
    BIC_vect <- rbind(BIC_vect, bic[1])
  }
  if(i != 1){
    row <- as.vector(model.spec.matrix[i,])
    ref <- seq(1:nvars)[as.logical(row)]
    model_vars <- vars[ref]
    glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""))
    aic <- AIC(glm_model)
    bic <- BIC(glm_model)
    AIC_vect <- rbind(AIC_vect, aic[1])
    BIC_vect <- rbind(BIC_vect, bic[1])
  }
}
model.spec.df <- cbind(model.spec.df, AIC_vect, BIC_vect)

top_AIC <- model.spec.df[with(model.spec.df,order(AIC_vect)), ]
top_BIC <- model.spec.df[with(model.spec.df,order(BIC_vect)), ]
top_AIC[1,]
top_BIC[1,]

## 6
vars <- c("Rank_Code", "Gender", "LogACG", "Engineering")
y_var <- c("logSalary")
dataset <- salary

nvars <- length(vars)
list.to.expand = vector(mode = "list", length = nvars)
for(i in 1:nvars){
  list.to.expand[[i]] = c(0,1)
}
model.spec.matrix <- expand.grid(list.to.expand)
colnames(model.spec.matrix) <- vars
model.spec.matrix <- as.matrix(model.spec.matrix)

cv.err <- c()   # initialize cv.err to NULL

n_models <- nrow(model.spec.matrix)
for(i in 1:n_models){
  if(i == 1){
    glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""))
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
  if(i != 1){
    row <- as.vector(model.spec.matrix[i,])
    ref <- seq(1:nvars)[as.logical(row)]
    model_vars <- vars[ref]
    glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""))
    cv.err_i <- cv.glm(dataset, glm_model, K = 10)$delta[1]
    cv.err <- rbind(cv.err, cv.err_i[1])
  }
}

model.spec.df <- as.data.frame(model.spec.matrix)
model_id <- c(seq(1:n_models))
model.spec.df <- cbind(model_id, model.spec.df, cv.err)
colnames(model.spec.df) <- c("Model_ID", "Rank_Code", "Gender", "LogACG", "Engineering", "CV_Errors")

## Create a Dot Plot of CV_Errors vs Models 1-16; Ordered from Small to Large
ggplot(model.spec.df, aes(x = reorder(as.factor(Model_ID), CV_Errors), y = CV_Errors)) + geom_point(size = 2)

## Which Model is Best?
model.spec.df[16,]

filter(model.spec.df, CV_Errors < 0.05)


AIC_vect <- c()
BIC_vect <- c()
for(i in 1:n_models){
  if(i == 1){
    glm_model <- glm(paste(y_var, " ~ ", 1, sep = ""))
    aic <- AIC(glm_model)
    bic <- BIC(glm_model)
    AIC_vect <- rbind(AIC_vect, aic[1])
    BIC_vect <- rbind(BIC_vect, bic[1])
  }
  if(i != 1){
    row <- as.vector(model.spec.matrix[i,])
    ref <- seq(1:nvars)[as.logical(row)]
    model_vars <- vars[ref]
    glm_model <- glm(paste(y_var, " ~ ", paste(model_vars, collapse = "+"), sep = ""))
    aic <- AIC(glm_model)
    bic <- BIC(glm_model)
    AIC_vect <- rbind(AIC_vect, aic[1])
    BIC_vect <- rbind(BIC_vect, bic[1])
  }
}
model.spec.df <- cbind(model.spec.df, AIC_vect, BIC_vect)

top_AIC <- model.spec.df[with(model.spec.df,order(AIC_vect)), ]
top_BIC <- model.spec.df[with(model.spec.df,order(BIC_vect)), ]
top_AIC[1,]
top_BIC[1,]

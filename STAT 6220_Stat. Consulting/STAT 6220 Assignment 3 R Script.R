###   GETTING DATA IN   ###

library(tidyr)
library(lmerTest)
library(lattice)
library(emmeans)
library(ggplot2)

confusion <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 3/Confusion.csv", header=TRUE)
confusion_long <- gather(confusion, SNR, Confusion_Score, Quiet:X.11, factor_key=TRUE)
confusion_long$sqrt_Confusion_Score <- sqrt(confusion_long$Confusion_Score)

frustration <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 3/Frustration.csv", header=TRUE)
frustration_long <- gather(frustration, SNR, Frustration_Score, Quiet:X.11, factor_key=TRUE)
frustration_long$ln_Frustration_Score <- log(frustration_long$Frustration_Score)

negative <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 3/Negative.csv", header=TRUE)
negative_long <- gather(negative, SNR, Negative_Score, Quiet:X.11, factor_key=TRUE)
negative_long$sqrt_Negative_Score <- sqrt(negative_long$Negative_Score)



###   CONFUSION SCORES ANALYSES   ###

## Model Fitting: regular vs transformed scores

# Note: this code uses the "lmer" package
confusion.lmer <- lmer(Confusion_Score ~ (1|Subject.ID) + SNR, data = confusion_long)
conf.resids <- residuals(confusion.lmer, type = "pearson")

confusion_tr.lmer <- lmer(sqrt_Confusion_Score ~ (1|Subject.ID) + SNR, data = confusion_long)
conf.resids_tr <- residuals(confusion_tr.lmer, type = "pearson")

## Diagnostics: QQ Plots Before and After SqRt Transformation

par(mfrow = c(1,2))
qqBefore <- qqnorm(conf.resids, main = "QQ Normal Plot with Confusion Score"); qqline(conf.resids)
qqAfter <- qqnorm(conf.resids_tr, main = "QQ Normal Plot with sqrt(Confusion Score)"); qqline(conf.resids_tr)

## Scatter Plots of  Confusion Scores Stratified by Subject

# Note: this code uses the "lattice" package
xyplot(Confusion_Score ~ SNR | Subject.ID, data = confusion_long, type = c("p", "r"))

## Dot Plot of Overall SqRt(Confusion Score Means) By SNR Level with 95% CI Bars

# Note: this code uses both the "ggplot2" and "emmeans" packages
emmip(confusion_tr.lmer, ~ SNR, CIs = TRUE)

## Analysis: Sqrt(Confusion Scores)

# Note: this code uses the "emmeans" package
summary(confusion_tr.lmer)
anova(confusion_tr.lmer)
conf.emm <- emmeans(confusion_tr.lmer, "SNR")
pairs(conf.emm)




###   FRUSTRATION SCORES ANALYSES   ###

## Model Fitting: regular vs transformed scores

frustration.lmer <- lmer(Frustration_Score ~ (1|Subject.ID) + SNR, data = frustration_long)
frus.resids <- residuals(frustration.lmer, type = "pearson")

frustration_tr.lmer <- lmer(ln_Frustration_Score ~ (1|Subject.ID) + SNR, data = frustration_long)
frus.resids_tr <- residuals(frustration_tr.lmer, type = "pearson")

## Diagnostics: QQ Plots Before and After Log Transformation

par(mfcol = c(1,2))
qqBefore <- qqnorm(frus.resids, main = "QQ Normal Plot with Frustration Score"); qqline(frus.resids)
qqAfter <- qqnorm(frus.resids_tr, main = "QQ Normal Plot with ln(Frustration Score)"); qqline(frus.resids_tr)

## Scatter Plots of Frustration Scores Stratified by Subject

xyplot(Frustration_Score ~ SNR | Subject.ID, data = frustration_long, type = c("p", "r"))

## Dot Plot of Overall Ln(Frustration Score Means) By SNR Level with 95% CI Bars

emmip(frustration_tr.lmer, ~ SNR, CIs = TRUE)

## Analysis: Ln(Frustration Scores)

summary(frustration_tr.lmer)
anova(frustration_tr.lmer)
frus.emm <- emmeans(frustration_tr.lmer, "SNR")
pairs(frus.emm)


###   NEGATIVE SCORES ANALYSES   ###

## Model Fitting: regular vs transformed scores

negative.lmer <- lmer(Negative_Score ~ (1|Subject.ID) + SNR, data = negative_long)
neg.resids <- residuals(negative.lmer, type = "pearson")

negative_tr.lmer <- lmer(sqrt_Negative_Score ~ (1|Subject.ID) + SNR, data = negative_long)
neg.resids_tr <- residuals(negative_tr.lmer, type = "pearson")

## Diagnostics: QQ Plots Before and After Sqrt Transformation

par(mfcol = c(1,2))
qqBefore <- qqnorm(neg.resids, main = "QQ Normal Plot with Negative Score"); qqline(neg.resids)
qqAfter <- qqnorm(neg.resids_tr, main = "QQ Normal Plot with sqrt(Negative Score"); qqline(neg.resids_tr)

## Scatter Plots of Negative Scores Stratified by Subject

xyplot(Negative_Score ~ SNR | Subject.ID, data = negative_long, type = c("p", "r"))

## Dot Plot of Overall SqRt(Negative Score Means) By SNR Level with 95% CI Bars

emmip(negative_tr.lmer, ~ SNR, CIs = TRUE)

## Analysis: Sqrt(Negative Scores)

summary(negative_tr.lmer)
anova(negative_tr.lmer)
neg.emm <- emmeans(negative_tr.lmer, "SNR")
pairs(neg.emm)

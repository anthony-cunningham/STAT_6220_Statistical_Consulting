## STAT 6220: Assignment 9 ##

speed_data <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 9/work_zone.csv", header = TRUE)

speed_data$WorkZone <- relevel(speed_data$WorkZone, ref = "s")
speed_data$ID <- factor(speed_data$ID)

library(lmerTest)
library(emmeans)

## Baseline Model
lm_speed <- lm(MeanSpeed ~ WorkZone, data = speed_data)
summary(lm_speed)
baseline_fit <- anova(lm_speed)
baseline_fit

### Baseline Model Assumptions

## Constant Variance
boxplot(MeanSpeed ~ WorkZone, data = speed_data) ## Looks Good

## Normality
qqnorm(residuals(lm_speed), pch = 16); qqline(residuals(lm_speed)) ## Looks Fine

### Further Analyses
wz_base.emm <- emmeans(lm_speed, "WorkZone")
pairs(wz_base.emm)

## With Interaction
glmm_speed_int <- lmer(MeanSpeed ~ (1|ID) + WorkZone + rep + WorkZone*rep, data = speed_data)
summary(glmm_speed_int)
anova(glmm_speed_int) ## Interaction nowhere near significant; will drop from model

## Without Interaction
glmm_speed <- lmer(MeanSpeed ~ (1|ID) + WorkZone + rep, data = speed_data)
summary(glmm_speed)
best_fit <- anova(glmm_speed)
best_fit

## Model Assumptions

## Constant Variance
plot(fitted(glmm_speed), residuals(glmm_speed), pch = 16); abline(h = 0) ## Looks Good

## Normality
qqnorm(residuals(glmm_speed), pch = 16); qqline(residuals(glmm_speed)) ## A bit thicker in the tails

## Further Analyses
xyplot(MeanSpeed ~ WorkZone | ID, data = speed_data, type = c("p", "r"))
xyplot(MeanSpeed ~ rep | ID, data = speed_data, type = c("p", "r"))

emmip(glmm_speed, ~ WorkZone, CIs = TRUE)
emmip(glmm_speed, ~ rep, CIs = TRUE)

wz.emm <- emmeans(glmm_speed, "WorkZone")
pairs(conf.emm)
rep.emm <- emmeans(glmm_speed, "rep")
pairs(rep.emm)

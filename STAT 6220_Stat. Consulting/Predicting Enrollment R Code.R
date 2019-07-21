### STAT 6220 Assignment 7 R Code ###

admit <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 7/admissions_data_model_build.csv", header=TRUE)
      ## NOTE: Change the url to wherever Admissions data is locally ##

library(rpart)

admit$enrl_ind <- as.factor(admit$enrl_ind)
attach(admit)

admit_tree <- rpart(enrl_ind ~ ., data = admit, method = "class")
print(admit_tree)
plot(admit_tree)
text(admit_tree)


## For Cross-Validation ##
printcp(admit_tree)
plotcp(admit_tree)
## Where to Prune (ie What Should we set the cp as?)
## For the geometric means of the intervals of values of cp for which a pruning is optimal, 
## a cross-validation has (usually) been done in the initial construction by rpart. 
## A good choice of cp for pruning is often the leftmost value for which the mean lies below the horizontal line.
admit_prune <- prune(admit_tree, cp = 0.015)
admit_prune

## Make Nicer Trees ##
library(rattle)
fancyRpartPlot(admit_prune, main = "Who is Likely to Enroll at the University of Iowa?")


predictions <- predict(admit_prune)
decision <- predict(admit_prune, type = "class")
both <- cbind(predictions, decision)
enrl_ind <- as.numeric(enrl_ind)
full <- cbind(both, enrl_ind)
colnames(full) <- c("Will Not Enroll", "Will Enroll", "Predicted Decision", "Actual Decision")

## Creating Vectors of Predicted vs Actual Enrollment Outcomes ##
yes_noEnroll <- NULL
yes_Enroll <- NULL
no_noEnroll <- NULL
no_Enroll <- NULL
for(i in 1:nrow(full)){
  if(full[i,3] == 1 & full[i,4] == 1){
    yes_noEnroll <- append(yes_noEnroll, i)
  }
  if(full[i,3] == 2 & full[i,4] == 2){
    yes_Enroll <- append(yes_Enroll, i)
  }
  if(full[i,3] == 2 & full[i,4] == 1){
    no_noEnroll <- append(no_noEnroll, i)
  }
  if(full[i,3] == 1 & full[i,4] == 2){
    no_Enroll <- append(no_Enroll, i)
  }
}

outcome <- rbind(c(length(yes_Enroll), length(no_noEnroll)), c(length(no_Enroll), length(yes_noEnroll)))
rownames(outcome) <- c("Predicted Enrolled", "Predicted Not Enrolled")
colnames(outcome) <- c("Actual Enrolled", "Actual Not Enrolled")
misclass <- (outcome[1,2] + outcome[2,1])/15000

library(ggplot2)
library(dplyr)

## Focusing on Student's Home and Enrollment ##
admit_state <- select(admit, StateProxy, enrl_ind)

admit_state$enrl_ind <- as.character(admit_state$enrl_ind)
admit_state$enrl_ind[admit_state$enrl_ind == "0"] <- "No"
admit_state$enrl_ind[admit_state$enrl_ind == "1"] <- "Yes"
admit_state$enrl_ind <- as.factor(admit_state$enrl_ind)

state_sum <- summarize(group_by(admit_state, enrl_ind, StateProxy), n_combo = n())
state_sum

## Graphics - Stacked Bar Chart ##
stack_state <- ggplot(state_sum) + geom_bar(aes(y = n_combo, x = StateProxy, fill = enrl_ind), stat = "identity", position = "fill") + 
  labs(title = "Proportion of Students Enrolled, By Applicant's Residence", y = "Percentage", x = "Applicant's Residence", fill = "Did Student Enroll?") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))
stack_state

## Focusing on Number of Fees Paid and Enrollment ##
admit_fee <- select(admit, nFee, enrl_ind)

admit_fee$nFee <- as.factor(admit_fee$nFee)
admit_fee$enrl_ind <- as.character(admit_fee$enrl_ind)
admit_fee$enrl_ind[admit_fee$enrl_ind == "0"] <- "No"
admit_fee$enrl_ind[admit_fee$enrl_ind == "1"] <- "Yes"
admit_fee$enrl_ind <- as.factor(admit_fee$enrl_ind)

fee_sum <- summarize(group_by(admit_fee, enrl_ind, nFee), n_combo = n())
fee_sum

## Graphics - Stacked Bar Chart of Number of Fees Paid vs Enrolled ##
stack_fee <- ggplot(fee_sum) + geom_bar(aes(y = n_combo, x = nFee, fill = enrl_ind), stat = "identity", position = "fill") + 
  labs(title = "Enrollment Proportion By Number of Fees Paid", y = "Percentage", x = "Number of Fees Paid", fill = "Did Student Enroll?") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))
stack_fee

## Focusing on Number of On-Campus Events ("REG") Attended ##
admit_event <- select(admit, N_EVENT_ON_CAMPUS_REG, enrl_ind)

admit_event$N_EVENT_ON_CAMPUS_REG <- as.factor(admit_event$N_EVENT_ON_CAMPUS_REG)
admit_event$enrl_ind <- as.character(admit_event$enrl_ind)
admit_event$enrl_ind[admit_event$enrl_ind == "0"] <- "No"
admit_event$enrl_ind[admit_event$enrl_ind == "1"] <- "Yes"
admit_event$enrl_ind <- as.factor(admit_event$enrl_ind)

event_sum <- summarize(group_by(admit_event, enrl_ind, N_EVENT_ON_CAMPUS_REG), n_combo = n())
event_sum

## Graphics - Stacked Bar Chart of Number of Events vs Enrolled ##
stack_event <- ggplot(event_sum) + geom_bar(aes(y = n_combo, x = N_EVENT_ON_CAMPUS_REG, fill = enrl_ind), stat = "identity", position = "fill") + 
  labs(title = "Enrollment Proportions by Number of Regular On-Campus Events Attended", y = "Percentage", x = "Number of Events Attended", fill = "Did Student Enroll?") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))
stack_event

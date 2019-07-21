## Scholten Orthopedic Staff Surveys

Scholten_Survey <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Scholten/Ortho_Surveys.csv", header = TRUE)
Scholten_Survey$StaffNumber <- as.factor(Scholten_Survey$StaffNumber)

library(tidyr)
library(dplyr)
survey <- select(Scholten_Survey, Test = Pretest.or.Posttest, ID = StaffNumber, Question.1:Question.8)
survey_long <- gather(survey, Question, Correct, Question.1:Question.8, factor_key=TRUE)


total <- rbind(c(37, 28), c(3, 12)) ## not paired

paired <- rbind(c(27, 1), c(10, 2)) ## paired on questions

rownames(paired) <- c("Correct", "Incorrect")
colnames(paired) <- c("Correct", "Incorrect")

library(exact2x2)

fisher.test(total)            ## No pairing accounted for
exact2x2(total, midp = TRUE)

exact2x2(paired, paired = TRUE) ## Exact McNemar's Test, Matching on Question Pairs (not Scores, as in Non-Parametric Methods)
                                ## Statistically Significant (p = 0.01172)


#Objective 3: Knowledge increase (pre_posttestdata.xlsx)
pre<- c(5,6,6,7,4)
post <- c(8,8,8,7,6)
my_data <- data.frame(Group = rep(c("Pre", "Post"), each = 5), Score = c(pre,  post))
my_data$Group <- factor(my_data$Group,levels=levels(my_data$Group)[c(2,1)])

## nonparametric for paired t-test, exact test
library(coin)
wilcoxsign_test(Score ~ Group, data=my_data, dist=c("exact"),zero.method="Pratt")
wilcoxsign_test(Score ~ Group, data=my_data, dist=c("exact"),zero.method="Wilcoxon")

wilcoxsign_test(post ~ pre, dist=c("exact"),zero.method="Pratt")  ## The Right One


# You might read through this, I just skimmed it, but I think
# the exact wilcoxon is a good option here, there are ties.
#https://stat.ethz.ch/pipermail/r-help/2011-April/274931.html

#> wilcoxsign_test(Score ~ Group, data=my_data, dist=c("exact"),zero.method="Pratt")
#
#Exact Wilcoxon-Pratt Signed-Rank Test
#
#data:  y by x (pos, neg) 
#stratified by block
#Z = 2.8308, p-value = 0.001953
#alternative hypothesis: true mu is not equal to 0

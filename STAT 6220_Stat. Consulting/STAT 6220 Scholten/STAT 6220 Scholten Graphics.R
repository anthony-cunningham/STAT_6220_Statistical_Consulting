## Scholten Graphics

library(ggplot2)
library(tibble)
## Patient Data

## As a tibble
referred_tbl <- tibble(Year = c("2017", "2017", "2018", "2018"), 
                       Referred = c("Yes", "No", "Yes", "No"), 
                       n = c(5, 50, 9, 32))

## Side-By-Side Bar Chart of Referred Patients
side_ref <- ggplot(referred_tbl) + geom_bar(aes(y = n, x = Year, fill = Referred), stat = "identity", position = "dodge") + 
  labs(title = "Patients Referred for Osteoporosis Treatment, By Year", y = "Count", fill = "Referred") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000")) + 
  annotate("text", x = 0.80, y = 52.5, label = "Percentage of 'Yes': 9.09%") + 
  annotate("text", x = 1.80, y = 35, label = "Percentage of 'Yes': 21.95%")

pdf("side_ref.pdf")
side_ref
dev.off()

## Stacked Bar Chart of Proportion of Patients Referred
stack_ref <- ggplot(referred_tbl) + geom_bar(aes(y = n, x = Year, fill = Referred), stat = "identity", position = "fill") + 
  labs(title = "Proportion of Patients Referred for Osteoporosis Treatment, By Year", y = "Percent",
       fill = "Referred") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000")) + expand_limits(y = 1.05) + 
  annotate("text", x = 1, y = 1.05, label = "Proportion of 'Yes': 5/55") + 
  annotate("text", x = 2, y = 1.05, label = "Proportion of 'Yes': 9/41")

pdf("stack_ref.pdf")
stack_ref
dev.off()

################################################
ref_yes2018 <- referred[[1]]
ref_total2018 <- referred[[1]] + referred[[2]]
ref_perc2018 <- ref_yes2018/ref_total2018

ref_yes2017 <- referred[[3]]
ref_total2017 <- referred[[3]] + referred[[4]]
ref_perc2017 <- ref_yes2017/ref_total2017
################################################

## Treated Patients

## As a tibble
treated_tbl <- tibble(Year = c("2017", "2017", "2018", "2018"), 
                      Treated = c("Yes", "No", "Yes", "No"), 
                      n = c(5, 50, 6, 35))

## Side-By-Side Bar Chart of Treated Patients
side_trt <- ggplot(treated_tbl) + geom_bar(aes(y = n, x = Year, fill = Treated), stat = "identity", position = "dodge") + 
  labs(title = "Patients Treated for Osteoporosis, By Year", y = "Count", fill = "Treated") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000")) + 
  annotate("text", x = 0.80, y = 52.5, label = "Percentage of 'Yes': 9.09%") + 
  annotate("text", x = 1.80, y = 37.5, label = "Percentage of 'Yes': 14.63%")

pdf("side_trt.pdf")
side_trt
dev.off()

## Stacked Bar Chart for Proportion of Treated Patients
stack_trt <- ggplot(treated_tbl) + geom_bar(aes(y = n, x = Year, fill = Treated), stat = "identity", position = "fill") + 
  labs(title = "Proportion of Patients Treated for Osteoporosis, By Year", y = "Percent",
       fill = "Treated") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000")) + expand_limits(y = 1.05) + 
  annotate("text", x = 1, y = 1.05, label = "Proportion of 'Yes': 5/55") + 
  annotate("text", x = 2, y = 1.05, label = "Proportion of 'Yes': 6/41")

pdf("stack_ref.pdf")
stack_ref
dev.off()

################################################
trt_yes2018 <- treated[[1]]
trt_total2018 <- treated[[1]] + treated[[2]]
trt_perc2018 <- trt_yes2018/trt_total2018

trt_yes2017 <- treated[[3]]
trt_total2017 <- treated[[3]] + treated[[4]]
trt_perc2017 <- trt_yes2017/trt_total2017
################################################

## Survey Data
library(dplyr)
Scholten_Survey <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Scholten/Ortho_Surveys.csv", header = TRUE)
Scholten_Survey <- plyr::rename(Scholten_Survey, c("Pretest.or.Posttest" = "Test", "StaffNumber" = "ID"))
Scholten_Survey <- select(Scholten_Survey, Test, ID, Total)
Scholten_Survey$ID <- as.factor(Scholten_Survey$ID)
Scholten_Survey$Total[3] <- 6.05
Scholten_Survey$Total[4] <- 8.05

## Slope Graph of Staff Test Scores
Scholten_Survey$Test <- factor(Scholten_Survey$Test,levels=levels(Scholten_Survey$Test)[c(2,1)])
slope_survey <- ggplot(Scholten_Survey, aes(x = Test, y = Total, group = ID)) + 
  geom_line(aes(color = Scholten_Survey$ID), size = 1) + 
  geom_point() + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  labs(title = "Staff Osteoporosis Knowledge Scores\nPre vs. Post-Implementation", 
       y = "Score (Out of 8)", 
       color = "Staff ID") +  
       #caption = "Note: 2 Staff Members Had Scores of '6' Before and '8' After") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        text = element_text(size = 12))

pdf("slope_survey.pdf")
slope_survey
dev.off()

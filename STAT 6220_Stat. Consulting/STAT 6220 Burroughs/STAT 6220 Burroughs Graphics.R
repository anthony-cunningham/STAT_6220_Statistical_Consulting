# Burroughs Patient Referral Data #

## Packages Used: "dplyr", "ggplot2"
Burroughs <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Burroughs/STAT 6220 Burroughs_2017-18 Combined.csv", 
                      header = TRUE)
new_old <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Burroughs/STAT 6220 Burroughs_2017-18_New_Old.csv", 
                    header=TRUE)
##  "Year" variable is read as an integer initially; use this command to tell R to read it as a factor with 2 levels
Burroughs$Year <- as.character(Burroughs$Year)
Burroughs$Year[Burroughs$Year == "2017"] <- "Oct-Dec 2017"
Burroughs$Year[Burroughs$Year == "2018"] <- "Oct-Dec 2018"
Burroughs$Year <- as.factor(Burroughs$Year)

new_old$Year <- as.character(new_old$Year)
new_old$Year[new_old$Year == "2017"] <- "Oct-Dec 2017"
new_old$Year[new_old$Year == "2018"] <- "Oct-Dec 2018"
new_old$Year <- as.factor(new_old$Year)

library(dplyr)
##  use this to obtain counts for each combo of factor levels
within90_table <- count(Burroughs, Year, within90)
Labs <- count(Burroughs, Year, Complete.Labs)
new <- count(new_old, Year, New)

library(ggplot2)
#library(extrafont)
#font_import(pattern = "[A/a]rial")
#loadfonts(device = "win")
## Graphics Using Lab Data

## Stacked Bar Chart of "Complete.Labs" Frequencies
bar_lab <- ggplot(Labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "stack") + 
    labs(title = "Count of Complete vs Incomplete Lab Sets \n Among New Patients, By Year", y = "Count", fill = "Complete Labs") + 
    theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
    scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("bar_lab.pdf")
bar_lab
dev.off()

## Side-By_Side Bar Chart of "Complete.Labs" Frequencies
side_lab <- ggplot(Labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "dodge") + 
    labs(title = "Count of Complete vs Incomplete Lab Sets \n Among New Patients, By Year", y = "Count", fill = "Complete Labs") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
    scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("side_lab.pdf")
side_lab
dev.off()

## Stacked Bar Chart of Proportion of Patients with Complete Lab Set
stack_lab <- ggplot(Labs) + geom_bar(aes(y = n, x = Year, fill = Complete.Labs), stat = "identity", position = "fill") + 
    labs(title = "Proportion of Complete vs Incomplete Lab Sets \n Among New Patients, By Year", 
         y = "Percent    ",
         fill = "Complete Labs", 
         tag = "Figure 1.\n") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), 
          axis.title.y = element_text(angle = 0, vjust = 0.54), 
          plot.tag = element_text(size = 12), 
          plot.tag.position = "top", 
          text = element_text(size = 12)) +
    scale_fill_manual(values = c("#ccb85f","#000000")) + expand_limits(y = 1.05) + 
    annotate("text", x = 1, y = 1.05, label = "Proportion of 'Yes': 4/25") + 
    annotate("text", x = 2, y = 1.05, label = "Proportion of 'Yes': 11/15")

pdf("stack_lab.pdf")
stack_lab
dev.off()

just_Year <- plyr::revalue(Labs$Year, c("Oct-Dec 2017"="2017", "Oct-Dec 2018"="2018"))
## Donut Plot
donut_lab <- ggplot(Labs) + geom_bar(aes(y = n, x = 1, fill = Complete.Labs), stat = "identity", position = "fill") + coord_polar(theta = "y") + 
  facet_wrap(~ Year) + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                             panel.border = element_blank(), strip.background = element_blank(), 
                             strip.text = element_blank()) + xlim(0, 1.5) + 
  geom_text(aes(x = 0, y = 0, label = as.factor(just_Year)), size = 5, fontface = "bold") + 
  labs(title = "Complete vs Incomplete Lab Sets: \n Proportion of New Patients, By Year", 
       subtitle = "Note: Patient Data Collected October-December Within Each Year", 
       fill = "Complete Labs",
       tag = "Figure 1.\n") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        plot.subtitle = element_text(size = 12, hjust = 0.5), 
        plot.tag = element_text(size = 12), 
        plot.tag.position = "top", 
        legend.position = "bottom", 
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("donut_lab.pdf")
donut_lab
dev.off()

## Graphics Using within90 Data

## Stacked Bar Chart of within90 Frequencies
bar_within90 <- ggplot(within90_table) + geom_bar(aes(y = n, x = Year, fill = within90), stat = "identity") + ylab("Count") +
  ggtitle("Count of New Patients Seen vs Not Seen Within 90 Days \n of Initial ADT Treatment, By Year") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("bar_within90.pdf")
bar_within90
dev.off()

## Side-By-Side Bar Chart of within90 Frequencies
side_within90 <- ggplot(within90_table) + geom_bar(aes(y = n, x = Year, fill = within90), stat = "identity", position = "dodge") + 
    ylab("Count") +
    ggtitle("Count of New Patients Seen vs Not Seen Within 90 Days \n of Initial ADT Treatment, By Year") +
    theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
    scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("side_within90.pdf")
side_within90
dev.off()

## Stacked Bar Chart of Proportion of Patients Seen by NP Within 90 Days of Initial ADT Treatment
stack_within90 <- ggplot(within90_table) + geom_bar(aes(y = n, x = Year, fill = within90), stat = "identity", position = "fill") + 
  ylab("Percent  ") + 
  ggtitle("Proportion of New Patients Seen Within 90 Days \n of Initial ADT Treatment, By Year") + 
  labs(tag = "Figure 2.\n") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        axis.title.y = element_text(angle = 0, vjust = 0.54), 
        plot.tag = element_text(size = 12), 
        plot.tag.position = "top", 
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000")) + expand_limits(y = 1.05) + 
  annotate("text", x = 1, y = 1.05, label = "Proportion of 'Yes': 9/25") + 
  annotate("text", x = 2, y = 1.05, label = "Proportion of 'Yes': 8/15")

pdf("stack_within90.pdf")
stack_within90
dev.off()

just_Year <- plyr::revalue(within90_table$Year, c("Oct-Dec 2017"="2017", "Oct-Dec 2018"="2018"))
## Donut Plot
donut_within90 <- ggplot(within90_table) + geom_bar(aes(y = n, x = 1, fill = within90), stat = "identity", position = "fill") + coord_polar(theta = "y") + 
  facet_wrap(~ Year) + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                             panel.border = element_blank(), strip.background = element_blank(), 
                             strip.text = element_blank()) + xlim(0, 1.5) + 
  geom_text(aes(x = 0, y = 0, label = as.factor(just_Year)), size = 5, fontface = "bold") + 
  labs(title = "Proportion of New Patients Seen Within 90 Days \n of Initial ADT Treatment, By Year", 
       subtitle = "Note: Patient Data Collected October-December Within Each Year", 
       tag = "Figure 2.\n") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        plot.subtitle = element_text(size = 12, hjust=0.5), 
        plot.tag = element_text(size = 12), 
        plot.tag.position = "top", 
        legend.position = "bottom", 
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("donut_within90.pdf")
donut_within90
dev.off()

## Graphic Using "New/Old" Data

bar_new <- ggplot(new) + geom_bar(aes(y = n, x = Year, fill = New), stat = "identity", position = "stack") + ylab("Count") + 
  ggtitle("Count of New vs Returning Patients, By Year") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("bar_new.pdf")
bar_new
dev.off()

just_Year <- plyr::revalue(new$Year, c("Oct-Dec 2017"="2017", "Oct-Dec 2018"="2018"))
# Donut Graph

donut_new <- ggplot(new) + geom_bar(aes(y = n, x = 1, fill = New), stat = "identity", position = "fill") + coord_polar(theta = "y") + 
  facet_wrap(~ Year) + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
                             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                             panel.border = element_blank(), strip.background = element_blank(), 
                             strip.text = element_blank()) + xlim(0, 1.5) + 
  geom_text(aes(x = 0, y = 0, label = as.factor(just_Year)), size = 5, fontface = "bold") + 
  labs(title = "Proportion of New vs Returning Patients, By Year", 
       subtitle = "Note: Patient Data Collected October-December Within Each Year\n\nProportion of 'Yes': 24/69                      Proportion of 'Yes': 15/76") + 
  theme(plot.title = element_text(size = 14, hjust = 0.5), 
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        text = element_text(size = 12)) +
  scale_fill_manual(values = c("#ccb85f","#000000"))

pdf("donut_new.pdf")
donut_new
dev.off()

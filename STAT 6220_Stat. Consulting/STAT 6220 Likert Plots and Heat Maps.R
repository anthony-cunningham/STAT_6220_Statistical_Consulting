  ## Assignment 5: Likert-Scale Response Graphics ##

#1
library(tibble)
library(sjlabelled)
dt <- read_spss("likert.sav")
is_tibble(dt)
dim(dt)
names(dt)
str(dt)
print(dt)

#2
dt[[2]]
dt[["Q1a"]]
get_labels(dt[[2]])
get_label(dt[[2]])

#3
table(as_label(dt[[2]]))
Q1a_vector <- as.vector(table(as_label(dt[[2]])))
par(las = 2)
par(mar=c(5,8,4,2))
Q1a_Count <- barplot(Q1a_vector, names.arg = get_labels(dt[[2]]), horiz = TRUE, space = 1.5, cex.names = 0.75, axis.lty = 1)

#4
sum_Q1a <- sum(Q1a_vector)
Q1a_Percent <- Q1a_vector/sum_Q1a
par(las = 2)
par(mar=c(5,8,4,2))
Q1a_Bar_Percent <- barplot(Q1a_Percent, names.arg = get_labels(dt[[2]]), horiz = TRUE, space = 1.5, cex.names = 0.75, axis.lty = 1)

#5
dtImp <- data.frame(as_label(dt[[2]]),
                    as_label(dt[[4]]),
                    as_label(dt[[6]]),
                    as_label(dt[[8]]))
library(likert)
dtImpLik <- likert(dtImp)
plot(dtImpLik)

#6
colnames(dtImp) <- c(get_label(dt[[2]]),
                     get_label(dt[[4]]),
                     get_label(dt[[6]]),
                     get_label(dt[[8]]))
dtImpLik <- likert(dtImp)
plot(dtImpLik, colors = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c"))

#7
library(stringr)
common <- word(colnames(dtImp), -5, -1)
colnames(dtImp) <- substring(colnames(dtImp), 1, regexpr("-", colnames(dtImp)) - 2)
dtImpLik <- likert(dtImp)
plot(dtImpLik, colors = c("#eceabb", "#d2cf82", "#a7a565", "#51514e", "#0d0d00")) + ggtitle(common[[1]])

#8
dtHelp <- data.frame(as_label(dt[[3]]),
                    as_label(dt[[5]]),
                    as_label(dt[[7]]),
                    as_label(dt[[9]]))
colnames(dtHelp) <- c(get_label(dt[[3]]),
                      get_label(dt[[5]]),
                      get_label(dt[[7]]),
                      get_label(dt[[9]]))
common2 <- word(colnames(dtHelp), -9, -1)
colnames(dtHelp) <- substring(colnames(dtHelp), 1, regexpr("-", colnames(dtHelp)) - 2)
dtHelpLik <- likert(dtHelp)
plot(dtHelpLik, colors = c("#ffff99", "#ffff33", "#ffcc33", "#ff9999", "#ff6633")) + ggtitle(common2[1])

# PAIRED DATA #

#9
# Create new dataset that combines both sets of questions
dtImpNum <- data.frame(a=dt[[2]], b=dt[[4]], c=dt[[6]], d=dt[[8]])
dtHelpNum <- data.frame(a=dt[[3]], b=dt[[5]], c=dt[[7]], d=dt[[9]])
dtAll <- rbind(dtImpNum, dtHelpNum)
Activities <- colnames(dtHelp)
colnames(dtAll) <- Activities

# Coerce all vars to factors AND coerce all to have the same levels
# you'll get an error from likert() otherwise
library(forcats)
for (i in 1:ncol(dtAll)){
  x <- as.factor(dtAll[,i])
  x <- fct_expand(x, as.character(1:5))
  x <- fct_relevel(x, as.character(1:5))
  dtAll[,i] <- x
}

# Include grouping var for two combined data sets:
dtAll$Group <- factor(c(rep("IMPORTANCE",nrow(dtImpNum)),
                        rep("HELPS", nrow(dtHelpNum))))

# Plot graphic showing paired grouping
forPlot <- likert(dtAll[,1:(ncol(dtAll)-1)], grouping = dtAll$Group)
plot(forPlot)

#10
charLegend <- c("Not at all", "Not really", "Somewhat", "Very Much", "Absolutely")
dtAll[[1]] = recode(dtAll[[1]], from=c(1, 2, 3, 4, 5), to=charLegend)
dtAll[[2]] = recode(dtAll[[2]], from=c(1, 2, 3, 4, 5), to=charLegend)
dtAll[[3]] = recode(dtAll[[3]], from=c(1, 2, 3, 4, 5), to=charLegend)
dtAll[[4]] = recode(dtAll[[4]], from=c(1, 2, 3, 4, 5), to=charLegend)

dtAll[[1]] = as.factor(dtAll[[1]])
dtAll[[2]] = as.factor(dtAll[[2]])
dtAll[[3]] = as.factor(dtAll[[3]])
dtAll[[4]] = as.factor(dtAll[[4]])

dtAll[[1]] = factor(dtAll[[1]], levels=charLegend)
dtAll[[2]] = factor(dtAll[[2]], levels=charLegend)
dtAll[[3]] = factor(dtAll[[3]], levels=charLegend)
dtAll[[4]] = factor(dtAll[[4]], levels=charLegend)

forPlot <- likert(dtAll[,1:(ncol(dtAll)-1)], grouping = dtAll$Group)
plot(forPlot)

#11
# Formatting Data for a Heat Map (Individuals' Bivariate Responses) - "Quiet Work" activity
i <- 1
dt_for_heat_two <- reshape2::melt(table(dtImp[,i],dtHelp[,i]))
colnames(dt_for_heat_two) <- c("Importance", "Helps", "Freq")
dt_for_heat_two$percent <- round(dt_for_heat_two$Freq/sum(dt_for_heat_two$Freq),2)
dt_for_heat_two$perclabel <- paste0(100*dt_for_heat_two$percent,"%")

ggplot(dt_for_heat_two, aes(Importance, Helps)) + geom_tile(aes(fill = percent), color = "black") + 
  scale_fill_gradient(low = "lightyellow", high = "gold4") + ggtitle(Activities[i]) +
  geom_text(aes(label = perclabel), size = 6) + 
  #geom_text(aes(label = Freq), size = 6) + 
  xlab("Importance") + ylab("Helps") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10), plot.title = element_text(size = 10),
        legend.title = element_text(size = 10), legend.key.size = unit(.85, 'lines'), legend.text = element_text(size = 10))

#12
# Changing Likert Response Levels to Low, Medium, High
library(plyr)
lik_response_three <- c("Low", "Low", "Medium", "High", "High")

dtHelp[[1]] <- mapvalues(dtHelp[[1]], levels(dtHelp[[1]]), lik_response_three)
dtHelp[[2]] <- mapvalues(dtHelp[[2]], levels(dtHelp[[2]]), lik_response_three)
dtHelp[[3]] <- mapvalues(dtHelp[[3]], levels(dtHelp[[3]]), lik_response_three)
dtHelp[[4]] <- mapvalues(dtHelp[[4]], levels(dtHelp[[4]]), lik_response_three)

dtImp[[1]] <- mapvalues(dtImp[[1]], levels(dtImp[[1]]), lik_response_three)
dtImp[[2]] <- mapvalues(dtImp[[2]], levels(dtImp[[2]]), lik_response_three)
dtImp[[3]] <- mapvalues(dtImp[[3]], levels(dtImp[[3]]), lik_response_three)
dtImp[[4]] <- mapvalues(dtImp[[4]], levels(dtImp[[4]]), lik_response_three)

# Recreating Heat Map for "Quiet Work" activity
i <- 1
dt_for_heat_two <- reshape2::melt(table(dtImp[,i],dtHelp[,i]))
colnames(dt_for_heat_two) <- c("Importance", "Helps", "Freq")
dt_for_heat_two$percent <- round(dt_for_heat_two$Freq/sum(dt_for_heat_two$Freq),2)
dt_for_heat_two$perclabel <- paste0(100*dt_for_heat_two$percent,"%")

quiet_plot <- ggplot(dt_for_heat_two, aes(Importance, Helps)) + geom_tile(aes(fill = percent), color = "black") + 
  scale_fill_gradient(low = "lightyellow", high = "gold4") + ggtitle(Activities[i]) +
  geom_text(aes(label = perclabel), size = 6) + 
  #geom_text(aes(label = Freq), size = 6) + 
  xlab("Importance") + ylab("Helps") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10), plot.title = element_text(size = 10),
        legend.title = element_text(size = 10), legend.key.size = unit(.85, 'lines'), legend.text = element_text(size = 10))
quiet_plot

#13
# Heat Map for "Using the Telephone" activity
i <- 2
dt_for_heat_three <- reshape2::melt(table(dtImp[,i],dtHelp[,i]))
colnames(dt_for_heat_three) <- c("Importance", "Helps", "Freq")
dt_for_heat_three$percent <- round(dt_for_heat_three$Freq/sum(dt_for_heat_three$Freq),2)
dt_for_heat_three$perclabel <- paste0(100*dt_for_heat_three$percent,"%")

phone_plot <- ggplot(dt_for_heat_three, aes(Importance, Helps)) + geom_tile(aes(fill = percent), color = "black") + 
  scale_fill_gradient(low = "lightyellow", high = "gold4") + ggtitle(Activities[i]) +
  geom_text(aes(label = perclabel), size = 6) + 
  #geom_text(aes(label = Freq), size = 6) + 
  xlab("Importance") + ylab("Helps") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10), plot.title = element_text(size = 10),
        legend.title = element_text(size = 10), legend.key.size = unit(.85, 'lines'), legend.text = element_text(size = 10))

# Heat Map for "Meeting with One Person" activity
i <- 3
dt_for_heat_four <- reshape2::melt(table(dtImp[,i],dtHelp[,i]))
colnames(dt_for_heat_four) <- c("Importance", "Helps", "Freq")
dt_for_heat_four$percent <- round(dt_for_heat_four$Freq/sum(dt_for_heat_four$Freq),2)
dt_for_heat_four$perclabel <- paste0(100*dt_for_heat_four$percent,"%")

mtg_one_plot <- ggplot(dt_for_heat_four, aes(Importance, Helps)) + geom_tile(aes(fill = percent), color = "black") + 
  scale_fill_gradient(low = "lightyellow", high = "gold4") + ggtitle(Activities[i]) +
  geom_text(aes(label = perclabel), size = 6) + 
  #geom_text(aes(label = Freq), size = 6) + 
  xlab("Importance") + ylab("Helps") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10), plot.title = element_text(size = 10),
        legend.title = element_text(size = 10), legend.key.size = unit(.85, 'lines'), legend.text = element_text(size = 10))

# Heat Map for "Scheduled Group Meeting" Activity
i <- 4
dt_for_heat_five <- reshape2::melt(table(dtImp[,i],dtHelp[,i]))
colnames(dt_for_heat_five) <- c("Importance", "Helps", "Freq")
dt_for_heat_five$percent <- round(dt_for_heat_five$Freq/sum(dt_for_heat_five$Freq),2)
dt_for_heat_five$perclabel <- paste0(100*dt_for_heat_five$percent,"%")

mtg_group_plot <- ggplot(dt_for_heat_five, aes(Importance, Helps)) + geom_tile(aes(fill = percent), color = "black") + 
  scale_fill_gradient(low = "lightyellow", high = "gold4") + ggtitle(Activities[i]) +
  geom_text(aes(label = perclabel), size = 6) + 
  #geom_text(aes(label = Freq), size = 6) + 
  xlab("Importance") + ylab("Helps") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10), plot.title = element_text(size = 10),
        legend.title = element_text(size = 10), legend.key.size = unit(.85, 'lines'), legend.text = element_text(size = 10))
gridExtra::grid.arrange(quiet_plot, phone_plot, mtg_one_plot, mtg_group_plot, nrow = 2, ncol = 2)

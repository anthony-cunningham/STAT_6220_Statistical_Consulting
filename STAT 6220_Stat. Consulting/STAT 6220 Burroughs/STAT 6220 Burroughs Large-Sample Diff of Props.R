# Burroughs Patient Referral Data #
    
Burroughs <- read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Burroughs/STAT 6220 Burroughs_2017-18 Combined.csv", header = TRUE)
Burroughs$Year <- as.factor(Burroughs$Year)
    # Create a "Labs" Contingency Table #

##  Note: table shows Year*Labs:                    Column 1 = "Yes"  Column 2 = "No"
##                                    Row 1 = 2018          11               4
##                                    Row 2 = 2017           4              21
labs <- rbind(c(11, 4), c(4, 21))

        # Difference of Proportions #
p2018 = 11/15
p2017 = 4/25
# Wald #
SE_Wald = sqrt(((p2018*(1-p2018))/15) + ((p2017*(1-p2017))/25))
(TestStat_Wald = (p2018 - p2017)/SE_Wald)
(p_value = 1 - pnorm(TestStat_Wald))
(CI_Wald = rbind(((p2018-p2017) - abs(qnorm(0.025))*SE_Wald), ((p2018-p2017) + abs(qnorm(0.025))*SE_Wald)))
# Score #
p_Pooled = (15*p2018 + 25*p2017)/40
SE_Pooled = sqrt(p_Pooled*(1-p_Pooled)*((1/15) + (1/25)))
(TestStat_Score = (p2018-p2017)/SE_Pooled)
(p_value = 1 - pnorm(TestStat_Score))
(CI_Score = rbind(((p2018-p2017) - abs(qnorm(0.025))*SE_Pooled), ((p2018-p2017) + abs(qnorm(0.025))*SE_Pooled)))

        # Odds Ratio #
(theta_hat = (p2018/(1-p2018))/(p2017/(1-p2017)))
SE = sqrt((1/11)+(1/4)+(1/4)+(1/21))
CI_ln = rbind((log(theta_hat)-abs(qnorm(0.025))*SE), (log(theta_hat)+abs(qnorm(0.025))*SE))
(CI_theta = exp(CI_ln))

        # Pearson's Chi-Sq Test of Ind #
chisq.test(labs, correct=FALSE)   # No continuity correction
chisq.test(labs)                  # With continuity correction

        # Likelihood Ratio Test of Ind #
n = sum(labs)                                    # total sample size
row = rowSums(labs)                              # row total
col = colSums(labs)                              # col total
ept = outer(row, col, "*")/n                     # expected counts
(G_squared = 2*sum(labs*log(labs/ept)))          # Calculated G^2 Statistic
(1-pchisq(G_squared, 1))                           # Calculates p-value

    # Create a "Within90" Contingency Table #

##  Note: table shows Year*within90:                Column 1 = "Yes"  Column 2 = "No"
##                                    Row 1 = 2018          8               7
##                                    Row 2 = 2017          9              16

within90 <- rbind(c(8, 7), c(9, 16))

        # Difference of Proportions #
p2018 = 8/15
p2017 = 9/25
# Wald #
SE_Wald = sqrt(((p2018*(1-p2018))/15) + ((p2017*(1-p2017))/25))
(TestStat_Wald = (p2018 - p2017)/SE_Wald)
(p_value = 1 - pnorm(TestStat_Wald))
(CI_Wald = rbind(((p2018-p2017) - abs(qnorm(0.025))*SE_Wald), ((p2018-p2017) + abs(qnorm(0.025))*SE_Wald)))
# Score #
p_Pooled = (15*p2018 + 25*p2017)/40
SE_Pooled = sqrt(p_Pooled*(1-p_Pooled)*((1/15) + (1/25)))
(TestStat_Score = (p2018-p2017)/SE_Pooled)
(p_value = 1 - pnorm(TestStat_Score))
(CI_Score = rbind(((p2018-p2017) - abs(qnorm(0.025))*SE_Pooled), ((p2018-p2017) + abs(qnorm(0.025))*SE_Pooled)))

        # Odds Ratio #
(theta_hat = (p2018/(1-p2018))/(p2017/(1-p2017)))
SE = sqrt((1/8)+(1/7)+(1/9)+(1/16))
CI_ln = rbind((log(theta_hat)-abs(qnorm(0.025))*SE), (log(theta_hat)+abs(qnorm(0.025))*SE))
(CI_theta = exp(CI_ln))

        # Pearson Chi-Sq Test of Ind #
chisq.test(within90, correct=FALSE)   # No continuity correction
chisq.test(within90)                  # With continuity correction

        # Likelihood Ratio Test of Ind #

n = sum(within90)                                    # total sample size
row = rowSums(within90)                              # row total
col = colSums(within90)                              # col total
ept = outer(row, col, "*")/n                         # expected counts
(G_squared = 2*sum(within90*log(within90/ept)))      # Calculated G^2 Statistic
(1-pchisq(G_squared, 1))                               # Calculates p-value

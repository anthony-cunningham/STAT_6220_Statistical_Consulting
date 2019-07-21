# Burroughs Patient Referral Data #
    
Burroughs <- read.csv("H:/STAT 6220 Burroughs/STAT 6220 Burroughs 2017-18 Patient Data.csv", header = TRUE)
Burroughs$Year <- as.factor(Burroughs$Year)

# Create Contingency Tables #
labs <- rbind(c(11, 4), c(4, 21))
labs_cool <- rbind(c(11, 4, 15), c(4, 21, 25), c(15, 25, 40))
colnames(labs_cool) <- c("2018", "2017", "Total")
rownames(labs_cool) <- c("Complete Set", "Incomplete Set", "Total")

within90 <- rbind(c(8, 7), c(9, 16))
within90_cool <- rbind(c(8, 7, 15), c(9, 16, 25), c(15, 25, 40))
colnames(within90_cool) <- c("2018", "2017", "Total")
rownames(within90_cool) <- c("Within 90", "Not Within 90", "Total")


library(exact2x2)

fisher.test(labs)
exact2x2(labs, midp = TRUE)
fisher.test(labs, alternative = "greater")
exact2x2(labs, midp = TRUE, alternative = "greater")


fisher.test(within90)
exact2x2(within90, midp = TRUE)
fisher.test(within90, alternative = "greater")
exact2x2(within90, midp = TRUE, alternative = "greater")

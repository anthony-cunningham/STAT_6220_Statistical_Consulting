## Scholten Patient Data

referred <- rbind(c(9, 5), c(32, 50))
treated <- rbind(c(6, 5), c(35, 50))

library(exact2x2)

fisher.test(referred)
exact2x2(referred, midp = TRUE)
fisher.test(referred, alternative = "greater")
exact2x2(referred, midp = TRUE, alternative = "greater") ## Statistically Significant (p = 0.04544)

fisher.test(treated)
exact2x2(treated, midp = TRUE)
fisher.test(treated, alternative = "greater")
exact2x2(treated, midp = TRUE, alternative = "greater")

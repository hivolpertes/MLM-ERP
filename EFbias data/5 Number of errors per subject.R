require(ggplot2)

# Look at number of errors committed by each subject

numerr = read.delim("EFbias data/ErrorsPerCondition.txt")

hist(numerr$sum, xlab = "Total number of errors per subject")
hist(numerr$BG, xlab = "Number of Black-gun errors per subject")
hist(numerr$BT, xlab = "Number of Black-tool errors per subject")
hist(numerr$WG, xlab = "Number of White-gun errors per subject")
hist(numerr$WT, xlab = "Number of White-tool errors per subject")


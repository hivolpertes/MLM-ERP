require(lme4)
require(lmerTest)
require(dplyr)

dat = read.delim("./EFbias data/4 quantified data/ERN_noMiss_noArt_RTaccept_noBS.txt")

errDat = dat %>% filter(Accuracy == "incorrect")

# add effect codes for categorical variables
errDat$Race.e = NA
errDat$Race.e[errDat$Race == "Black"] = -1
errDat$Race.e[errDat$Race == "White"] = 1

errDat$Object.e = NA
errDat$Object.e[errDat$Object == "gun"] = -1
errDat$Object.e[errDat$Object == "tool"] = 1

# Not including trial -----------------------------------------------------

# Model specification: start with maximal model
m1 = lmer(MeanAmp ~ 1 + (Race.e*Object.e|Subject) + (1|Electrode:Subject), dat = errDat)
summary(m1)

# Final model:
mF = lmer(MeanAmp ~ Race.e*Object.e + (Race.e*Object.e|Subject) + (1|Electrode:Subject), dat = errDat)
summary(mF)


# Including trial ---------------------------------------------------------

# Model specification: start with maximal model
m1 = lmer(MeanAmp ~ 1 + (Race.e*Object.e*Trial|Subject) + (1|Electrode:Subject), dat = errDat)
# Doesn't converge

m2 = lmer(MeanAmp ~ 1 + (Race.e+Object.e+Trial|Subject) + (1|Electrode:Subject), dat = errDat)
# Doesn't converge

m3 = lmer(MeanAmp ~ 1 + (Object.e+Trial|Subject) + (1|Electrode:Subject), dat = errDat)
# Doesn't converge

m4 = lmer(MeanAmp ~ 1 + (Trial|Subject) + (1|Electrode:Subject), dat = errDat)
# Doesn't converge

# Final model:
mF = lmer(MeanAmp ~ Race.e*Object.e*Trial + (Race.e*Object.e|Subject) + (1|Electrode:Subject), dat = errDat)
summary(mF)

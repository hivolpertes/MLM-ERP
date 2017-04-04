require(lme4)
require(lmerTest)

dat = read.delim("./N170 data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt")

# add effect codes for categorical variables
dat$Race.e = NA
dat$Race.e[dat$Race == "Black"] = -1
dat$Race.e[dat$Race == "White"] = 1

dat$Fix.e = NA
dat$Fix.e[dat$Fix == "eyes"] = -1
dat$Fix.e[dat$Fix == "forehead"] = 1

# rescale trial
dat$Trial.begin = (dat$Trial-1)/25.5
# shift trial to look at fixed effects at end of task as well
dat$Trial.end = dat$Trial.begin - 10


# Model with fixation as predictor
begin.e = lmer(MeanAmp ~ Race.e*Fix.e*Trial.begin + (Race.e*Fix.e|Subject) + (1|Electrode:Subject), dat = dat)


sink(file = "./N170 data/ModelOutputWithFixation.txt")
summary(begin.e)
"________________________________________________________________________________________________"
coef(begin.e)
sink()


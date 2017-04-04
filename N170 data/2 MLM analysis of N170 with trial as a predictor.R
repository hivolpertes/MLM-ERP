require(lme4)
require(lmerTest)

path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
filepath = "Analyses with full sample/1 ERP quant data/2 Group time intervals/Changing effect over trial- N170/"

# read in cat data
catdat = read.delim(paste(path, "Quantified data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))

catdat$rescaleTrial = catdat$Trial/25.6

# Random effects for Subject but only intercept for electrode
m1 = lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

sink(file = paste(filepath,"M3a_N170_Cat_TBTdat_RescaledTrialBegin.txt", sep=""))
summary(m1)
"________________________________________________________________________________________________"
coef(m1)
sink()

# # Adjust Trial variable to look at fixed effects at the end of cat task
catdat$TrialEnd = catdat$rescaleTrial - 10

# Random effects for Subject but only intercept for electrode

m1.2 = lmer(MeanAmp ~ Race * Fix * TrialEnd + (Race*Fix|Subject) + (1|Electrode:Subject), data = catdat)

sink(file = paste(filepath,"M3b_N170_Cat_TBTdat_RescaledTrialEnd.txt", sep=""))
summary(m1.2)
"________________________________________________________________________________________________"
coef(m1.2)
sink()



# read in eval data
evaldat = read.delim(paste(path, "Quantified data/Eval_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))
evaldat$rescaleTrial = evaldat$Trial/51.2

# Random effects for Subject but only intercept for electrode
m2 = lmer(MeanAmp ~ Race * Fix * rescaleTrial + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(filepath,"M4a_N170_Eval_TBTdat_RescaledTrialBegin.txt", sep=""))
summary(m2)
"________________________________________________________________________________________________"
coef(m2)
sink()

# # Adjust Trial variable to look at fixed effects at the end of cat task
evaldat$TrialEnd = evaldat$rescaleTrial - 10

# Random effects for Subject but only intercept for electrode

m2.2 = lmer(MeanAmp ~ Race * Fix * TrialEnd + (Race*Fix|Subject) + (1|Electrode:Subject), data = evaldat)

sink(file = paste(filepath,"M4b_N170_Eval_TBTdat_RescaledTrialEnd.txt", sep=""))
summary(m2.2)
"________________________________________________________________________________________________"
coef(m2.2)
sink()



# Take out fixation for eval task

m2.3 = lmer(MeanAmp ~ Race * TrialEnd + (Race|Subject) + (1|Electrode:Subject), data = evaldat)
summary(m2.3)

m2.4 = lmer(MeanAmp ~ Race * rescaleTrial + (Race|Subject) + (1|Electrode:Subject), data = evaldat)
summary(m2.4)


# Take out fixation for cat task

m1.3 = lmer(MeanAmp ~ Race * TrialEnd + (Race|Subject) + (1|Electrode:Subject), data = catdat)
summary(m1.3)

sink(file = paste(filepath,"M3c_N170_Eval_TBTdat_RescaledTrialEnd_noFix.txt", sep=""))
summary(m1.3)
"________________________________________________________________________________________________"
coef(m1.3)
sink()


m1.4 = lmer(MeanAmp ~ Race * rescaleTrial + (Race|Subject) + (1|Electrode:Subject), data = catdat)
summary(m1.4)
 
sink(file = paste(filepath,"M3c_N170_Eval_TBTdat_RescaledTrialBegin_noFix.txt", sep=""))
summary(m1.4)
"________________________________________________________________________________________________"
coef(m1.4)
sink()

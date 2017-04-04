require(dplyr)
require(ggplot2)

dat = read.delim("./EFbias data/4 quantified data/ERN_noMiss_noArt_RTaccept_noBS.txt") %>% filter(Accuracy == "incorrect")


errors = read.delim("ErrorsPerCondition.txt")
errors$Total = errors$WT + errors$WG + errors$BT + errors$BG

# All errors together -----------------------------------------------------

dat.sum = select(dat, Subject, MeanAmp) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

forCor = cbind(errors, dat.sum)

ggplot(aes(Total, MeanAmp), data = forCor) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("All errors")

cor.test(forCor$Total, forCor$MeanAmp)



# Just Black-tool errors --------------------------------------------------
BT = filter(dat, Condition == "Black_tool") %>% 
  select(Subject, MeanAmp) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

forCorBT = cbind(errors, BT)

ggplot(aes(BT, MeanAmp), data = forCorBT) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("Just Black-tool errors")

cor.test(forCorBT$BT, forCorBT$MeanAmp)

# Just Black-gun errors --------------------------------------------------
BG = filter(dat, Condition == "Black_gun") %>% 
  select(Subject, MeanAmp) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

forCorBG = cbind(errors, BG)

ggplot(aes(BG, MeanAmp), data = forCorBG) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("Just Black-gun errors")

cor.test(forCorBG$BG, forCorBG$MeanAmp)

# Just White-tool errors --------------------------------------------------
WT = filter(dat, Condition == "White_tool") %>% 
  select(Subject, MeanAmp) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

forCorWT = cbind(errors, WT)

ggplot(aes(WT, MeanAmp), data = forCorWT) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("Just White-tool errors")

cor.test(forCorWT$WT, forCorWT$MeanAmp)

# Just White-gun errors --------------------------------------------------
WG = filter(dat, Condition == "White_gun") %>% 
  select(Subject, MeanAmp) %>% 
  group_by(Subject) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

forCorWG = cbind(errors, WG)

ggplot(aes(WG, MeanAmp), data = forCorWG) +
  geom_point() +
  geom_smooth(method="lm") +
  ggtitle("Just White-gun errors")

cor.test(forCorWG$WG, forCorWG$MeanAmp)

# Graph all together -----------------------

datcond = select(dat, -Race, -Object, -Accuracy, -Electrode, -Trial) %>% 
  group_by(Subject, Condition) %>% 
  summarise_each(funs(mean(., na.rm=TRUE))) %>% 
  as.data.frame()

errors = rename(errors, White_tool = WT,
                Black_tool = BT,
                White_gun = WG,
                Black_gun = BG)

for (k in unique(datcond$Subject)) {
  datcond$numErr[datcond$Subject == k][1] = errors[errors$Subject == k, 5] #Black-gun
  datcond$numErr[datcond$Subject == k][2] = errors[errors$Subject == k, 4] #Black-tool
  datcond$numErr[datcond$Subject == k][3] = errors[errors$Subject == k, 3] #White-gun
  datcond$numErr[datcond$Subject == k][4] = errors[errors$Subject == k, 2] #White-tool
}

ggplot(aes(numErr, MeanAmp), data = datcond) + #2174 is outlier in Black-Gun trials
  facet_wrap(~Condition) +
  geom_point() +
  geom_smooth(method="lm")

ggsave("Figure_CorrelationBetweenNumErrorsAndERN.tiff")

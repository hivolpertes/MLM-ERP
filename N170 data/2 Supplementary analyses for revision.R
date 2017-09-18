require(dplyr)
require(lme4)
require(lmerTest)

# Try to figure out when effect of Race becomes no longer significant

dat = read.delim("./N170 data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt")
# rescale trial
dat$Trial.begin = (dat$Trial-1)/25.5
# shift trial to look at fixed effects at end of task as well
dat$Trial.end = dat$Trial.begin - 10

dat$Race.e = NA
dat$Race.e[dat$Race == "Black"] = -1
dat$Race.e[dat$Race == "White"] = 1

# 1. Try separating into blocks, test with last block as the reference

dat$Block = NA
dat$Block[dat$Trial %in% 225:256] = 0
dat$Block[dat$Trial %in% 193:224] = 1
dat$Block[dat$Trial %in% 161:192] = 2
dat$Block[dat$Trial %in% 129:160] = 3
dat$Block[dat$Trial %in% 97:128] = 4
dat$Block[dat$Trial %in% 65:96] = 5
dat$Block[dat$Trial %in% 33:64] = 6
dat$Block[dat$Trial %in% 1:32] = 7
dat$Block = factor(dat$Block)

# interactions between race and block test difference in effect of race between block and reference block (last block)
# doesn't compare to 0, which is technically what we want
lmer(MeanAmp ~ Race.e*Block + (Race.e|Subject) + (1|Electrode), dat = dat) %>% summary()


# 2. Look at difference by block

# what we want is to test difference compared to 0
# "difference waveform" approach 
# calculate difference between black and white for each subject for each block
dat$Block = NA
dat$Block[dat$Trial %in% 225:256] = 8
dat$Block[dat$Trial %in% 193:224] = 7
dat$Block[dat$Trial %in% 161:192] = 6
dat$Block[dat$Trial %in% 129:160] = 5
dat$Block[dat$Trial %in% 97:128] = 4
dat$Block[dat$Trial %in% 65:96] = 3
dat$Block[dat$Trial %in% 33:64] = 2
dat$Block[dat$Trial %in% 1:32] = 1
dat$Block = factor(dat$Block)

diff = 
  select(dat, Subject, Electrode, MeanAmp, Race, Block) %>% 
  group_by(Subject, Block,Electrode, Race) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

diffDat = NULL
for (k in unique(diff$Subject)) {
  for (b in unique(diff$Block)) {
    for (e in unique(diff$Electrode)) {
      temp = diff[diff$Subject == k & diff$Block == b & diff$Electrode == e,]
      WminusB = temp[2,5] - temp[1,5]
      diffDat = rbind(diffDat, data.frame(Subject = k,
                                          Block = b,
                                          Electrode = e,
                                          WminusB = WminusB))
    }
  }
}

plot(diffDat$Block, diffDat$WminusB)

lmer(WminusB ~ Block-1 + (1|Subject) + (1|Electrode), dat = diffDat) %>% summary()


# 3. Try fitting a number of models where Trial is shifted

sum = lmer(MeanAmp ~ Race.e*Trial.end + (Race.e|Subject) + (1|Electrode), dat = dat) %>% summary
p = sum$coefficients[2,5]

trialrange = 160:170
pTable = NULL
for (i in trialrange) {
  # shift trial so that it's centered on trial i
  dat$TrialTest = dat$Trial - i
  # run model
  sum = lmer(MeanAmp ~ Race.e*TrialTest + (Race.e|Subject) + (1|Electrode), dat = dat) %>% summary
  # get p value
  p = sum$coefficients[2,5]
  # write to table
  pTable = rbind(pTable, data.frame(Trial = i, p = p))
}


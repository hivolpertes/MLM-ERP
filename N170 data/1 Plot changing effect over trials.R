require(ggplot2)
require(magrittr)
require(dplyr)

# looking at N170

# read in cat data
path = "./Analyses with full sample/1 ERP quant data/2 Group time intervals/"
catdat = read.delim(paste(path, "Quantified data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))

filepath = "Analyses with full sample/1 ERP quant data/2 Group time intervals/Changing effect over trial- N170/"

catdat.condense = 
  catdat[catdat$Electrode == "TP7",] %>% 
  group_by(Trial, Electrode, Condition, Race, Fix) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

ggplot(catdat.condense, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("blue", "blue", "red", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250),
                     expand=c(.02,0))
  
ggsave(filename = paste(filepath, "Figure_Cat_N170_overTrials.png", sep=""), width=6.5, height=7.85, units="in")

# Black and white
ggplot(catdat.condense, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("black", "black", "gray65", "gray65")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250),
                     expand=c(.02,0))

ggsave(filename = paste(filepath, "Figure_Cat_N170_overTrials_BW.png", sep=""), width=6.5, height=7.85, units="in")


# read in eval data
evaldat = read.delim(paste(path, "Quantified data/Eval_AllSubs_TBTaverages_noBS_groupN170.txt", sep=""))

evaldat.condense = 
  evaldat[evaldat$Electrode == "TP7",] %>% 
  group_by(Trial, Electrode, Condition, Race, Fix) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

ggplot(evaldat.condense, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("blue", "blue", "red", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                     expand=c(.02,0))


ggsave(filename = paste(filepath, "Figure_Eval_N170_overTrials.png", sep=""), width=10.8, height=7.85, units="in")
 
# Black and white
ggplot(evaldat.condense, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("black", "black", "gray65", "gray65")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                     expand=c(.02,0))


ggsave(filename = paste(filepath, "Figure_Eval_N170_overTrials_BW.png", sep=""), width=10.8, height=7.85, units="in")



### same but with all electrodes

catdat.condense2 = 
  catdat %>% 
  select(-Electrode) %>% 
  group_by(Trial, Condition, Race, Fix) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

ggplot(catdat.condense2, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("blue", "blue", "red", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250),
                     expand=c(.02,0))

ggsave(filename = paste(filepath, "Figure_Cat_N170_overTrials_allElec.png", sep=""), width=6.5, height=7.85, units="in")


# read in eval data

evaldat.condense2 = 
  evaldat %>% 
  select(-Electrode) %>% 
  group_by(Trial, Condition, Race, Fix) %>% 
  summarise_each(funs(mean)) %>% 
  as.data.frame()

ggplot(evaldat.condense2, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition, linetype = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.7,.5,.7,.5)) +
  scale_linetype_manual(values = c("solid", "longdash", "solid", "longdash")) +
  scale_color_manual(values=c("blue", "blue", "red", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7.5, 7.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
                     expand=c(.02,0))


ggsave(filename = paste(filepath, "Figure_Eval_N170_overTrials_allElec.png", sep=""), width=10.8, height=7.85, units="in")


# plot catdat with no fix
ggplot(catdat.condense2, aes(Trial, MeanAmp, alpha = Race, color = Race, shape = Race, linetype = Race)) +
  geom_point() +
  geom_smooth(method = "lm", se=F, lwd = 1.3) +
  labs(x = "Trial", y = "Mean Amplitude (uV)") +
  scale_shape_manual(values=c(1,1)) +
  scale_alpha_manual(values=c(.7,.7)) +
  scale_linetype_manual(values=c("solid", "solid")) +
  scale_color_manual(values=c("blue", "red")) +
  theme_bw() +
  scale_y_continuous(limits=c(-7, 6.5)) +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200, 250),
                     expand=c(.02,0))

ggsave(filename = paste(filepath, "Figure_Cat_N170_overTrials_allElec_noFix.png", sep=""), width=6.5, height=7.85, units="in")




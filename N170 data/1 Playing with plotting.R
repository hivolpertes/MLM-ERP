dat = read.delim("./N170 data/Cat_AllSubs_TBTaverages_noBS_groupN170.txt")

# making dummy codes is redundant but comforting, so LET ME LIVE
dat$Race.d = NA
dat$Race.d[dat$Race == "Black"] = 0
dat$Race.d[dat$Race == "White"] = 1

# find slopes and intercepts with dummy coded model, trial (unscaled) is included
plot1.d = lmer(MeanAmp ~ Race.d*Trial + (Race.d|Subject) + (1|Electrode:Subject), dat = dat)

parms <- fixef(plot1.d) # fixed parameters from model
vcmat <- vcov(plot1.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,2,4)
STmat[1,] <- c(0,0,1,0) # slope for race = 0 (black)
STmat[2,] <- c(0,0,1,1) # slope for race = 1 (white)

Sparest <- STmat %*% parms          # see above for notation

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,2,4)
ITmat[1,] <- c(1,0,0,0) # intercept for race = 0 (black)
ITmat[2,] <- c(1,1,0,0) # intercept for race = 1 (white)

Iparest <- ITmat %*% parms          # see above for notation

forPlotting = cbind(Sparest, Iparest) %>% as.data.frame()
names(forPlotting) = c("Slope", "Intercept")

# label for understanding
forPlotting$Race = c("Black", "White")

# calculate SE for each trial, separately for each race
SETmat <- matrix(NA,512,4)
SETmat[,1] = 1 # first column is intercept
SETmat[1:256, 2] = 0 # second column is race. For first half of rows, race = 0 (Black) 
SETmat[1:256, 3] = 1:256 # third column is trial. Different row for each trial
SETmat[1:256, 4] = 0 # fourth column is interaction. 0 when race = 0, trial number when race = 1
SETmat[257:512, 2] = 1 # second column is race. For second half of rows, race = 1 (White) 
SETmat[257:512, 3] = 1:256 # third column is trial. Different row for each trial
SETmat[257:512, 4] = 1:256 # fourth column is interaction. 0 when race = 0, trial number when race = 1

newvc <- SETmat %*% vcmat %*% t(SETmat)
ses <- sqrt(diag(newvc)) # long string of SEs for each trial, first 256 are for Black faces, second 256 are for White faces

SE.dat = data.frame(Race = rep(c("Black", "White"), each=256), Trial = rep(c(1:256), 2), SE = ses)

# sloppy, and there's probably a better way of doing this
# but take slope and intercept and calculate all the points on the line for each trial so that I can calculate upper and lower 95% CIs 
SE.dat$Point = NULL
SE.dat$Point[SE.dat$Race == "Black"] = forPlotting$Intercept[forPlotting$Race == "Black"] + 1:256*forPlotting$Slope[forPlotting$Race == "Black"]
SE.dat$Point[SE.dat$Race == "White"] = forPlotting$Intercept[forPlotting$Race == "White"] + 1:256*forPlotting$Slope[forPlotting$Race == "White"]

# calculate upper and lower bound (+/- 1 SE)
SE.dat$upper = SE.dat$Point + SE.dat$SE
SE.dat$lower = SE.dat$Point - SE.dat$SE

# BINGO -------------------------------------------------------------------
ggplot(SE.dat, aes(Trial, Point, alpha = Race, color = Race, shape = Race)) +
  geom_abline(data = forPlotting, aes(intercept=Intercept, slope=Slope, color = Race), size=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper, x = Trial),
              linetype = "dashed",
              alpha = .1,
              color = "black") +
  labs(x = "Trial", y = "N170 Mean Amplitude") +
  #  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.1,.1,.1,.1)) +
  scale_color_manual(values=c("blue", "dodgerblue")) +
  theme_bw() +
  scale_y_continuous(limits =c(-4, 1), expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  #  ggtitle("Trial is original trial number") +
  theme(plot.title = element_text(hjust = 0.5),# center title
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16)) 

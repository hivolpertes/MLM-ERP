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


### 3. Looking at the ERN over the course of the experiment
##### - Each error occurs at original trial number

# making dummy codes is redundant but comforting, so LET ME LIVE
errDat$Race.d = NA
errDat$Race.d[errDat$Race == "Black"] = 0
errDat$Race.d[errDat$Race == "White"] = 1

errDat$Object.d = NA
errDat$Object.d[errDat$Object == "gun"] = 0
errDat$Object.d[errDat$Object == "tool"] = 1

# find slopes and intercepts with dummy coded model, trial (unscaled) is included
plot1.d = lmer(MeanAmp ~ Race.d*Object.d*Trial + (Race.d*Object.d|Subject) + (1|Electrode:Subject), dat = errDat)

parms <- fixef(plot1.d) # fixed parameters from model
vcmat <- vcov(plot1.d) # variance/covariance matrix of fixed effects

# First calculate slopes

## each row of this matrix defines entries of parms that we want to combine:
STmat <- matrix(NA,4,8)
STmat[1,] <- c(rep(0,3),1,0,0,0,0) # slope for race = 0, object = 0 (black-gun)
STmat[2,] <- c(rep(0,3),1,0,0,1,0) # slope for race = 0, object = 1 (black-tool)
STmat[3,] <- c(rep(0,3),1,0,1,0,0) # slope for race = 1, object = 0 (white-gun)
STmat[4,] <- c(rep(0,3),1,0,1,1,1) # slope for race = 1, object = 1 (white-tool)

Sparest <- STmat %*% parms          # see above for notation
Snewvc <- STmat %*% vcmat %*% t(STmat)
Sses <- sqrt(diag(Snewvc)) 

slopes = cbind(Sparest, Sses) %>% as.data.frame()
names(slopes) = c("Slope", "Slope_SE")

# Next calculate intercepts

## each row of this matrix defines entries of parms that we want to combine:
ITmat <- matrix(NA,4,8)
ITmat[1,] <- c(1,0,0,0,0,rep(0,3)) # intercept for race = 0, object = 0 (black-gun)
ITmat[2,] <- c(1,0,1,0,0,rep(0,3)) # intercept for race = 0, object = 1 (black-tool)
ITmat[3,] <- c(1,1,0,0,0,rep(0,3)) # intercept for race = 1, object = 0 (white-gun)
ITmat[4,] <- c(1,1,0,0,1,rep(0,3)) # intercept for race = 1, object = 1 (white-tool)

Iparest <- ITmat %*% parms          # see above for notation
Inewvc <- ITmat %*% vcmat %*% t(ITmat)
Ises <- sqrt(diag(Inewvc)) 

intercepts = cbind(Iparest, Ises) %>% as.data.frame()
names(intercepts) = c("Intercept", "Intercept_SE")

forPlotting = cbind(slopes, intercepts)

# label for understanding
forPlotting$Condition = c("Black_gun", 
                          "Black_tool",
                          "White_gun",
                          "White_tool")


# BINGO -------------------------------------------------------------------
ggplot(errDat, aes(Trial, MeanAmp, alpha = Condition, color = Condition, shape = Condition)) +
  geom_point() +
  geom_abline(data = forPlotting, aes(intercept=Intercept, slope=Slope, color = Condition), size=1)+
  labs(x = "Trial", y = "Mean Amplitude") +
  scale_shape_manual(values=c(1,19,1,19)) +
  scale_alpha_manual(values=c(.1,.1,.1,.1)) +
  scale_color_manual(values=c("dodgerblue", "blue", "red", "darkred")) +
  theme_bw() +
  scale_y_continuous(limits =c(-7.5, 10), expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  ggtitle("Trial is original trial number") +
  theme(plot.title = element_text(hjust = 0.5)) # center title

##### Simple slopes
# rescale trial
errDat$Trial.begin = (errDat$Trial-1)/3.83
# shift trial to look at fixed effects at middle and end of task as well
errDat$Trial.middle = errDat$Trial.begin - 5
errDat$Trial.end = errDat$Trial.begin - 10

# find slopes with dummy coded model, trial is scaled
begin.d <- lmer(MeanAmp ~ Race.d*Object.d*Trial.begin + (Race.d*Object.d|Subject) + (1|Electrode:Subject), dat = errDat)

parms <- fixef(begin.d) # fixed parameters from model
vcmat <- vcov(begin.d) # variance/covariance matrix of fixed effects

## each row of this matrix defines entries of parms that we want to combine:
Tmat <- matrix(NA,4,8)
Tmat[1,] <- c(rep(0,3),1,0,0,0,0) # weights for estimates for current = 0, previous = 0 (current compat-previous compat)
Tmat[2,] <- c(rep(0,3),1,0,0,1,0) # weights for estimates for current = 0, previous = 1 (current compat-previous incompat)
Tmat[3,] <- c(rep(0,3),1,0,1,0,0) # weights for estimates for current = 1, previous = 0 (current incompat-previous compat)
Tmat[4,] <- c(rep(0,3),1,0,1,1,1) # weights for estimates for current = 1, previous = 1 (current incompat-previous incompat)

parest <- Tmat %*% parms          # see above for notation
newvc <- Tmat %*% vcmat %*% t(Tmat)
ses <- sqrt(diag(newvc)) 

## final results
fin = cbind(parest, ses) %>% as.data.frame()
names(fin) = c("est", "ses")

# to calculate 95% CI intervals, lower bound = m - 2*SE, upper bound = m + 2*SE

fin$lbnd = fin$est - 2*fin$ses
fin$ubnd = fin$est + 2*fin$ses

fin = format(fin, digits = 3)

# relabel for understanding
fin$Race = c("Black", "Black", "White", "White")
fin$Object = c("gun", "tool", "gun", "tool")
fin$Color = c("light blue", "dark blue", "light red", "dark red")

fin = rename(fin, Estimate = est, SE = ses, ci95_lower = lbnd, ci95_upper = ubnd)

# display
fin

#### Model output
# The intercept, slopes of current and previous trial condition and their interaction are allowed to vary by subject. *Categorical variables are effect coded.*   
#   
#   Trial is scaled to range from 0 to 10.  

# same model, but with effect coding
begin.e = lmer(MeanAmp ~ Race.e*Object.e*Trial.begin + (Race.e*Object.e|Subject) + (1|Electrode:Subject), dat = errDat)

summary(begin.e)$varcor

round(summary(begin.e)$coefficients[c(1:3,5),1:5], digits = 4)
round(summary(begin.e)$coefficients[c(4,6:8),1:5], digits = 3)



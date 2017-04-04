grand = read.delim("./EFbias data/5 waveforms/data/Data for grand averages_allelec.txt")

# plot grand averages
require(ggplot2)
require(colorspace)
require(grid)

condLine <- c("Black_gun_correct" = "dashed",    # CRN is dashed
              "Black_tool_correct" = "dashed", 
              "White_gun_correct" = "dashed", 
              "White_tool_correct" = "dashed",
              "Black_gun_incorrect" = "solid",  # ERN is solid
              "Black_tool_incorrect" = "solid", 
              "White_gun_incorrect" = "solid", 
              "White_tool_incorrect" = "solid")


condColors <- c("Black_gun_correct" = "red", 
                "Black_tool_correct" = "darkred", 
                "White_gun_correct" = "gray44", 
                "White_tool_correct" = "black",
                "Black_gun_incorrect" = "red", 
                "Black_tool_incorrect" = "darkred", 
                "White_gun_incorrect" = "gray44", 
                "White_tool_incorrect" = "black")

ERPline = geom_line(lwd=1.1,
                    #linetype=plotCondition,
                    aes(color = plotCondition, linetype = plotCondition))


ERNbox = annotate("rect",
                 xmin=25, xmax=125, ymin=-Inf, ymax=Inf, 
                 alpha=0,
                 fill="#F0E0FF",
                 color="black", 
                 linetype="dashed") 

none = element_blank() 

# average of all 9 electrodes
ggplot(data=grand, aes(Time, avgElec, group = plotCondition)) + 
  ERPline + 
  ERNbox + 
  theme_bw() + 
  theme(panel.grid.major.x = none, panel.grid.minor.x = none) +
  scale_x_continuous("Time (ms)", 
                     limits=c(-400, 500), 
                     expand=c(0,0),   # expand=c(0,0) removes extra space before & after data
                     breaks=c(-400, -300, -200, -100, 0, 100, 200, 300, 400, 500)) +
  geom_hline(yintercept=0) + # adds x axis
  geom_vline(xintercept=0) +
  scale_y_reverse(limits =c(10, -7.5)) +  # scale_y_reverse flips y axis
  ylab("Amplitude (uV)") +
  scale_color_manual(values=condColors) +
  scale_linetype_manual(values=condLine) +
  ggtitle("Average of fronto-central electrodes") +
  theme(plot.title = element_text(hjust = 0.5)) # center title

ggsave("./EFbias data/5 waveforms/Grand_9elec.tiff", width=12, height=8)


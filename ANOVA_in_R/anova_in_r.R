# ANOVA in R
# Albert Baichen Du
# Sep 29, 2021

# Load the package =====
library(tidyverse)

# Set working directory ==== # You need to change this when you work on your computer
# Basically you can just create a folder on your desktop and throw everything into it
# Then you change the directory below to your desktop, in the format of "/Users/USERNAME/Desktop/FOLDER"
# USERNAME and FOLDER means your computer's account name and the folder name.
setwd("/Users/albertbdu/Desktop")

# Import data ====
df <- read.csv("reward_punishment_sensitivity.csv")

  #Create a folder where you could all plots.
  dir.create("/Users/albertbdu/Desktop/plots")
  setwd("/Users/albertbdu/Desktop/plots")

# Exploratory data analysis ====
  # First, let's set data types
  # id is nominal, since the value itself does not provide any information. It's just an identifier
  df$id <- as.character(df$id)
  
  # diagnostic_group is character as well, 1 means healthy group, 2 and 3 mean disorder groups.
  df$diagnostic_group <-  as.character(df$diagnostic_group)
  
  # Finally, the punishment_ and reward_sensitivity should be numeric.
  # The difference in values conveys information on how sensitive they are to punishment and reward
  df$punishment_sensitivity <- as.numeric(df$punishment_sensitivity)
  df$reward_sensitivity <- as.numeric(df$reward_sensitivity)

# Data Visualization  
  # Punishment Scatterplot
ggplot(data = df) +
  geom_point(mapping = aes(x = diagnostic_group, y = punishment_sensitivity))+
  labs(
    title = "Punishment Sensitivity between Groups",
    subtitle = "Scatterplot",
    caption = "1 = Healthy Control Group
    2 = Gambling Disorder Group
    3 = Alcohol Disorder Group
    Graph by [YOUR NAME]"
  ) + xlab("Groups") + ylab("Punishment Sensitivity")

  # Save the plot
  ggsave("punishment_sensitivity_scatterplot.png", width = 4, height = 5)

  # Reward Scatterplot
ggplot(data = df) +
  geom_point(mapping = aes(x = diagnostic_group, y = reward_sensitivity))+
  labs(
    title = "Reward Sensitivity between Groups",
    subtitle = "Scatterplot",
    caption = "1 = Healthy Control Group
    2 = Gambling Disorder Group
    3 = Alcohol Disorder Group
    Graph by [YOUR NAME]"
  ) + xlab("Groups") + ylab("Reward Sensitivity")

  # Save the plot
  ggsave("reward_sensitivity_scatterplot.png", width = 4, height = 5)

# ANOVA =====
  # After checking the plots, it's quite possible that they have significant difference between groups
  # So, we want to test this by ANOVA
  # Let's see their mean and standard deviation
  df %>%
    group_by(diagnostic_group) %>%
    summarise(
      count_nm = n(),
      mean_pnsh = mean(punishment_sensitivity, na.rm = TRUE),
      mean_rwd = mean(reward_sensitivity, na.rm = TRUE),
      sd_pnsh = sd(punishment_sensitivity, na.rm = TRUE),
      sd_rwd = sd(reward_sensitivity, na.rm = TRUE),
    )
  # Looks good!
  
  # Then, run two anova, save the results, and do post hoc tests
  anova_pnsh <- aov(punishment_sensitivity ~ diagnostic_group, data = df)
  # Save anova
  result_pnsh <- summary(anova_pnsh)
  # And view it
  summary(anova_pnsh)
  # Save the printout
  capture.output(result_pnsh, file = "anova_punishment_sensitivity.txt")
  # Do the posthoc test to see if there is really a difference
  posthoc_pnsh <- TukeyHSD(anova_pnsh)
  # And save the posthoc's printout as well
  capture.output(posthoc_pnsh, file = "posthoc_pnsh.txt")
  
  # Same as above
  anova_rwd <- aov(reward_sensitivity ~ diagnostic_group, data = df)
  result_rwd <- summary(anova_rwd)
  summary(anova_rwd)
  capture.output(result_rwd, file = "anova_reward_sensitivity.txt")
  posthoc_rwd <- TukeyHSD(anova_rwd)
  capture.output(posthoc_rwd, file = "posthoc_rwd.txt")

  # That's it!
  

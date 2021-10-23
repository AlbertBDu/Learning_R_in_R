# Learn paired t.test and how to fix the vialation of independent observation
# Albert Baichen DU
# Purpose: For self-note, reminder, and future reference
# Last Update: Oct 24, 2021
# Download the data from github first

# The data comes from an experiment where each participant is exposed to one
#single sentence under two different conditions. Each person will response to the sentence twice,
#14 participants, 24 sentences and 2 conditions, summed up to 672 observations in total.

# Let's import the file
dataN2 <- read.table("dataN2.txt", header = TRUE)

# The xtab function is pretty much the same as table()
xtabs(~ Sentence + Speaker_id, dataN2)

# To use table(), let's load tidyverse
library(tidyverse)
table(dataN2$Sentence, dataN2$Speaker_id)
# You may see that they are actually the same output in the console

# This is a wrong t-test, why?
t.test(dataN2$N2_dur.2, dataN2$N2_dur.1, paired = TRUE)
# We have only 24 sentences, or 14 subjects, the degree of freedom should be 
#either 14-1 = 13 or 24-1 = 23, how could it be more than 300 hundred?

# If we really want to know what is assumed if the above t-test is true, we have to 
#say that each sentence and each person is split up into two....

# So, if you are doing a t-test of repeated observation of each subject and each stimulus
#, you have to average across one of them, let's do it!

# First, pivot the file longer.
# Because now each row contains two observations for both conditions, it's not a 
#clean data structure. (Recall that clean data structure is: each row contains one
#and only one observation, and each column contains one and only one variable)
N2data_longer <- pivot_longer(dataN2, c("N2_dur.2", "N2_dur.1"), 
                   names_to = "cond", values_to = "value")

# By-subject t.test
# let's recode the values of cond, which looks better than a chunk of numbers and letters
df_by_subject <- N2data_longer %>% 
  mutate(
    cond = recode(cond, "N2_dur.1" = "a", "N2_dur.2" = "b" )
  )

# let's take the average of response time across items
# What does it mean? It means we will compress all subject's response time under 
#condition A, and that of all condition B.
df_by_subject<- df_by_subject %>% 
  group_by(Speaker_id, cond) %>% 
  summarise(mean_by_subject = mean(value))

# Then, we split the dataset according to which condition where we observed the response time
conda_by_subject <- subset(df_by_subject, cond == "a")
condb_by_subject <- subset(df_by_subject, cond == "b")

# Finally, t-test
# You will see that the statistical significance which appeared in the incorrect 
#t-test shown above now disappears
t.test(conda_by_subject$mean_by_subject, condb_by_subject$mean_by_subject, paired = TRUE)

# By-item t.test
# The same logic as above, you know this is you have followed the above example.
df2 <- pivot_longer(data = dataN2, cols = c("N2_dur.2", "N2_dur.1"), 
                    names_to = "cond", values_to = "value")

df2 <- df2 %>% 
  group_by(Sentence, cond) %>% 
  summarise(mean_by_item = mean(value)) %>% 
  mutate(
    cond = recode(cond, "N2_dur.1" = "a", "N2_dur.2" = "b")
  )

conda_by_sentence <- subset(df2, cond == "a")
condb_by_sentence <- subset(df2, cond == "b")

t.test(conda_by_sentence$mean_by_item, condb_by_sentence$mean_by_item, paired = TRUE)






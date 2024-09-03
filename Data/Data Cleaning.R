
# clear environment
rm(list=ls())

library(dplyr)
library(ggplot2)
library(knitr)
library(foreign)
library(nnet)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(cowplot)


RANDS1 <- read.csv("/Users/toriwhite/Downloads/RANDS1.csv")
RANDS2 <- read.csv("/Users/toriwhite/Downloads/RANDS2.csv")

RANDS.COVID1 <- read.csv("/Users/toriwhite/Downloads/RANDS_COVID_1.csv")
RANDS.COVID2 <- read.csv("/Users/toriwhite/Downloads/RANDS_COVID_3.csv")

atp <- read.spss("/Users/toriwhite/Downloads/W93_Jul21/ATP W93.sav", to.data.frame = TRUE)
na.omit(atp)
atp2 <- read.spss("/Users/toriwhite/Downloads/W72_Aug20/ATP W72.sav", to.data.frame = TRUE)
na.omit(atp2)

################################################################################
## PRE-COVID
################################################################################

# Subset original table with needed data
rands1 <- RANDS1 %>%
  select(AGE,DEMO_GENDER, AWEBOFNO_N, AWEBOFNO_F, ANX_1, ACIEFFRT, ACIHOPLS, 
         ACINERV, ACISAD, DEMO_EMPLOYMENT_STATUS, EDUCATION, INCOME)
rands1$YEAR = 2015


# Subset original table with needed data
rands2 <- RANDS2 %>%
  select(AGE,DEMO_GENDER, AWEBOFNO_N, AWEBOFNO_F, ANX_1, ACIEFFRT, ACIHOPLS, 
         ACINERV, ACISAD, DEMO_EMPLOYMENT_STATUS, EDUCATION, INCOME)
rands2$YEAR = 2016

# Combine to make final pre-covid table
rands <- rbind(rands1, rands2)


# Define the age ranges and labels
age_ranges <- c(29, 49, 64, 65, Inf)
age_labels <- c("18-29", "30-49", "50-64", "65+")

# Use cut to create a new column with age ranges
rands$AGE <- cut(rands$AGE, breaks = age_ranges, labels = age_labels, include.lowest = TRUE)

# Convert the columns to numeric
rands$AWEBOFNO_N <- as.numeric(rands$AWEBOFNO_N)
rands$AWEBOFNO_F <- as.numeric(rands$AWEBOFNO_F)

# Keep data where AWEBOFNO_N and AWEBOFNO_F has complete cases
rands <- rands[complete.cases(rands$AWEBOFNO_N, rands$AWEBOFNO_F), ]

# If the participant response was daily, then value was multiplied by 7 values are standardized 
rands$AWEBOFNO_N[rands$AWEBOFNO_F == 2] <- rands$AWEBOFNO_N[rands$AWEBOFNO_F == 2] / 7

# Rename columns
rands <- rands %>%
  rename(GENDER = DEMO_GENDER)

rands <- rands %>%
  rename(EMPLOY = DEMO_EMPLOYMENT_STATUS)

rands <- rands %>%
  rename(EDUC = EDUCATION)

# Recoding gender 
rands$GENDER[rands$GENDER == 1] <- "Male"
rands$GENDER[rands$GENDER == 2] <- "Female"

# Recoding Employment
rands$EMPLOY[rands$EMPLOY == 1] <- "Working full time"
rands$EMPLOY[rands$EMPLOY == 2] <- "Working part-time, but not full-time student"
rands$EMPLOY[rands$EMPLOY == 3] <- "Full-time student"
rands$EMPLOY[rands$EMPLOY == 4] <- "Retired"
rands$EMPLOY[rands$EMPLOY == 5] <- "Homemaker"
rands$EMPLOY[rands$EMPLOY == 6] <- "Not employed"


# Recoding Education level
rands$EDUC[rands$EDUC == 1] <- "HS graduate or less"
rands$EDUC[rands$EDUC == 2] <- "Some college"
rands$EDUC[rands$EDUC == 3] <- "BA or above"

# Recoding ACIEFFRT levels
rands$ACIEFFRT[rands$ACIEFFRT == 1] <- "All of the time"
rands$ACIEFFRT[rands$ACIEFFRT == 2] <- "Most of the time"
rands$ACIEFFRT[rands$ACIEFFRT == 3] <- "Some of the time"
rands$ACIEFFRT[rands$ACIEFFRT == 4] <- "A little of the time"
rands$ACIEFFRT[rands$ACIEFFRT == 5] <- "None of the time"

# Recoding ACIHOPLS levels
rands$ACIHOPLS[rands$ACIHOPLS == 1] <- "All of the time"
rands$ACIHOPLS[rands$ACIHOPLS == 2] <- "Most of the time"
rands$ACIHOPLS[rands$ACIHOPLS == 3] <- "Some of the time"
rands$ACIHOPLS[rands$ACIHOPLS == 4] <- "A little of the time"
rands$ACIHOPLS[rands$ACIHOPLS == 5] <- "None of the time"

# Recoding ACINERV levels
rands$ACINERV[rands$ACINERV == 1] <- "All of the time"
rands$ACINERV[rands$ACINERV == 2] <- "Most of the time"
rands$ACINERV[rands$ACINERV == 3] <- "Some of the time"
rands$ACINERV[rands$ACINERV == 4] <- "A little of the time"
rands$ACINERV[rands$ACINERV == 5] <- "None of the time"

# Recoding ACISAD levels
rands$ACISAD[rands$ACISAD == 1] <- "All of the time"
rands$ACISAD[rands$ACISAD == 2] <- "Most of the time"
rands$ACISAD[rands$ACISAD == 3] <- "Some of the time"
rands$ACISAD[rands$ACISAD == 4] <- "A little of the time"
rands$ACISAD[rands$ACISAD == 5] <- "None of the time"


# PRE COVID INTERNET USAGE LOOKS TO BE EQUAL BETWEEN THE TWO GROUPS
ggplot(rands, aes(x = GENDER, y = AWEBOFNO_N, fill = EMPLOY)) +
  geom_boxplot() +
  labs(title = "Box Plot of Daily Internet Usage by Gender and Employment",
       x = "Gender",
       y = "Hours")


# PRE COVID, INTERNET USAGE LOOKS TO STAY CONSISTENT IN TERMS OF AGE AND EDUCATION
# Define the order of levels
order_levels <- c("HS graduate or less", "Some college", "BA or above")

# Filter the data for the specified years and plot
rands %>%
  filter(YEAR %in% c(2015, 2016)) %>%
  mutate(EDUC = factor(EDUC, levels = order_levels)) %>%  # Reorder the levels
  ggplot(aes(AWEBOFNO_N, AGE, col = EDUC)) +
  geom_point() +
  facet_grid(EDUC ~ YEAR) +  # "row ~ column"
  scale_color_manual(name = "Education Level", values = c("darkblue", "darkgreen", "darkorange")) +
  theme_minimal() +
  labs(y = "Age", x = "Daily Hours on Internet") + 
  theme(panel.background = element_rect(fill = "gray90"))

# Remove rows with "." in any column
rands <- rands[complete.cases(rands) & !apply(rands, 1, function(row) any(row == ".")), ]


# EMOTION LEVLES STAY RELATIVLEY THE SAME WITH EACH EMOTION AND INTERNET USAGE
# Define the order of levels
order_levels <- c("None of the time", "A little of the time", "Some of the time", "Most of the time", "All of the time")

# Plot the data
p1 <- ggplot(rands, aes(x = AWEBOFNO_N, y = factor(ACISAD, levels = order_levels), fill = ACISAD)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(name = "ACISAD", option = "C") +
  labs(title = 'Emotions by Daily Internet Usage', x = "Hours", y = "Sadness") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Plot the data
p2 <- ggplot(rands, aes(x = AWEBOFNO_N, y = factor(ACIEFFRT, levels = order_levels), fill = ACIEFFRT)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(name = "ACIEFFRT", option = "C") +
  labs(x = "Hours", y = "Exhaustion") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Plot the data
p3 <-ggplot(rands, aes(x = AWEBOFNO_N, y = factor(ACIHOPLS, levels = order_levels), fill = ACIHOPLS)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(name = "ACIHOPLS", option = "C") +
  labs(x = 'Hours', y = "Hopelessness") +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Plot the data
p4 <-ggplot(rands, aes(x = AWEBOFNO_N, y = factor(ACINERV, levels = order_levels), fill = ACINERV)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, color = "white") +
  scale_fill_viridis_d(name = "ACINERV", option = "C") +
  labs(x = 'Hours', y = "Nervousness") +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Arrange the plots
plot_grid(p1, p2, p3, p4, ncol = 1)

################################################################################
## COVID
################################################################################

randscovid1 <- RANDS.COVID1 %>%
  select(AGE, GENDER, ANXFREQ, PHQ_B, GAD7_A, EDUC, EMPLOY)
randscovid1$YEAR = 2020

# GENDER
randscovid1$GENDER[randscovid1$GENDER == 1] <- "Male"
randscovid1$GENDER[randscovid1$GENDER == 2] <- "Female"

# How often do you feel worried, nervous or anxious?
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 1] <- "Daily"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 2] <- "Weekly"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 3] <- "Monthly"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 4] <- "A few times a year"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 5] <- "Never"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 77] <- "Don't know"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 98] <- "WS"
randscovid1$ANXFREQ[randscovid1$ANXFREQ == 99] <- "Refused"

# Over the last 2 how often have you felt feeling down, depressed, or hopeless ?
randscovid1$PHQ_B[randscovid1$PHQ_B == 1] <- "Not at all"
randscovid1$PHQ_B[randscovid1$PHQ_B == 2] <- "Several days"
randscovid1$PHQ_B[randscovid1$PHQ_B == 3] <- "More than half the days"
randscovid1$PHQ_B[randscovid1$PHQ_B == 4] <- "Nearly every day"
randscovid1$PHQ_B[randscovid1$PHQ_B == 77] <- "Don't know"
randscovid1$PHQ_B[randscovid1$PHQ_B == 98] <- "WS"
randscovid1$PHQ_B[randscovid1$PHQ_B == 99] <- "Refused"

# Over the last 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless?
randscovid1$GAD7_A[randscovid1$GAD7_A == 1] <- "Not at all"
randscovid1$GAD7_A[randscovid1$GAD7_A == 2] <- "Several days"
randscovid1$GAD7_A[randscovid1$GAD7_A == 3] <- "More than half the days"
randscovid1$GAD7_A[randscovid1$GAD7_A == 4] <- "Nearly every day"
randscovid1$GAD7_A[randscovid1$GAD7_A == 77] <- "Don't know"
randscovid1$GAD7_A[randscovid1$GAD7_A == 98] <- "Web skip"
randscovid1$GAD7_A[randscovid1$GAD7_A == 99] <- "Refused"

# Education level
randscovid1$EDUC[randscovid1$EDUC == 2] <- "HS graduate or less"
randscovid1$EDUC[randscovid1$EDUC == 3] <- "Some college"
randscovid1$EDUC[randscovid1$EDUC == 4] <- "BA or above"


# Employment
randscovid1$EMPLOY[randscovid1$EMPLOY == 1] <- "Working"
randscovid1$EMPLOY[randscovid1$EMPLOY == 2] <- "Self-employed"
randscovid1$EMPLOY[randscovid1$EMPLOY == 3] <- "Temporary layoff"
randscovid1$EMPLOY[randscovid1$EMPLOY == 4] <- "Looking for work"
randscovid1$EMPLOY[randscovid1$EMPLOY == 5] <- "Retired"
randscovid1$EMPLOY[randscovid1$EMPLOY == 6] <- "Disabled"
randscovid1$EMPLOY[randscovid1$EMPLOY == 7] <- "Other"



randscovid2 <- RANDS.COVID2 %>%
  select(AGE, GENDER, ANXFREQ, PHQ_B, GAD7_A, EDUC, 
         EMPLOY)
randscovid2$YEAR = 2021

# How often do you feel worried, nervous or anxious?
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 1] <- "Daily"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 2] <- "Weekly"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 3] <- "Monthly"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 4] <- "A few times a year"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 5] <- "Never"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 77] <- "Don't know"
randscovid2$ANXFREQ[randscovid2$ANXFREQ == 99] <- "Refused"

# Over the last 2 weeks, how often have you been bothered by feeling down, depressed, or hopeless?
randscovid2$PHQ_B[randscovid2$PHQ_B == 1] <- "Not at all"
randscovid2$PHQ_B[randscovid2$PHQ_B == 2] <- "Several days"
randscovid2$PHQ_B[randscovid2$PHQ_B == 3] <- "More than half the days"
randscovid2$PHQ_B[randscovid2$PHQ_B == 4] <- "Nearly every day"
randscovid2$PHQ_B[randscovid2$PHQ_B == 77] <- "Don't know"
randscovid2$PHQ_B[randscovid2$PHQ_B == 99] <- "Refused"

# Over the last 2 weeks, how often have you been bothered by feeling nervous, anxious, or on edge?
randscovid2$GAD7_A[randscovid2$GAD7_A == 1] <- "Not at all"
randscovid2$GAD7_A[randscovid2$GAD7_A == 2] <- "Several days"
randscovid2$GAD7_A[randscovid2$GAD7_A == 3] <- "More than half the days"
randscovid2$GAD7_A[randscovid2$GAD7_A == 4] <- "Nearly every day"
randscovid2$GAD7_A[randscovid2$GAD7_A == 77] <- "Don't know"
randscovid2$GAD7_A[randscovid2$GAD7_A == 98] <- "Web skip"
randscovid2$GAD7_A[randscovid2$GAD7_A == 99] <- "Refused"

# Gender
randscovid2$GENDER[randscovid2$GENDER == 1] <- "Male"
randscovid2$GENDER[randscovid2$GENDER == 2] <- "Female"

# Education level
randscovid2$EDUC[randscovid2$EDUC == 2] <- "HS graduate or less"
randscovid2$EDUC[randscovid2$EDUC == 3] <- "Some college"
randscovid2$EDUC[randscovid2$EDUC == 4] <- "BA or above"


# Employment
randscovid2$EMPLOY[randscovid2$EMPLOY == 1] <- "Working"
randscovid2$EMPLOY[randscovid2$EMPLOY == 2] <- "Self-employed"
randscovid2$EMPLOY[randscovid2$EMPLOY == 3] <- "Temporary layoff"
randscovid2$EMPLOY[randscovid2$EMPLOY == 4] <- "Looking for work"
randscovid2$EMPLOY[randscovid2$EMPLOY == 5] <- "Retired"
randscovid2$EMPLOY[randscovid2$EMPLOY == 6] <- "Disabled"
randscovid2$EMPLOY[randscovid2$EMPLOY == 7] <- "Other"


randscovid <- rbind(randscovid1, randscovid2)

# Use cut to create a new column with age ranges
randscovid$AGE <- cut(randscovid$AGE, breaks = age_ranges, labels = age_labels, include.lowest = TRUE)

p1 <- ggplot(randscovid, aes(x = GENDER)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), fill = c("pink", "lightblue")) +
  labs(title = "COVID RANDS: Count of Participants by Gender",
       x = "Gender",
       y = "Count")

randscovid_filtered <- randscovid %>%
  filter(AGE != "NA")

p2 <- ggplot(randscovid_filtered, aes(x = AGE, fill = as.factor(AGE))) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "COVID RANDS: Count of Participants by Age",
       x = "Age",
       y = "Count",
       fill = "Age Group")


p3 <- ggplot(randscovid, aes(x = EDUC, fill = as.factor(EDUC))) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "COVID RANDS: Count of Participants by Education",
       x = "Education",
       y = "Count",
       fill = "Education Level") 

# Arrange the plots
plot_grid(p1, p2, p3, ncol = 1)

################################################################################
## MERGING ATP DATA WITH COVID DATA
################################################################################

newatp <- atp %>%
  select(F_AGECAT, F_GENDER, F_EDUCCAT, F_INTFREQ)
newatp$YEAR <- 2021

newatp2 <- atp2 %>%
  select(F_AGECAT, F_GENDER, F_EDUCCAT, F_INTFREQ)
newatp2$YEAR <- 2020

finalatp <- rbind(newatp, newatp2)
finalatp <- na.omit(finalatp)


finalatp <- finalatp %>%
  rename(AGE = F_AGECAT)

finalatp <- finalatp %>%
  rename(GENDER = F_GENDER)

finalatp <- finalatp %>%
  rename(EDUC = F_EDUCCAT)



# GENDER
finalatp$GENDER <- as.character(finalatp$GENDER)
finalatp$GENDER[finalatp$GENDER == "A man"] <- "Male"
finalatp$GENDER[finalatp$GENDER == "A woman"] <- "Female"


# EDUC
finalatp$EDUC <- as.character(finalatp$EDUC)
finalatp$EDUC[finalatp$EDUC == "H.S. graduate or less"] <- "HS graduate or less"
finalatp$EDUC[finalatp$EDUC == "College graduate+"] <- "BA or above"


finalatp_filtered <- finalatp %>%
  filter(GENDER != "In some other way" & GENDER != "Refused")

p1 <- ggplot(finalatp_filtered, aes(x = GENDER)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), fill = c("pink", "lightblue")) +
  labs(title = "ATP: Count of Participants by Gender",
       x = "Gender",
       y = "Count")

finalatp_filtered <- finalatp %>%
  filter(AGE != "Refused")

p2 <- ggplot(finalatp_filtered, aes(x = AGE, fill = as.factor(AGE))) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "ATP: Count of Participants by Age",
       x = "Age",
       y = "Count",
       fill = "Age Group")

finalatp_filtered <- finalatp %>%
  filter(EDUC != "Refused")

p3 <- ggplot(finalatp_filtered, aes(x = EDUC, fill = as.factor(EDUC))) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "ATP: Count of Participants by Education",
       x = "Education",
       y = "Count",
       fill = "Education Level") 

# Arrange the plots
plot_grid(p1, p2, p3, ncol = 1)




# Joining based on AGE, GENDER, and EDUC
result <- left_join(finalatp, randscovid, by = c("AGE", "GENDER", "EDUC", "YEAR"))



result <- result %>%
  filter(GENDER != "In some other way" & GENDER != "Refused")

result <- result %>%
  filter(AGE != "Refused")

result <- result %>%
  filter(EDUC != "Refused")

p1 <- ggplot(result, aes(x = GENDER, fill = F_INTFREQ)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "Daily Internet Usage by Demographic",
       x = "Gender",
       y = "Internet Usage Count",
       fill = "Internet Usage")

p2 <- ggplot(result, aes(x = AGE, fill = F_INTFREQ)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(x = "Age",
       y = "Internet Usage Count",
       fill = "Internet Usage")


# Plot the grouped bar plot
p3 <- ggplot(result, aes(x = EDUC, fill = F_INTFREQ)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(x = "Education",
       y = "Internet Usage Count",
       fill = "Internet Usage")


# Arrange the plots
plot_grid(p1, p2, p3, ncol = 1)






result <- result %>%
  filter(GAD7_A != "Web skip" & GAD7_A != "NA" & GAD7_A != "Don't know" & GAD7_A != "Refused")

result <- result %>%
  filter(PHQ_B != "WS" & PHQ_B != "NA" & PHQ_B != "98" & PHQ_B != "Don't know")

result <- result %>%
  filter(ANXFREQ != "WS" & ANXFREQ != "Refused" & ANXFREQ != "98" & ANXFREQ!= "Don't know")

# Define the order for x-axis levels
emotion_order <- c("Not at all", "Several days", "More than half the days", "Nearly every day")

# Create a ggplot for GAD7_A
p1 <- ggplot(result, aes(x = factor(GAD7_A, levels = emotion_order), fill = factor(F_INTFREQ))) +
  geom_bar(position = "dodge") +
  labs(title = "Emotions by Daily Internet Usage",
       x = "Nervous or on edge",
       y = "Count") +
  scale_fill_discrete(name = "Internet Usage")

# Create a ggplot for PHQ_B
p2 <- ggplot(result, aes(x = factor(PHQ_B, levels = emotion_order), fill = factor(F_INTFREQ))) +
  geom_bar(position = "dodge") +
  labs(x = "Depressed or hopeless",
       y = "Count") +
  scale_fill_discrete(name = "Internet Usage")


# Define the order for x-axis levels
anxiety_order <- c("Never","A few times a year","Monthly", "Weekly","Daily" )

# Create a ggplot for ANXFREQ
p3 <- ggplot(result, aes(x = factor(ANXFREQ, levels = anxiety_order), fill = factor(F_INTFREQ))) +
  geom_bar(position = "dodge") +
  labs(x = "Worried or anxious",
       y = "Count") +
  scale_fill_discrete(name = "Internet Usage")


# Arrange the plots
plot_grid(p1, p2, p3, ncol = 1)




# # Save the dataset to an .Rdata RANDS1
# save(RANDS1, file  = "cleaned_dataset_RANDS1.Rdata")
# save(atp, file = "cleaned_dataset_atp.Rdata")
# save(atp2, file = "cleaned_dataset_atp2.Rdata")
# save(RANDS.COVID1, file = "cleaned_dataset_RANDS_COVID_1.Rdata")
# save(RANDS.COVID2, file = "cleaned_dataset_RANDS_COVID_2.Rdata")
# save(RANDS2, file = "cleaned_dataset_RANDS2.Rdata")




result <- result %>%
  filter(GENDER != "In some other way" & GENDER != "Refused")

# Create grouped bar plots with dodged bars for each year
p1 <- ggplot(result, aes(x = GENDER, fill = factor(YEAR), position = "dodge")) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(title = "Daily Internet Usage by Gender and Employment",
       x = "Gender",
       y = "Internet Usage Count",
       fill = "Year") +
  scale_fill_discrete(name = "Year")

p2 <- ggplot(result, aes(x = AGE, fill = factor(YEAR), position = "dodge")) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(x = "Age",
       y = "Internet Usage Count",
       fill = "Year") +
  scale_fill_discrete(name = "Year")

p3 <- ggplot(result, aes(x = EDUC, fill = factor(YEAR), position = "dodge")) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8)) +
  labs(x = "Education",
       y = "Internet Usage Count",
       fill = "Year") +
  scale_fill_discrete(name = "Year")

# Arrange the plots
plot_grid(p1, p2, p3, ncol = 1)


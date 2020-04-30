library(tidyverse)
library(data.table)
library(psych)
library(gridExtra)

setwd("/home/vanagpau/R/thermostat")
VEB <- fread ("VEB.csv")
as_tibble (VEB)

#Create filter for all completed and non-preview surveys: cs ('completed surveys')
cs <- filter(VEB, Status == "IP Address", Progress == "100")

#Replace Likert strings with numbers as characters
cs <- cs %>% lapply(gsub, pattern = "Strongly agree", replacement = 3)
cs <- cs %>% lapply(gsub, pattern = "Agree", replacement = 2)
cs <- cs %>% lapply(gsub, pattern = "Somewhat agree", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "Neither agree nor disagree", replacement = 0)
cs <- cs %>% lapply(gsub, pattern = "Somewhat disagree", replacement = -1)
cs <- cs %>% lapply(gsub, pattern = "Disagree", replacement = -2)
cs <- cs %>% lapply(gsub, pattern = "Strongly disagree", replacement = -3)
cs <- cs %>% lapply(gsub, pattern = "7 = Very likely", replacement = 7)
cs <- cs %>% lapply(gsub, pattern = "4 = Neither likely nor unlikely", replacement = 4)
cs <- cs %>% lapply(gsub, pattern = "1 = Not at all likely", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "5 = Very much", replacement = 5)
cs <- cs %>% lapply(gsub, pattern = "1 = Not at all", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "1 = Very conservative", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "7 = Very liberal", replacement = 7)


#Coerce list back to tibble
cs <- as_tibble(cs)

#Convert Crescent & Warneford to factors
cs$Q3 <- as.factor(cs$Q3)

#Convert char strings to numerics for col 15-111 (survey main body)
cs [,15:111] <- as_tibble(
  sapply(cs[,15:111], as.numeric, USE.NAMES = FALSE))
cs [,115] <- as_tibble(
  sapply(cs[,115], as.numeric, USE.NAMES = FALSE))

#Define function to reverse scored items
reverse5 <- function (x) recode(x,'5' = 1, '4' = 2, `2` = 4, '1'=5) 
reverse7 <- function (x) recode(x, '3'=-3, `2` = -2, `1` = -1, `-1` = 1, `-2` = 2, '-3'=3) 

#Apply reverse scoring to NEP
cs[c(16, 18, 20, 22, 24, 26, 28)] <- lapply (
  cs[c(16, 18, 20, 22, 24, 26, 28)], reverse7)

#Apply reverse scoring to EAI
#Original: cs[c(31,33,35,37,38,41,42,44,47,49,51,53)] <- lapply (cs[c(31,33,35,37,38,41,42,44,47,49,51,53)], reverse7)
cs[c(31,33,35,36,39,41,43,44,46,48,51,53)] <- lapply (
  cs[c(31,33,35,36,39,41,43,44,46,48,51,53)], reverse7)

#Apply reverse scoring to BSCS
cs[c(100,101,102,103,105,107,108,110,111)] <- lapply (
  cs[c(100,101,102,103,105,107,108,110,111)], reverse5)

#Calculate Cronbach's Alpha
NEP <- cs %>% select(Q5_1:Q5_15)
alpha(NEP)
EAI <- cs %>% select(Q6_1:Q6_24)
alpha(EAI)
BSCS <- cs %>% select(Q11_1:Q11_13)
alpha(BSCS)
likelyPEB <- cs %>% select(Q7_1:Q7_22)
alpha(likelyPEB)
thermo_attitude <- cs %>% select(Q8_1:Q8_9)
alpha(thermo_attitude)
moral_ascoop <- cs %>% select(Q9_1:Q9_7)
alpha(moral_ascoop)
moral_found <- cs %>% select(Q10_1:Q10_7)
alpha(moral_found)

#Calculate all mean averages for EAI
means_EAI <- lapply(EAI, mean, na.rm = TRUE)

#Compute sub-scale scores for EAI
cs <- cs %>% rowwise() %>% mutate(
EAI1 = mean(c(Q6_1, Q6_2), na.rm = TRUE),
EAI2 = mean(c(Q6_3, Q6_4), na.rm = TRUE),
EAI3 = mean(c(Q6_5, Q6_6), na.rm = TRUE),
EAI4 = mean(c(Q6_7, Q6_8), na.rm = TRUE),
EAI5 = mean(c(Q6_9, Q6_10), na.rm = TRUE),
EAI6 = mean(c(Q6_11, Q6_12), na.rm = TRUE),
EAI7 = mean(c(Q6_13, Q6_14), na.rm = TRUE),
EAI8 = mean(c(Q6_15, Q6_16), na.rm = TRUE),
EAI9 = mean(c(Q6_17, Q6_18), na.rm = TRUE),
EAI10 = mean(c(Q6_19, Q6_20), na.rm = TRUE),
EAI11 = mean(c(Q6_21, Q6_22), na.rm = TRUE),
EAI12 = mean(c(Q6_23, Q6_24), na.rm = TRUE),
EAI_mean = mean(c(
  Q6_1, Q6_2, Q6_3, Q6_4, Q6_5, Q6_7, Q6_8, Q6_9, Q6_10, Q6_11, Q6_12, Q6_13,
  Q6_14, Q6_15, Q6_16, Q6_17, Q6_18, Q6_19, Q6_20, Q6_21, Q6_22, Q6_23, Q6_24), na.rm = TRUE)) %>% ungroup()

#Create EAI sub-scales data frame
EAI_subscales <- cs %>% select(EAI1:EAI12)

#Create Activist and Pragmatist variables from likely PEB scale
cs <- cs %>% rowwise() %>% mutate(
  PEB_activist = mean(c(Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_8, Q7_9, Q7_10, Q7_11), na.rm = TRUE))
cs <- cs %>% rowwise() %>% mutate(
  PEB_pragmatist = mean(c(
    Q7_12, Q7_13, Q7_14, Q7_15, Q7_16, Q7_17, Q7_18, Q7_19, Q7_20, Q7_21, Q7_22), na.rm = TRUE))
cs

#Alphas
#Activist
cs %>% select(Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_8, Q7_9, Q7_10, Q7_11) %>% alpha()
#Pragmatist
cs %>% select(Q7_12, Q7_13, Q7_14, Q7_15, Q7_16, Q7_17, Q7_18, Q7_19, Q7_20, Q7_21, Q7_22) %>% alpha()
#Descriptives
summary(cs$PEB_activist)
summary(cs$PEB_pragmatist)


#Plot histogram of EAI_mean per subject
cs %>% ggplot(aes(x=EAI_mean)) + geom_histogram(binwidth = 0.1)

#Calculate overall scores for NEP, BSCS, likelyPEB
#creates vector of means..
cs <- cbind(cs, NEP_mean = rowMeans(NEP, na.rm = TRUE))
cs <- cbind(cs, likelyPEB_mean = rowMeans(likelyPEB, na.rm = TRUE))
cs <- cbind(cs, BSCS_mean = rowMeans(BSCS, na.rm = TRUE))
cs <- cbind(cs, thermo_mean = rowMeans(thermo_attitude, na.rm = TRUE))

#Scatter plot of NEP vs EAI
ggplot(cs) + geom_smooth(mapping = aes(x = EAI_mean, y = NEP_mean))

#Smoothed line plots of NEP, EAI and BSCS scales vs likelyPEB
plot1 <- ggplot(cs) + geom_smooth(mapping = aes(x = EAI_mean, y = likelyPEB_mean))
plot2 <- ggplot(cs) + geom_smooth(mapping = aes(x = NEP_mean, y = likelyPEB_mean))
plot3 <- ggplot(cs) + geom_smooth(mapping = aes(x = BSCS_mean, y = likelyPEB_mean))
grid.arrange(plot1, plot2, plot3, ncol = 3)
ggplot(cs) + geom_smooth(mapping = aes(x = thermo_mean, y = EAI_mean))


#Plot of attitude to likelyPEB
ggplot (cs) + geom_smooth(mapping = aes(x = thermo_mean, y = likelyPEB_mean))
ggplot (cs) + geom_smooth(method = "lm", se = FALSE, mapping = aes(
  x = thermo_mean, y = EAI_mean), colour = "red") + geom_point(
    mapping = aes(x = thermo_mean, y = EAI_mean), colour = "red") + geom_smooth(
    method = "lm", se = FALSE, mapping = aes(x = thermo_mean, y = NEP_mean), colour = "blue") + 
  geom_point(mapping = aes(x = thermo_mean, y = NEP_mean), colour = "blue", shape = "triangle") +
  labs(x = "Pro-environmental Attitude to Thermostat", y = "General Environmental Attitude Scale (Mean)") +
  annotate(geom = "point", x = -1, y = 2.8, colour = "blue", shape = "triangle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.8, label = " NEP", hjust = "left", size = 6) +
  annotate(geom = "point", x = -1, y = 2.5, colour = "red", shape = "circle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.5, label = " EAI", hjust = "left", size = 6)

#Normal distribution tests
#Q-Q plots
qqnorm(cs$likelyPEB_mean, ylab = "likelyPEB")
qqline(cs$likelyPEB_mean, ylab = "likelyPEB")
qqnorm(cs$BSCS_mean, ylab = "BSCS_mean")
qqline(cs$BSCS_mean, ylab = "BSCS_mean")
qqnorm(cs$NEP_mean, ylab = "NEP_mean")
qqline(cs$NEP_mean, ylab = "NEP_mean")
qqnorm(cs$EAI_mean, ylab = "EAI_mean")
qqline(cs$EAI_mean, ylab = "EAI_mean")
qqnorm(cs$likelyPEB_mean, ylab = "thermo_attitude")
qqline(cs$likelyPEB_mean, ylab = "thermo_attitude")

#Shapiro-Wilk test on all scales
lapply(cs[132:138], shapiro.test)
#Results  = PEB_pragmatist = non-normal, PEB_Activist and thermo_mean borderline 

hist(cs$PEB_activist, breaks = 7)
hist(cs$PEB_pragmatist, breaks = 7)


library(tidyverse)
library(data.table)
library(psych)
library(gridExtra)
library(GPArotation)

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

#Calculate overall scores for NEP, BSCS, likelyPEB
#creates vector of means..
cs <- cbind(cs, NEP_mean = rowMeans(NEP, na.rm = TRUE))
cs <- cbind(cs, likelyPEB_mean = rowMeans(likelyPEB, na.rm = TRUE))
cs <- cbind(cs, BSCS_mean = rowMeans(BSCS, na.rm = TRUE))
cs <- cbind(cs, thermo_mean = rowMeans(thermo_attitude, na.rm = TRUE))

#Factor Analysis

#FA of Attitudes scale
cs %>% select(Q8_1:Q8_9) %>% cor() %>% fa.parallel(
  n.obs=88, main = "Parallel Analysis scree plot - Attitude scale")

#FA of likely PEB scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q7_1:Q7_22) %>% cortest()
cs %>% select(Q7_1:Q7_22) %>% KMO()
#Parallel analysis suggests 2 factors
cs %>% select(Q7_1:Q7_22) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - likely PEB")
cs %>% select(Q7_1:Q7_22) %>% cor(
  use = "complete.obs") %>% fa(nfactors=2)

#FA of MAC scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q9_1:Q9_7) %>% cortest()
cs %>% select(Q9_1:Q9_7) %>% KMO()
#Parallel analysis suggests 1 factors
cs %>% select(Q9_1:Q9_7) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Morality as Co-operation")

#FA of MFT scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q10_1:Q10_7) %>% cortest()
cs %>% select(Q10_1:Q10_7) %>% KMO()
#Parallel analysis suggests 1 factors
cs %>% select(Q10_1:Q10_7) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Moral Foundations Theory")

#FA of BSCS scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q11_1:Q11_13) %>% cortest()
cs %>% select(Q11_1:Q11_13) %>% KMO()
#Parallel analysis suggests 1 factors
cs %>% select(Q11_1:Q11_13) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Brief Self-Control Scale")

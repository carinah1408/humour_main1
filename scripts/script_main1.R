### load packages ----
library(tidyverse)
library(dplyr)
library(remotes)
library(devtools)
install_github("mdelacre/Routliers")

### load dataset, make political affilication and exp. conditions factors, recode political affiliation----
main1 <- read_csv("data/main1_workdata.csv") %>%
  as_tibble() %>%
  mutate(polaffili = as.factor(polaffili),
         polaffili = recode(polaffili,
                            "1" = "Left",
                            "2" = "Centre",
                            "3" = "Right",
                            "4" = "Not affiliated")) %>%
  mutate(gend = recode(gend,
                       "1" = "Female",
                       "2" = "Male",
                       "3" = "Trans Female/ Trans Woman",
                       "4" = "Trans Male/ Trans Man",
                       "5" = "Genderqueer/ Gender Non Confirming",
                       "6" = "Different Identity",
                       "7" = "Rather not say")) %>%
  mutate(cond = as.factor(cond))

### subsetting dataset ----

main1_sub <- main1 %>%
  select(id, cond, selfcat_1, selfcat_2, gend, age, polaffili, orgaeff_1, orgaeff_2, stereo_1, stereo_2, legit_1, legit_2, legit_3, support, eff_check1, eff_check2, hum_check1, hum_check2)

### mean age of participants ----
mean(main1_sub$age)
sd(main1_sub$age)

### gender distribution----
main1_sub %>%
  group_by(gend) %>%
  summarise(n())

### mean age and no. of participants per affiliation ----
main1_sub %>%
  group_by(polaffili) %>%
  summarise(., mean_age = mean(age), sd_age = sd(age), n = n())

### gender per political affiliation----
main1_sub %>%
  group_by(polaffili, gend) %>%
  summarise(n())

### allocation to conditions----
main1_sub %>%
  group_by(cond) %>%
  summarise(n())

### allocation to conditions (per political affiliation)----
main1_sub %>%
  group_by(cond, polaffili) %>%
  summarise(n())

### manipulation checks----

  
  






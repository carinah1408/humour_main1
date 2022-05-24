### load packages ----
library(tidyverse)
library(dplyr)
library(remotes)
library(devtools)
library(car)
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
## humour

main1_sub %>%
  filter(cond == "2") %>%
  summarise(mean_hum_check1 = mean(hum_check1), sd_hum_check1 = sd(hum_check1),
            mean_hum_check2 = mean(hum_check2), sd_hum_check2 = sd(hum_check2))

t.test(main1_sub$hum_check1, mu = 3.99, alternative = "two.sided") # testing whether personal and other humour perception sign. diff 
t.test(main1_sub$hum_check1, mu = 3.5, alternative = "two.sided") # testing whether personal humour perception sign. diff from scale mid-point (3.5)
t.test(main1_sub$hum_check2, mu = 3.5, alternative = "two.sided") # testing whether other humour perception sign. diff from scale mid-point (3.5)


## comparison of perceptions of free expression and organisation across conditions

main1_sub %>%
  group_by(cond) %>%
  summarise(mean_eff_check1 = mean(eff_check1), sd_eff_check1 = sd(eff_check1), med_eff_check1 = median(eff_check1),
            mean_eff_check2 = mean(eff_check2), sd_eff_check2 = sd(eff_check2), med_eff_check2 = median(eff_check2), 
            n = n())

# anova eff_check 1 (= "free expreesion")
eff_1.aov <- aov(eff_check1 ~ cond, data = main1_sub)
summary(eff_1.aov)

TukeyHSD(eff_1.aov) # pairwise comparisons

leveneTest(eff_check1 ~ cond, data = main1_sub) # checking homogeneity of variance: homogeneity of variance is violated, using Welch test instead:

oneway.test(eff_check1 ~ cond, data = main1_sub)

eff_1.pairwise.t.test <- pairwise.t.test(main1_sub$eff_check1, main1_sub$cond,
                p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
eff_1.pairwise.t.test

plot(eff_1.aov, 2) # checking normality: normality seems violated, therefore, non-parametric test:

kruskal.test(eff_check1 ~ cond, data = main1_sub)

eff_1.pairwise.wilcox.test <- pairwise.wilcox.test(main1_sub$eff_check1, main1_sub$cond,
                     p.adjust.method = "BH", conf.int = TRUE) # pairwise comparison (non-parametric)
eff_1.pairwise.wilcox.test

# anova eff_check 2 (= "successful rally")
eff_2.aov <- aov(eff_check2 ~ cond, data = main1_sub)
summary(eff_2.aov)

TukeyHSD(eff_2.aov) # pairwise comparisons

leveneTest(eff_check2 ~ cond, data = main1_sub) # checking homogeneity of variance: no viiolation

plot(eff_2.aov, 2) # checking normality: normality seems violated, therefore, non-parametric test:

kruskal.test(eff_check2 ~ cond, data = main1_sub)

eff_2.pairwise.wilcox.test <- pairwise.wilcox.test(main1_sub$eff_check2, main1_sub$cond,
                                                   p.adjust.method = "BH", conf.int = TRUE) # pairwise comparison (non-parametric)
eff_2.pairwise.wilcox.test

### reliability, normality, validity & making new variables----

## self-categorization (reliability and variable generation)----

library(performance) # inter-item correlation

interitem_selfcat <- main1_sub[, c("selfcat_1", "selfcat_2")]
item_intercor(interitem_selfcat) # inter-item correlation of 0.91

main1_sub <- main1_sub %>% # create new variable "selfcat"
  mutate(
    selfcat = (selfcat_1 + selfcat_2)/2
  )

## organizational efficacy (reliability and variable generation)----

interitem_orgaeff <- main1_sub[, c("orgaeff_1", "orgaeff_2")]
item_intercor(interitem_orgaeff) # inter-item correlation of 0.92

main1_sub <- main1_sub %>% # create new variable "selfcat"
  mutate(
    orgaeff = (orgaeff_1 + orgaeff_2)/2
  )

## competence stereotype (reliability and variable generation)----

interitem_stereo <- main1_sub[, c("stereo_1", "stereo_2")]
item_intercor(interitem_stereo) # inter-item correlation of 0.85

main1_sub <- main1_sub %>% # create new variable "selfcat"
  mutate(
    stereo = (stereo_1 + stereo_2)/2
  )

## legitimacy (reliablity, validty and variable generation) ----

library(psych) # cronbach's alpha

key <- list(
  legit = c("legit_1", "legit_2", "legit_3")
)

score.items(key, main1_sub) # reliability of 0.87

main1_sub <- main1_sub %>% # create new variable "selfcat"
  mutate(
    legit = (legit_1 + legit_2 + legit_3)/3
  )

library(lavaan) # validity
library(semPlot)
library(lm.beta)

legit.cfa <- 'legit.cfa =~ legit_1 + legit_2 + legit_3'
cfa_legit.sem <- sem(legit.cfa, data = main1_sub)
lavaan::summary(cfa_legit.sem, standardized = TRUE, fit.measures = TRUE) 
# # model is saturated (sign chi-square, RMSEA = 0, CFI = 1)
# the adequacy of saturated models can be tested by experimentally targeting it, i.e., if its predictions match the observed 
# differences (or lack thereof) of the parameter estimates, then the model may be valid 
# (https://stats.stackexchange.com/questions/283/what-is-a-saturated-model#:~:text=If%20a%20model%20is%20saturated,that%20the%20model%20is%20valid.)
# --> can we observe differences in predictions based on condition (i.e., theoretically, individuals in control group should rate the
# the group as legitimate, whereas individuals in the exp conditions should not/ to a lesser degree)

## self-categorization (normality) ----

hist(main1_sub$selfcat) 
describe(main1_sub$selfcat) # # in the range of normality (skew: 0.01; kurtosis: -0.58), but visually slightly negatively skewed

main_left <- main1_sub %>% # subgroup political affiliation = left
  filter(polaffili == "Left")

hist(main_left$selfcat) # even distribution of low, medium and high values
describe(main_left$selfcat) # in the range of normality

main_centre <- main1_sub %>% # subgroup political affiliation = centre
  filter(polaffili == "Centre")

hist(main_centre$selfcat) # positively skewed
describe(main_centre$selfcat) # in the range of normality

main_right <- main1_sub %>% # subgroup political affiliation = right
  filter(polaffili == "Right")

hist(main_right$selfcat) # strongly positively skewed
describe(main_right$selfcat) # in the range of normality

main_na <- main1_sub %>% # subgroup political affiliation = right
  filter(polaffili == "Not affiliated")

hist(main_na$selfcat) # even distribution of low, medium and high values
describe(main_na$selfcat) # in the range of normality

main_control <- main1_sub %>% # subgroup condition = control
  filter(cond == "0")

hist(main_control$selfcat) # positive skew
describe(main_control$selfcat) # in the range of normality

main_exp1 <- main1_sub %>% # subgroup condition = experimental condition 1
  filter(cond == "1")

hist(main_exp1$selfcat) # even distribution
describe(main_exp1$selfcat) # in the range of normality

main_exp2 <- main1_sub %>% # subgroup condition = experimental condition 2
  filter(cond == "2")

hist(main_exp2$selfcat) # positively skewed
describe(main_exp2$selfcat) # in the range of normality

## organizational efficacy (normality)----

hist(main_control$orgaeff) # slightly negatively skewed
describe(main_control$orgaeff) # slight positive kurtosis (1.24)

hist(main_exp1$orgaeff) # positive skew
describe(main_exp1$orgaeff) # in the range of normality

hist(main_exp2$orgaeff) # positive skew (slightly more higher scores than in exp1)
describe(main_exp2$orgaeff) # slight negative kurtosis (-1.08) 

## competence stereotype (normality)----

hist(main_control$stereo) # negative skew due to strong positive peak at around 4
describe(main_control$stereo) # in the range of normality

hist(main_exp1$stereo) # positive skew
describe(main_exp1$stereo) # in the range of normality

hist(main_exp2$stereo) # positive skew but more higher scores than in exp1
describe(main_exp2$stereo) # in the range of normality

## legitimacy (normality)----

hist(main_control$legit) # negative skew 
describe(main_control$legit) # in the range of normality

hist(main_exp1$legit) # slight negative skew!!!!! no max. of 7
describe(main_exp1$legit) # in the range of normality

hist(main_exp2$legit) # rather positively skewed
describe(main_exp2$legit) # in the range of normality

t.test(main_exp1$legit, mu = 3.8, alternative = "two.sided") # sign. difference between exp. conditions in legitimacy

legit_exp1 <- main_exp1$legit # outlier detection since trimmed mean (4.09) higher than mean (4.02)
legit_exp1_mad <- Routliers::outliers_mad(x=legit_exp1)
legit_exp1_mad # two outliers detected

outliers_legit_exp1 <-dplyr::filter(main_exp1, legit < "1.368133") # ID 089 and ID 146 scored extremely low --> mean will increase even stronger

nooutliers_legit_exp1 <- main_exp1 %>%
  filter(legit >= "1.368133") 

t.test(nooutliers_legit_exp1$legit, mu = 3.8, alternative = "two.sided") # repeat t-test without outliers, mean increases to M = 4.07, obs. continues to be sign. diff. from exp2
  
library(stats) # for Wilcoxon rank sum test/ Mann-Whitney test (~ t-test for median)
wilcox.test(main_exp1$legit, mu = 4) # n.s.

### descriptive statistics & univariate outliers inspection----

main1_sub %>%
  select(selfcat, orgaeff, stereo, legit, support) %>%
  psych::describe() %>%
  as_tibble(rownames="rowname")  %>%
  print()

summary <- main1_sub %>%
  dplyr::group_by(cond) %>%
  summarise(
    selfcat_min = min(selfcat),
    selfcat_max = max(selfcat),
    selfcat_mean = mean(selfcat),
    selfcat_sd = sd(selfcat),
    orgaeff_min = min(orgaeff),
    orgaeff_max = max(orgaeff),
    orgaeff_mean = mean(orgaeff),
    orgaeff_sd = sd(orgaeff),
    stereo_min = min(stereo),
    stereo_max = max(stereo),
    stereo_mean = mean(stereo),
    stereo_sd = sd(stereo),
    legit_min = min(legit),
    legit_max = max(legit),
    legit_mean = mean(legit),
    legit_sd = sd(legit),
    support_min = min(support),
    support_max = max(support),
    support_mean = mean(support),
    support_sd = sd(support))
summary

## univariate outliers inspection per variable----

orgaeff_out <- main1_sub$orgaeff
orgaeff_out_mad <- Routliers::outliers_mad(x=orgaeff_out)
orgaeff_out_mad # no outliers detected

stereo_out <- main1_sub$stereo
stereo_out_mad <- Routliers::outliers_mad(x=stereo_out)
stereo_out_mad # no outliers detected

legit_out <- main1_sub$legit
legit_out_mad <- Routliers::outliers_mad(x=legit_out)
legit_out_mad # no outliers detected

support_out <- main1_sub$support
support_out_mad <- Routliers::outliers_mad(x=support_out)
support_out_mad # no outliers detected
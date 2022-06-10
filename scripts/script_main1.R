### load packages ----
library(tidyverse)
library(remotes)
library(devtools)
library(car)
library(dplyr)
install_github("mdelacre/Routliers")

### load dataset, make political affilication and exp. conditions factors, recode political affiliation----
main1 <- read_csv("data/main1_workdata.csv") %>%
  as_tibble() %>%
  mutate(polaffili = as.factor(polaffili),
         polaffili = dplyr::recode(polaffili,
                            "1" = "Left",
                            "2" = "Centre",
                            "3" = "Right",
                            "4" = "Not affiliated")) %>%
  mutate(gend = dplyr::recode(gend,
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

### gender by political affiliation----
main1_sub %>%
  group_by(polaffili, gend) %>%
  summarise(n())

### allocation to conditions----
main1_sub %>%
  group_by(cond) %>%
  summarise(n())

### allocation to conditions (by political affiliation)----
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

### reliability, checking normality, validity & making new variables----

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

## univariate outliers inspection by variable----

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

## univariate outliers inspection by variable by condition----

orgaeff_control_out <- main_control$orgaeff
orgaeff_control_out_mad <- Routliers::outliers_mad(x=orgaeff_control_out)
orgaeff_control_out_mad # no outliers detected

orgaeff_exp1_out <- main_exp1$orgaeff
orgaeff_exp1_out_mad <- Routliers::outliers_mad(x=orgaeff_exp1_out)
orgaeff_exp1_out_mad # no outliers detected

orgaeff_exp2_out <- main_exp2$orgaeff
orgaeff_exp2_out_mad <- Routliers::outliers_mad(x=orgaeff_exp2_out)
orgaeff_exp2_out_mad # no outliers detected

stereo_control_out <- main_control$stereo
stereo_control_out_mad <- Routliers::outliers_mad(x=stereo_control_out)
stereo_control_out_mad # 3 outliers detected (extremely low): ID056, ID138, ID311

stereo_exp1_out <- main_exp1$stereo
stereo_exp1_out_mad <- Routliers::outliers_mad(x=stereo_exp1_out)
stereo_exp1_out_mad # no outliers detected

stereo_exp2_out <- main_exp2$stereo
stereo_exp2_out_mad <- Routliers::outliers_mad(x=stereo_exp2_out)
stereo_exp2_out_mad # no outliers detected

legit_control_out <- main_control$legit
legit_control_out_mad <- Routliers::outliers_mad(x=legit_control_out)
legit_control_out_mad # no outliers detected

legit_exp1_out <- main_exp1$legit
legit_exp1_out_mad <- Routliers::outliers_mad(x=legit_exp1_out)
legit_exp1_out_mad # 2 outliers detected (extremely low): ID089, ID146

legit_exp2_out <- main_exp2$legit
legit_exp2_out_mad <- Routliers::outliers_mad(x=legit_exp2_out)
legit_exp2_out_mad # no outliers detected

support_control_out <- main_control$support
support_control_out_mad <- Routliers::outliers_mad(x=support_control_out)
support_control_out_mad # no outliers detected

support_exp1_out <- main_exp1$support
support_exp1_out_mad <- Routliers::outliers_mad(x=support_exp1_out)
support_exp1_out_mad # no outliers detected

support_exp2_out <- main_exp2$support
support_exp2_out_mad <- Routliers::outliers_mad(x=support_exp2_out)
support_exp2_out_mad # 2 outliers detected (extremely high): ID032, ID223

## comparison experimental conditions by variable

main1_sub_comp_exp <- main1_sub %>%
  filter(cond != "0")

t.test(main1_sub_comp_exp$orgaeff ~ main1_sub_comp_exp$cond) # n.s.
t.test(main1_sub_comp_exp$stereo ~ main1_sub_comp_exp$cond) # n.s.
t.test(main1_sub_comp_exp$legit ~ main1_sub_comp_exp$cond) # n.s.
t.test(main1_sub_comp_exp$support ~ main1_sub_comp_exp$cond) # p = .05

## comparison variables between conditions

orgaeff.aov <- aov(orgaeff ~ cond, data = main1_sub) # sign.
summary(orgaeff.aov)
TukeyHSD(orgaeff.aov) # sign. between exp and control but not between exp

leveneTest(orgaeff ~ cond, data = main1_sub) # sign, using Welch test instead

oneway.test(orgaeff ~ cond, data = main1_sub) # sign.
orgaeff.pairwise.t.test <- pairwise.t.test(main1_sub$orgaeff, main1_sub$cond,
                                         p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
orgaeff.pairwise.t.test # sign. between control and exp


legit.aov <- aov(legit ~ cond, data = main1_sub)
summary(legit.aov)
TukeyHSD(legit.aov) # sign between exp2 and control

leveneTest(legit ~ cond, data = main1_sub) # n.s.



support.aov <- aov(support ~ cond, data = main1_sub)
summary(support.aov)
TukeyHSD(support.aov) # sign between exp2 and control

leveneTest(support ~ cond, data = main1_sub) # n.s.


stereo.aov <- aov(stereo ~ cond, data = main1_sub) 
summary(stereo.aov) # sign.
TukeyHSD(stereo.aov) # sign. between exp and control but not between exp

leveneTest(stereo ~ cond, data = main1_sub) # n.s.

### filter  and compare participants that were relatively close to guessing the purpose of the experiment----

main1_sub <- main1_sub %>% #new "condition" = "purpose" (= purpose guessed) vs "no purpose" (purposed not guessed)
  mutate(purpose = ifelse(id ==  "042"| id == "159"| id == "163"| id == "313" | id == "339"| id == "354"| id =="357"|
                            id == "065"| id == "143"| id == "144"| id == "160"| id =="174"| id == "178"| id == "297"| 
                            id == "305", 1, 0)) # no participants from control group

main_exp1 %>% # t.test by condition
  group_by(purpose) %>%
  summarise(mean = mean(support), sd = sd(support))

main_exp2 %>%
  group_by(purpose) %>%
  summarise(mean = mean(support), sd = sd(support))

t.test(main_exp1$support ~ main_exp1$purpose, var.equal = FALSE) # n.s.
t.test(main_exp2$support ~ main_exp2$purpose, var.equal = FALSE) # n.s.

# subsets by condition need to updated (run again ll. 213 - 226) to include the new variable

main_control <- main1_sub %>% # subgroup condition = control
  filter(cond == "0")

main_exp1 <- main1_sub %>% # subgroup condition = experimental condition 1
  filter(cond == "1")

main_exp2 <- main1_sub %>% # subgroup condition = experimental condition 2
  filter(cond == "2")

### intercorrelations----

main_cor <-main1_sub %>%
  select(selfcat, orgaeff, stereo, legit, support) %>%
  round(., 2)

library(Hmisc)
rcorr(as.matrix(main_cor)) %>%
  print()

library(apaTables)
apa.cor.table(main_cor,filename = "Correlation_main1.doc",table.number = 1,show.conf.interval = F)

### testing homogeneity of variance between conditions----

leveneTest(support ~ cond, data = main1_sub) # n.s. --> homogeneity of variance

### multivariate outliers detection----

selfcategorization <- main1_sub$selfcat 
organisationaleff <- main1_sub$orgaeff
stereotype <- main1_sub$stereo
legitimacy <- main1_sub$legit
support <- main1_sub$support

selfcategorization_control <- main_control$selfcat 
organisationaleff_control <- main_control$orgaeff
stereotype_control <- main_control$stereo
legitimacy_control <- main_control$legit
support_control <- main_control$support

selfcategorization_exp1 <- main_exp1$selfcat 
organisationaleff_exp1 <- main_exp1$orgaeff
stereotype_exp1 <- main_exp1$stereo
legitimacy_exp1 <- main_exp1$legit
support_exp1 <- main_exp1$support

selfcategorization_exp2 <- main_exp2$selfcat 
organisationaleff_exp2 <- main_exp2$orgaeff
stereotype_exp2 <- main_exp2$stereo
legitimacy_exp2 <- main_exp2$legit
support_exp2 <- main_exp2$support

## overall
orgaeff_legit_mcd <- Routliers::outliers_mcd(x = data.frame(organisationaleff,legitimacy)) # overall
orgaeff_legit_mcd # 5 outliers detected
Routliers::plot_outliers_mcd(orgaeff_legit_mcd, x = data.frame(organisationaleff, legitimacy))
# minimally steeper slope without outliers

outliers_orgaeff_legit <- orgaeff_legit_mcd$outliers_pos
outliers_orgaeff_legit # position (= ID): 58 133 138 171 289

stereo_legit_mcd <- Routliers::outliers_mcd(x = data.frame(stereotype,legitimacy))
stereo_legit_mcd # 5 outliers detected
Routliers::plot_outliers_mcd(stereo_legit_mcd, x = data.frame(stereotype, legitimacy))
# minimally steeper slope without outliers

outliers_stereo_legit <- stereo_legit_mcd$outliers_pos
outliers_stereo_legit # ID58, ID73, ID195, ID223, ID289

legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(legitimacy, support))
legit_support_mcd # 17 outliers detected
Routliers::plot_outliers_mcd(legit_support_mcd, x = data.frame(legitimacy, support))
# slightly steeper slope without outliers

outliers_legit_support <- legit_support_mcd$outliers_pos
outliers_legit_support # ID 15  37  56  89  91 114 138 146 153 171 220 231 234 283 289 297 339 


## by condition (control)
control_orgaeff_legit_mcd <- Routliers::outliers_mcd(x = data.frame(organisationaleff_control,legitimacy_control))
control_orgaeff_legit_mcd # 15 outliers detected
Routliers::plot_outliers_mcd(control_orgaeff_legit_mcd, x = data.frame(organisationaleff_control, legitimacy_control))
# somewhat steeper slope without outliers

outliers_control_orgaeff_legit <- control_orgaeff_legit_mcd$outliers_pos
outliers_control_orgaeff_legit # position: 6  14  21  22  25  27  48  50  53  55  62  67  73  89 107 
# => IDs: 014, 038, 056, 058, 063, 074, 133, 138, 147, 152, 171, 195, 213, 263, 311

glm_control_orgaeff_legit <- lm(legit ~ orgaeff, main_control) # cook's distance for influential outliers
plot(glm_control_orgaeff_legit) # IDposition 14 22 27 identified as outliers (but not crossing)

control_stereo_legit_mcd <- Routliers::outliers_mcd(x = data.frame(stereotype_control,legitimacy_control))
control_stereo_legit_mcd # 13 outliers detected
Routliers::plot_outliers_mcd(control_stereo_legit_mcd, x = data.frame(stereotype_control, legitimacy_control))
# slightly steeper slope without outliers

outliers_control_stereo_legit <- control_stereo_legit_mcd$outliers_pos
outliers_control_stereo_legit # position: 6  19  20  21  22  30  42  50  53  55  62 100 107 
# => IDs: 014, 051, 054, 056, 084, 119, 138, 152, 171, 292, 311 

glm_control_stereo_legit <- lm(legit ~ stereo, main_control)
plot(glm_control_stereo_legit) # IDposition 22 28 62 (but not crossing) 

control_legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(legitimacy_control, support_control))
control_legit_support_mcd # 6 outliers detected
Routliers::plot_outliers_mcd(control_legit_support_mcd, x = data.frame(legitimacy_control, support_control))
# somnewhat steeper slope without outliers

outliers_control_legit_support <- control_legit_support_mcd$outliers_pos
outliers_control_legit_support # position: 21 33 50 62 95 96 => IDs: 056, 091, 138, 171, 281, 283

glm_control_legit_support <- lm(support ~ legit, main_control)
plot(glm_control_legit_support) # IDposition 33 95 96 (but not crossing)


## by condition (exp1)
exp1_orgaeff_legit_mcd <- Routliers::outliers_mcd(x = data.frame(organisationaleff_exp1,legitimacy_exp1))
exp1_orgaeff_legit_mcd # 1 outliers detected
Routliers::plot_outliers_mcd(exp1_orgaeff_legit_mcd, x = data.frame(organisationaleff_exp1, legitimacy_exp1))
# minimally steeper slope without outlier

outliers_exp1_orgaeff_legit <- exp1_orgaeff_legit_mcd$outliers_pos
outliers_exp1_orgaeff_legit # position: 37 => ID 110

glm_exp1_orgaeff_legit <- lm(legit ~ orgaeff, main_exp1)
plot(glm_exp1_orgaeff_legit) # 30 37 84 (but not crossing)


exp1_stereo_legit_mcd <- Routliers::outliers_mcd(x = data.frame(stereotype_exp1,legitimacy_exp1))
exp1_stereo_legit_mcd # 2 outliers detected
Routliers::plot_outliers_mcd(exp1_stereo_legit_mcd, x = data.frame(stereotype_exp1, legitimacy_exp1))
# slightly steeper slope without outlier

outliers_exp1_stereo_legit <- exp1_stereo_legit_mcd$outliers_pos
outliers_exp1_stereo_legit # position 30 50 => IDs: 089, 146

glm_exp1_stereo_legit <- lm(legit ~ stereo, main_exp1)
plot(glm_exp1_stereo_legit) # IDposition 30 50 98 (not crossing)

exp1_legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(legitimacy_exp1, support_exp1))
exp1_legit_support_mcd # 5 outliers detected
Routliers::plot_outliers_mcd(exp1_legit_support_mcd, x = data.frame(legitimacy_exp1, support_exp1))
# somewhat steeper slope without outlier

outliers_exp1_legit_support <- exp1_legit_support_mcd$outliers_pos
outliers_exp1_legit_support # position: 8  30  50  79 112 => IDs: 015, 089, 146, 234, 339

glm_exp1_legit_support <- lm(support ~ legit, main_exp1)
plot(glm_exp1_legit_support) # IDposition 8 50 79 (not crossing)


## by condition (exp2)
exp2_orgaeff_legit_mcd <- Routliers::outliers_mcd(x = data.frame(organisationaleff_exp2,legitimacy_exp2))
exp2_orgaeff_legit_mcd # 7 outliers detected
Routliers::plot_outliers_mcd(exp2_orgaeff_legit_mcd, x = data.frame(organisationaleff_exp2, legitimacy_exp2))
# somewhat steeper slope without outliers

outliers_exp2_orgaeff_legit <- exp2_orgaeff_legit_mcd$outliers_pos
outliers_exp2_orgaeff_legit # position: 22  32  39  88  95 111 112 => IDs: 072, 099, 123, 267, 289, 334, 335

glm_exp2_orgaeff_legit <- lm(legit ~ orgaeff, main_exp2)
plot(glm_exp2_orgaeff_legit) # IDposition: 88 95 112 (not crossing)

exp2_stereo_legit_mcd <- Routliers::outliers_mcd(x = data.frame(stereotype_exp2,legitimacy_exp2))
exp2_stereo_legit_mcd # 2 outliers detected
Routliers::plot_outliers_mcd(exp1_stereo_legit_mcd, x = data.frame(stereotype_exp2, legitimacy_exp2))
# almost no difference between slopes

outliers_exp2_stereo_legit <- exp2_stereo_legit_mcd$outliers_pos
outliers_exp2_stereo_legit # position: 69 95 => IDs: 223, 289

glm_exp2_stereo_legit <- lm(legit ~ stereo, main_exp2)
plot(glm_exp2_stereo_legit) # IDposition: 69 95 112 (not crossing)

exp2_legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(legitimacy_exp2, support_exp2))
exp2_legit_support_mcd # 30 outliers detected
Routliers::plot_outliers_mcd(exp2_legit_support_mcd, x = data.frame(legitimacy_exp2, support_exp2))
# strongly flatter slope without outliers!!

outliers_exp2_legit_support <- exp2_legit_support_mcd$outliers_pos
outliers_exp2_legit_support # position: 3  10  16  17  28  34  35  42  46  48  50  56  60  66  69  71  81  82  84  85  89  92  93  97 102 103 110 111 115 116 
# => IDs: 017, 032, 057, 059, 088, 108, 109, 140, 151, 154, 160, 178, 186, 196, 210, 223, 225, 229, 253, 255, 257, 261, 269, 270, 282, 285, 295, 305, 312, 315, 319, 332, 334, 336, 347, 348

glm_exp2_legit_support <- lm(support ~ legit, main_exp2)
plot(glm_exp2_legit_support) # ID 10 68 72 (not crossing)

### all outliers (uni- and multivariate, across all conditions)----
# 014, 015, 017, 032,  038, 051, 054, 056, 057, 058, 059, 063, 072, 074, 084, 088, 089, 091, 099, 108, 109, 
# 110 119, 123, 133, 138, 140, 146, 147, 151, 152, 154, 160, 171,178, 186, 195, 196, 210, 213, 223,225, 229, 
# 234, 253, 255, 257, 261, 263, 267, 269, 270, 281, 282, 283, 285, 289, 292, 295, 305, 311, 312, 315, 319, 332, 
# 334, 335, 336, 339, 347, 348

scan(text="014 015 017 032 038 051 054 056 057 058 059 063 072 074 084 088 089 091 099 108 109 
110 119 123 133 138 140 146 147 151 152 154 160 171 178 186 195 196 210 213 223 225 229 234 253 255 257 261 
263 267 269 270 281 282 283 285 289 292 295 305 311 312 315 319 332 334 335 336 339 347 348", what="")

list_alloutliers <- c("014", "015", "017", "032", "038", "051", "054", "056", "057", "058", "059", "063", "072", "074", "084", "088", "089", "091", "099", "108", "109",
                   "110", "119", "123", "133", "138", "140", "146", "147", "151", "152", "154", "160", "171", "178", "186", "195", "196", "210", "213", "223", "225",
                   "229", "234", "253", "255", "257", "261", "263", "267", "269", "270", "281", "282", "283", "285", "289", "292", "295", "305", "311", "312", "315",
                   "319", "332", "334","335" ,"336", "339", "347", "348")

main1_sub_withoutanyoutliers <- main1_sub %>% # dataset without any of these outliers
  filter(!id %in% list_alloutliers)

### outliers with potentially strongest leverage (overlap uni- and multivariate outliers, between variables, across all conditions)----

# 014, 015, 032, 056, 084, 089, 110, 138, 146,  152, 171, 223, 234, 267, 281, 283, 289, 311, 335

scan(text = "014 015 032 056 084 089 110 138 146 152 171 223 234 267 281 283 289 311 335", what= "")

list_outliers <- c("014", "015", "032", "056", "084", "089", "110", "138", "146", "152", "171", "223", "234", "267", "281", "283", "289", "311", "335")

main1_sub_withoutoutliers <- main1_sub %>% # dataset without most crucial outliers (19 outliers)
  filter(!id %in% list_outliers)

### repeating variable comparisons between all conditions without outliers----

orgaeff_noout.aov <- aov(orgaeff ~ cond, data = main1_sub_withoutanyoutliers) 
summary(orgaeff_noout.aov) # sign.
TukeyHSD(orgaeff_noout.aov) # sign. between exp and control but not between exp

leveneTest(orgaeff ~ cond, data = main1_sub_withoutanyoutliers) # sign, using Welch test instead
oneway.test(orgaeff ~ cond, data = main1_sub_withoutanyoutliers) # sign.
orgaeff_noout.pairwise.t.test <- pairwise.t.test(main1_sub_withoutanyoutliers$orgaeff, main1_sub_withoutanyoutliers$cond,
                                           p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
orgaeff_noout.pairwise.t.test # sign. between exp and control but not between exp


legit_noout.aov <- aov(legit ~ cond, data = main1_sub_withoutanyoutliers)
summary(legit_noout.aov) # sign.
TukeyHSD(legit_noout.aov) # sign between all contrasts

leveneTest(legit ~ cond, data = main1_sub_withoutanyoutliers) # n.s.
main1_sub_withoutanyoutliers %>% group_by(cond) %>% summarise(mean = mean(legit), sd = sd(legit))

support_noout.aov <- aov(support ~ cond, data = main1_sub_withoutanyoutliers)
summary(support_noout.aov) # sign.
TukeyHSD(support_noout.aov) # sign between all contrasts

leveneTest(support ~ cond, data = main1_sub_withoutanyoutliers) # sign. using Welch test instead
oneway.test(support ~ cond, data = main1_sub_withoutanyoutliers) # sign.
support_noout.pairwise.t.test <- pairwise.t.test(main1_sub_withoutanyoutliers$support, main1_sub_withoutanyoutliers$cond,
                                                 p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
support_noout.pairwise.t.test # sign. between all contrasts

stereo_noout.aov <- aov(stereo ~ cond, data = main1_sub_withoutanyoutliers) 
summary(stereo_noout.aov) # sign.
TukeyHSD(stereo_noout.aov) # sign. between exp and control but not between exp

leveneTest(stereo ~ cond, data = main1_sub_withoutanyoutliers) # sign., using Welch test instead
oneway.test(stereo ~ cond, data = main1_sub_withoutanyoutliers) # sign.
stereo_noout.pairwise.t.test <- pairwise.t.test(main1_sub_withoutanyoutliers$stereo, main1_sub_withoutanyoutliers$cond,
                                                 p.adjust.method = "BH", pool.sd = FALSE) # pairwise comparison (with no assumption of equal variances)
stereo_noout.pairwise.t.test # sign. between all contrasts

main1_sub_withoutanyoutliers %>% filter(cond == "1") %>% summarise(mean = mean(stereo), sd = sd(stereo))
main1_sub_withoutanyoutliers %>% filter(cond == "2") %>% summarise(mean = mean(stereo), sd = sd(stereo))

### import process function----

source("scripts/process_function.R")

### main analyses: H1 (mediational analyses)----

## preparing the variables
# reverse factoring variable condition (process does not accept variables that are designated as factors)

main1_sub <- main1_sub %>%
  mutate(cond = as.numeric(cond))

main1_sub_withoutoutliers <- main1_sub_withoutoutliers %>%
  mutate(cond = as.numeric(cond))

main1_sub_withoutanyoutliers <- main1_sub_withoutanyoutliers %>%
  mutate(cond = as.numeric(cond))

# Helmert contrast coding for condition (though option mcx = 3 will be applied which does the contrast coding automatically)

d1<-(main1_sub$cond==0)*(-2/3)+(main1_sub$cond > 0)*(1/3)
d2<-(main1_sub$cond==1)*(-1/2)+(main1_sub$cond==2)*(1/2)
main1_sub <-data.frame(main1_sub,d1,d2)

# deviation from pre-registration: no mean-centering in mediation analysis 

## mediation analyses

med_orgaeff <- process(data = main1_sub, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, seed = 31522)
med_orgaeff <- process(data = main1_sub, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 31522) # increase bootstrap repetitions

# repeat without outliers

med_orgaeff_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutanyoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)

# run the same model with stereo

med_stereo <- process(data = main1_sub, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_stereo <- process(data = main1_sub, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)

# repeat without outliers

med_stereo_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_stereo_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutanyoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)


### main analyses: H2 (conditional effects)

# The plot option in PROCESS for R produces a table of data for visualizing
# an interaction but, like the SAS version, does not write any R
# code to produce the plot, nor will PROCESS produce a plot in the R
# window.



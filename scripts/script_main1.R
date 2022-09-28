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

main1_sub_dsbycond <- main1_sub %>%
  select(cond, selfcat, orgaeff, stereo, legit, support) 

library(vtable)

st(main1_sub_dsbycond, group = 'cond', group.test = TRUE)

orgaeff_cond <- main1_sub%>% select(cond, orgaeff) %>% 
  plot(xlab="Experimental condition", ylab="Organisational Efficacy", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)")
stereo_cond <- main1_sub%>% select(cond, stereo) %>% plot()
legit_cond <- main1_sub%>% select(cond, legit) %>% 
  plot(xlab="Experimental condition", ylab="Legitimacy", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)")
support_cond <- main1_sub%>% select(cond, support) %>% 
  plot(xlab="Experimental condition", ylab="Support intention", sub = "0 = Control; 1 = Disruption (no mocking); 2 = Disruption (mocking)")

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

main_exp1 <- main_exp1 %>% mutate(purpose = ifelse(id ==  "042"| id == "159"| id == "163"| id == "313" | id == "339"| id == "354"| id =="357"|
                            id == "065"| id == "143"| id == "144"| id == "160"| id =="174"| id == "178"| id == "297"| 
                            id == "305", 1, 0)) # no participants from control group

main_exp2 <- main_exp2 %>% mutate(purpose = ifelse(id ==  "042"| id == "159"| id == "163"| id == "313" | id == "339"| id == "354"| id =="357"|
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

# subsets by condition need to updated

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

# correlation with condition

main1_sub_numcond <- main1_sub %>%
  mutate(cond = as.numeric(cond))

corr_cond_orgaeff <- cor.test(x=main1_sub_numcond$cond, y=main1_sub_numcond$orgaeff, method = 'spearman')
corr_cond_orgaeff # r = -.49, p < .001 --> experimental conditions (1,2) are associated with lower organisational efficacy

corr_cond_stereo <- cor.test(x=main1_sub_numcond$cond, y=main1_sub_numcond$stereo, method = 'spearman')
corr_cond_stereo # r = -.39, p < .001

corr_cond_legit <- cor.test(x=main1_sub_numcond$cond, y=main1_sub_numcond$legit, method = 'spearman')
corr_cond_legit # r = -.18, p < .001 

corr_cond_support <- cor.test(x=main1_sub_numcond$cond, y=main1_sub_numcond$support, method = 'spearman')
corr_cond_support # r = -.13, p = .01

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

# deviation from pre-registration: no mean-centering in mediation analysis 

## mediation analyses (option mcx = 3 will be applied which does the contrast coding automatically, modelbt = 1 applies robust measures)

med_orgaeff <- process(data = main1_sub, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, seed = 31522)
med_orgaeff <- process(data = main1_sub, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 31522) # increase bootstrap repetitions
med_orgaeff <- process(data = main1_sub, y = "support", x = "cond", m = c("orgaeff", "legit"), modelbt =1, mcx = 3, total = 1, model = 6, boot = 10000, seed = 31522) 

# repeat without outliers

med_orgaeff_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutanyoutliers, y = "support", x = "cond", m = c("orgaeff", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)

# run the same model with stereo

med_stereo <- process(data = main1_sub, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_stereo <- process(data = main1_sub, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)
med_stereo <- process(data = main1_sub, y = "support", x = "cond", m = c("stereo", "legit"), modelbt = 1, mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)


# repeat without outliers

med_stereo_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, seed = 02622)
med_stereo_nooutliers <- process(data = main1_sub_withoutoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)
med_orgaeff_nooutliers <- process(data = main1_sub_withoutanyoutliers, y = "support", x = "cond", m = c("stereo", "legit"), mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)


### main analyses: H2 (conditional effects)----

## moderated mediation

# M1 = orgaeff
mod_orgaeff <- process (data=main1_sub,y="support",x="cond",m= c("orgaeff", "legit"),w="selfcat",mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=23622)
# deviation to pre-reg: all mediators were mean-centred
mod_orgaeff <- process (data=main1_sub,y="support",x="cond",m= c("orgaeff", "legit"),w="selfcat",modelbt = 1, mcx = 3, center = 1,model=89, boot = 10000, plot=1, jn = 1, seed=23622)

# visualiation

# creating dataset for interaction plot
legit_int <- c(-1.3991, 0.2676, 1.2676, -1.3991, 0.2676, 1.2676, -1.3991, 0.2676, 1.2676)
selfcat_int <- c(-1.3972, -1.3972, -1.3972, 0.1028, 0.1028, 0.1028, 1.6028, 1.6028, 1.6028)
support_int <- c(1.7658, 2.6597, 3.1961, 2.1778, 3.4575, 4.2253, 2.5898, 4.2552, 5.2545)

df <- data.frame(legit_int, selfcat_int, support_int)

df$selfcat_int <- factor(x = df$selfcat_int, labels = c("16th percentile", "50th percentile", "84th percentile"))
df$legit_int <- factor(x = df$legit_int, labels = c("-1SD", "Mean", "+1SD"))

interaction.plot(x.factor = df$legit_int, 
                 trace.factor = df$selfcat_int,
                 response = df$support_int,
                 fun = median,
                 legend = T,
                 ylab = "Support intention",
                 xlab = "Perceived legitimacy",
                 trace.label = "Self-categorisation",
                 col = c("blue", "red", "green"),
                 lyt = 1,
                 lwd = 3
)

# repeat without outliers
mod_orgaeff_nooutliers <- process (data=main1_sub_withoutanyoutliers, y="support",x="cond",m= c("orgaeff", "legit"),w="selfcat",mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=23622)

# M1 = stereo
mod_stereo <- process (data=main1_sub,y="support",x="cond",m= c("stereo", "legit"),w="selfcat",mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=28622)
mod_stereo <- process (data=main1_sub,y="support",x="cond",m= c("stereo", "legit"),w="selfcat",modelbt = 1, mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=28622)

# repeat without outliers
mod_stereo_nooutliers <- process (data=main1_sub_withoutanyoutliers, y="support",x="cond",m= c("stereo", "legit"),w="selfcat",mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=28622)


### alternative outliers ---- 

## deviation from pre-reg: alternative way of establishing and removing the influence of outliers (here; model-specific outliers): 
## 1. replicate models in lm format and establish regression-model-specific outliers
## 2. run with and without outliers to establish whether outliers are influential cases (significance, valence etc)
## 3. run robust regression and compare with OLS regression results in process, if more or less the same (i.e., no substantial changes in
## significance level or  valence), OLS regression in process seems to be robust against outliers (report 
## bootstrap coeffcicients from OLS in process)

# Helmert contrast coding for condition ---- 

d1<-(main1_sub$cond==0)*(-2/3)+(main1_sub$cond > 0)*(1/3)
d2<-(main1_sub$cond==1)*(-1/2)+(main1_sub$cond==2)*(1/2)
main1_sub <-data.frame(main1_sub,d1,d2)

helmert = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), ncol = 2)
helmert

main1_sub <- main1_sub %>%
  mutate(cond = as.factor(cond)) # contrasts requires cond to be a factor 

contrasts(main1_sub$cond) = helmert

# mediation (orgaeff = M1) ----

# regression 1 
med_orgaeff_lm1 <- lm(orgaeff ~ cond, data = main1_sub)
summary(med_orgaeff_lm1)

med_orgaeff_lm1_cooksd <- cooks.distance(med_orgaeff_lm1)

plot(med_orgaeff_lm1_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_orgaeff_lm1_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_orgaeff_lm1_cooksd)+1, y=med_orgaeff_lm1_cooksd, labels=ifelse(med_orgaeff_lm1_cooksd>4*mean(med_orgaeff_lm1_cooksd, na.rm=T),names(med_orgaeff_lm1_cooksd),""), col="red")  # add labels

# => 9 outliers: ID 14, 38, 56, 72, 80, 147, 213, 335, 360 (those in control condition = low orgaeff; those in exp1 and exp2 = high orgaeff)

influential <- as.numeric(names(med_orgaeff_lm1_cooksd)[(med_orgaeff_lm1_cooksd > 4*mean(med_orgaeff_lm1_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 9)  # influential observations.

car::outlierTest(med_orgaeff_lm1) # most extreme outlier = ID 56 (scores are extremely low)

# run again without outliers 

noutliers1 <- c("014", "038", "056", "072", "080", "147", "213", "335", "360")

main1_sub_noutliers1 <- main1_sub %>%
  filter(!id %in% noutliers1)

med_orgaeff_lm1_nout <- lm(orgaeff ~ cond, data = main1_sub_noutliers1)
summary(med_orgaeff_lm1_nout) # no significant or valence changes

# robust regression 1

library(robustbase)
med_orgaeff_lmrob1 <- lmrob(orgaeff ~ cond, data = main1_sub)
summary(med_orgaeff_lmrob1) # no significant or valence changes
confint(med_orgaeff_lmrob1)

# regression 2

med_orgaeff_lm2 <- lm(legit ~ cond + orgaeff, data = main1_sub)
summary(med_orgaeff_lm2)

# inspecting multicolinearity (vif, condition index and correlation)
library(olsrr)
ols_vif_tol(med_orgaeff_lm2)
ols_eigen_cindex(med_orgaeff_lm2)
ols_correlations(med_orgaeff_lm2)

med_orgaeff_lm2_cooksd <- cooks.distance(med_orgaeff_lm2)

plot(med_orgaeff_lm2_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_orgaeff_lm2_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_orgaeff_lm2_cooksd)+1, y=med_orgaeff_lm2_cooksd, labels=ifelse(med_orgaeff_lm2_cooksd>4*mean(med_orgaeff_lm2_cooksd, na.rm=T),names(med_orgaeff_lm2_cooksd),""), col="red")  # add labels

# => 15 outliers: ID 46, 48, 89, 110, 114, 138, 146, 171, 223, 244, 267, 269, 289, 297, 335 (no obvious pattern observable)

influential <- as.numeric(names(med_orgaeff_lm2_cooksd)[(med_orgaeff_lm2_cooksd > 4*mean(med_orgaeff_lm2_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 15)  # influential observations.

car::outlierTest(med_orgaeff_lm2) # most extreme outlier = ID 289 

# run again without outliers

noutliers2 <- c("046", "048", "089", "110", "114", "138", "146", "171", "223", "244", "267", "269", "289", "297", "335")

main1_sub_noutliers2 <- main1_sub %>%
  filter(!id %in% noutliers2)

med_orgaeff_lm2_nout <- lm(legit ~ cond + orgaeff, data = main1_sub_noutliers2)
summary(med_orgaeff_lm2_nout) # no significant or valence changes




# robust regression 2
med_orgaeff_lmrob2 <- lmrob(legit ~ cond + orgaeff, data = main1_sub)
summary(med_orgaeff_lmrob2) # no significant or valence changes
confint(med_orgaeff_lmrob2)

# regression 3
med_orgaeff_lm3 <- lm(support ~ cond + orgaeff + legit, data = main1_sub)
summary(med_orgaeff_lm3)

med_orgaeff_lm3_cooksd <- cooks.distance(med_orgaeff_lm3)

plot(med_orgaeff_lm3_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_orgaeff_lm3_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_orgaeff_lm3_cooksd)+1, y=med_orgaeff_lm3_cooksd, labels=ifelse(med_orgaeff_lm3_cooksd>4*mean(med_orgaeff_lm3_cooksd, na.rm=T),names(med_orgaeff_lm3_cooksd),""), col="red")  # add labels

# => 17 outliers: ID 4, 6, 15, 34, 80, 81, 169, 180, 201, 212, 220, 234, 243, 245, 261, 283, 357

# replicating regression models with robust regression (to eliminate the influence of outliers)

influential <- as.numeric(names(med_orgaeff_lm3_cooksd)[(med_orgaeff_lm3_cooksd > 4*mean(med_orgaeff_lm3_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 17)  # influential observations.

car::outlierTest(med_orgaeff_lm3) # most extreme outlier = ID 283 

# run again without outliers

noutliers3 <- c("004", "006", "015", "034", "080", "081", "169", "180", "201", "212", "220", "234", "243", "245", "261", "283", "357")

main1_sub_noutliers3 <- main1_sub %>%
  filter(!id %in% noutliers3)

med_orgaeff_lm3_nout <- lm(support ~ cond + orgaeff + legit, data = main1_sub_noutliers3)
summary(med_orgaeff_lm3_nout) # no significant or valence changes

# robust regression 3

med_orgaeff_lmrob3 <- lmrob(support ~ cond + orgaeff + legit, data = main1_sub)
summary(med_orgaeff_lmrob3) # no signficant or valence changes
confint(med_orgaeff_lmrob3)


# mediation (stereo = M1) ----

helmert = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), ncol = 2)
helmert

main1_sub <- main1_sub %>%
  mutate(cond = as.factor(cond)) 

contrasts(main1_sub$cond) = helmert

# regression 1 
med_stereo_lm1 <- lm(stereo ~ cond, data = main1_sub)
summary(med_stereo_lm1)

med_stereo_lm1_cooksd <- cooks.distance(med_stereo_lm1)

plot(med_stereo_lm1_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_stereo_lm1_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_stereo_lm1_cooksd)+1, y=med_stereo_lm1_cooksd, labels=ifelse(med_stereo_lm1_cooksd>4*mean(med_stereo_lm1_cooksd, na.rm=T),names(med_stereo_lm1_cooksd),""), col="red")  # add labels

# => 16 outliers: ID 37, 46, 56, 138, 146, 149, 172, 182, 220, 224, 256, 276, 296, 297,  311, 325

influential <- as.numeric(names(med_stereo_lm1_cooksd)[(med_stereo_lm1_cooksd > 4*mean(med_stereo_lm1_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 16)  # influential observations.

car::outlierTest(med_stereo_lm1) # most extreme outlier = ID 56 

# run again without outliers 

noutliers5 <- c("037", "046", "056", "138", "146", "149", "172", "182", "220", "224", "256", "276", "296", "297",  "311", "325")

main1_sub_noutliers5 <- main1_sub %>%
  filter(!id %in% noutliers5)

med_stereo_lm1_nout <- lm(stereo ~ cond, data = main1_sub_noutliers5)
summary(med_stereo_lm1_nout) # no significant or valence changes

# robust regression 1

med_stereo_lmrob1 <- lmrob(stereo ~ cond, data = main1_sub)
summary(med_stereo_lmrob1) # no significant or valence changes
confint(med_stereo_lmrob1)

# regression 2

med_stereo_lm2 <- lm(legit ~ cond + stereo, data = main1_sub)
summary(med_stereo_lm2)

med_stereo_lm2_cooksd <- cooks.distance(med_stereo_lm2)

plot(med_stereo_lm2_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_stereo_lm2_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_stereo_lm2_cooksd)+1, y=med_stereo_lm2_cooksd, labels=ifelse(med_stereo_lm2_cooksd>4*mean(med_stereo_lm2_cooksd, na.rm=T),names(med_stereo_lm2_cooksd),""), col="red")  # add labels

# => 17 outliers: ID 11, 53, 56, 58, 73, 89, 110, 114, 138, 146, 171, 195, 223, 289, 296, 330, 335

influential <- as.numeric(names(med_stereo_lm2_cooksd)[(med_stereo_lm2_cooksd > 4*mean(med_stereo_lm2_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 17)  # influential observations.

car::outlierTest(med_stereo_lm2) # most extreme outlier = ID 58 

# run again without outliers 

noutliers6 <- c("011", "053", "056", "058", "073", "089", "110", "114", "138", "146", "171", "195", "223", "289", "296", "330", "335")

main1_sub_noutliers6 <- main1_sub %>%
  filter(!id %in% noutliers6)

med_stereo_lm2_nout <- lm(legit ~ cond + stereo, data = main1_sub_noutliers6)
summary(med_stereo_lm2_nout) # cond2 now turns significant, the rest remains the same
confint(med_stereo_lm2_nout)

# robust regression 2

med_stereo_lmrob2 <- lmrob(legit ~ cond + stereo, data = main1_sub)
summary(med_stereo_lmrob2) # no significant or valence changes
confint(med_stereo_lmrob2)

# regression 3

med_stereo_lm3 <- lm(support ~ cond + stereo + legit, data = main1_sub)
summary(med_stereo_lm3)

med_stereo_lm3_cooksd <- cooks.distance(med_stereo_lm3)

plot(med_stereo_lm3_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(med_stereo_lm3_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(med_stereo_lm3_cooksd)+1, y=med_stereo_lm3_cooksd, labels=ifelse(med_stereo_lm3_cooksd>4*mean(med_stereo_lm3_cooksd, na.rm=T),names(med_stereo_lm3_cooksd),""), col="red")  # add labels

# => 14 outliers: ID 15, 52, 53, 79, 91, 111, 220, 227, 234, 243, 245, 281, 283, 357

influential <- as.numeric(names(med_stereo_lm3_cooksd)[(med_stereo_lm3_cooksd > 4*mean(med_stereo_lm3_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 14)  # influential observations.

car::outlierTest(med_stereo_lm3) # most extreme outlier = ID 283 

# run again without outliers 

noutliers7 <- c("015", "052", "053", "079", "091", "111", "220", "227", "234", "243", "245", "281", "283", "357")

main1_sub_noutliers7 <- main1_sub %>%
  filter(!id %in% noutliers7)

med_stereo_lm3_nout <- lm(support ~ cond + stereo + legit, data = main1_sub_noutliers7)
summary(med_stereo_lm3_nout) # cond1 and stereo now turn significant too
confint(med_stereo_lm3_nout)

# robust regression 3

med_stereo_lmrob3 <- lmrob(support ~ cond + stereo + legit, data = main1_sub)
summary(med_stereo_lmrob3) # no significant or valence changes
confint(med_stereo_lmrob3)

# run mediation model again without influential cases 

noutliers8 <- c("011", "015", "053", "056", "058", "073", "079","089", "091","110","111", "114", "138", "146", "171", "195","220", "223", "227", "234", "243", "245", "281", "283","289", "296", "330", "335", "357")

main1_sub_noutliers8 <- main1_sub %>%
  filter(!id %in% noutliers8)

main1_sub_noutliers8 <- main1_sub_noutliers8 %>%
  mutate(cond = as.numeric(cond))

med_stereo <- process(data = main1_sub_noutliers8, y = "support", x = "cond", m = c("stereo", "legit"), modelbt = 1, mcx = 3, total = 1, model = 6, boot = 10000, seed = 02622)


# moderation (orgaeff = M1) ----

helmert = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), ncol = 2)
helmert

main1_sub <- main1_sub %>%
  mutate(cond = as.factor(cond)) 

contrasts(main1_sub$cond) = helmert

# manually mean-centre selfcat, orgaeff, legit

main1_sub <- main1_sub %>%
  mutate(cselfcat = scale(selfcat, scale = FALSE),
         corgaeff = scale(orgaeff, scale = FALSE),
         clegit = scale(legit, scale = FALSE))

# regression 3 (regression 1 and 2 = the same as above)

mod_orgaeff_lm3 <- lm(support ~ cond + corgaeff + clegit + cselfcat + cond*cselfcat + corgaeff*cselfcat + clegit*cselfcat,  data = main1_sub)
summary(mod_orgaeff_lm3)

# plotting outliers
mod_orgaeff_lm3_cooksd <- cooks.distance(mod_orgaeff_lm3)

plot(mod_orgaeff_lm3_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(mod_orgaeff_lm3_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(mod_orgaeff_lm3_cooksd)+1, y=mod_orgaeff_lm3_cooksd, labels=ifelse(mod_orgaeff_lm3_cooksd>4*mean(mod_orgaeff_lm3_cooksd, na.rm=T),names(mod_orgaeff_lm3_cooksd),""), col="red")  # add labels

# --> 14 outliers: ID 015, 052, 053, 079, 091, 111, 220, 227, 234, 243, 245, 281, 283, 357

influential <- as.numeric(names(mod_orgaeff_lm3_cooksd)[(mod_orgaeff_lm3_cooksd > 4*mean(mod_orgaeff_lm3_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 14)  # influential observations.

car::outlierTest(mod_orgaeff_lm3) # most extreme outlier = ID 283 

# run again without outliers 

noutliers9 <- c("015", "052", "053", "079", "091", "111", "220", "227", "234", "243", "245", "281", "283", "357")

main1_sub_noutliers9 <- main1_sub %>%
  filter(!id %in% noutliers9)

mod_orgaeff_lm3_nout <- lm(support ~ cond + corgaeff + clegit + cselfcat + cond*cselfcat + corgaeff*cselfcat + clegit*cselfcat,  data = main1_sub_noutliers9)
summary(mod_orgaeff_lm3_nout) # valence changes: cond1 and cond1*cselfcat turn positive, however, not significant

mod_orgaeff_lmrob3 <- lmrob(support ~ cond + corgaeff + clegit + cselfcat + cond*cselfcat + corgaeff*cselfcat + clegit*cselfcat,  data = main1_sub)
summary(mod_orgaeff_lmrob3) # corgaeff turns negative, however, not signficant
confint(mod_orgaeff_lmrob3)


# moderation (stereo = M1) ----

helmert = matrix(c(-2/3, 1/3, 1/3, 0, -.5, .5), ncol = 2)
helmert

main1_sub <- main1_sub %>%
  mutate(cond = as.factor(cond)) # contrasts requires cond to be a factor 

contrasts(main1_sub$cond) = helmert

# manually mean-centre stereo

main1_sub <- main1_sub %>%
  mutate(cstereo = scale(stereo, scale = FALSE))

# regression 3 (regression 1 and 2 are the same as above)

mod_stereo_lm3 <- lm(support ~ cond + cstereo + clegit + cselfcat + cond*cselfcat + cstereo*cselfcat + clegit*cselfcat,  data = main1_sub)
summary(mod_stereo_lm3)

mod_stereo_lm3_cooksd <- cooks.distance(mod_stereo_lm3)

plot(mod_stereo_lm3_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(mod_stereo_lm3_cooksd, na.rm=T), col="red") + # add cutoff line
  text(x=1:length(mod_stereo_lm3_cooksd)+1, y=mod_stereo_lm3_cooksd, labels=ifelse(mod_stereo_lm3_cooksd>4*mean(mod_stereo_lm3_cooksd, na.rm=T),names(mod_stereo_lm3_cooksd),""), col="red")  # add labels

# --> 16 outliers: ID 006, 068, 077, 128, 161, 176, 201, 220, 234, 243, 245, 283, 334, 348, 357, 359

influential <- as.numeric(names(mod_stereo_lm3_cooksd)[(mod_stereo_lm3_cooksd > 4*mean(mod_stereo_lm3_cooksd, na.rm=T))])  # influential row numbers
head(main1_sub[influential, ], n = 16)  # influential observations.

car::outlierTest(mod_stereo_lm3) # most extreme outlier = ID 283 

# run again without outliers 

noutliers10 <- c("006", "068", "077", "128", "161", "176", "201", "220", "234", "243", "245", "283", "334", "348", "357", "359")

main1_sub_noutliers10 <- main1_sub %>%
  filter(!id %in% noutliers10)

mod_stereo_lm3_nout <- lm(support ~ cond + cstereo + clegit + cselfcat + cond*cselfcat + cstereo*cselfcat + clegit*cselfcat,  data = main1_sub_noutliers10)
summary(mod_stereo_lm3_nout) # cond2 marginally sign.; interaction cond1*cselfcat turns negative but n.s.

# robust regression 

mod_stereo_lmrob3 <- lmrob(support ~ cond + cstereo + clegit + cselfcat + cond*cselfcat + cstereo*cselfcat + clegit*cselfcat,  data = main1_sub)
summary(mod_stereo_lmrob3) # corgaeff turns negative, however, not signficant
confint(mod_stereo_lmrob3)

# run overall model again without potential influential cases

main1_sub_noutliers10 <- main1_sub_noutliers10 %>%
  mutate(cond = as.numeric(cond))

mod_stereo <- process (data=main1_sub_noutliers10,y="support",x="cond",m= c("stereo", "legit"),w="selfcat",modelbt = 1, mcx = 3, center = 1,model=89, boot = 10000, plot=1, seed=28622)

# plot the interaction (INCOMPLETE)----

# code derived from https://bookdown.org/ajkurz/recoding_Hayes_2018/conditional-process-analysis-with-a-multicategorical-antecedent.html#examining-the-first-stage-of-the-mediation-process.

library(brms)

m1_model <- bf(orgaeff ~ 1 + d1 + d2)
m2_model <- bf(legit ~ 1 + d1 + d2 + orgaeff)
y_model <- bf(support ~ 1 + d1+ d2 + orgaeff + legit + selfcat + d1:selfcat + d2:selfcat + orgaeff:selfcat + legit:selfcat)

model<-
  brm(data = main1_sub, 
      family = gaussian,
      c(m1_model + m2_model, y_model),
      chains = 4, cores = 4)

nd <-
  tibble(d1 = c(1/3, -2/3, 1/3),
         d2 = c(1/2, 0, -1/2)) %>% 
  expand(nesting(d1, d2),
         selfcat = seq(from = 3.5, to = 6.5, length.out = 30))

f1 <-
  fitted(model, 
         newdata = nd,
         orgaeff = "orgaeff") %>% 
  as_tibble() %>% 
  bind_cols(nd) %>% 
  mutate(cond = ifelse(d2 == 0, "No disruption",
                       ifelse(d2 == -1/2, "Disruption (no mocking)", "Disruption (mocking)"))) %>% 
  mutate(cond = factor(cond, levels = c("No disruption", "Disruption (no mocking)", "Disruption (mocking)")))

main1_sub <-
  main1_sub %>% 
  mutate(cond = ifelse(cond == 0, "No disruption",
                       ifelse(cond == 1, "Disruption (no mocking)", "Disruption (mocking)"))) %>% 
  mutate(cond = factor(cond, levels = c("No disruption", "Disruption (no mocking)", "Disruption (mocking)")))

f1 %>% 
  ggplot(aes(x = selfcat, group = cond)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              linetype = 3, color = "white", fill = "transparent") +
  geom_line(aes(y = Estimate), color = "white") +
  geom_point(data = main1_sub, aes(x = selfcat, y = support),
             color = "red", size = 2/3) +
  coord_cartesian(xlim = 4:6) +
  labs(x = expression(paste("Identifying as a group member (", italic(W), ")")),
       y = expression(paste("Organisational efficacy (", italic(M1), ")", 
                            "Perceived legitimacy (", italic(M2), ")"))) +
  theme_black() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~condition)
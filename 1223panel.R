getwd()
##setwd("C:/Users/ahnsangwon/OneDrive - SNU/MA/affective polarization/ap data")
setwd("/Users/ansang-won/Library/CloudStorage/OneDrive-SNU/MA/affective polarization/ap data")
library(tidyverse)
library(haven)
library(epiDisplay)
library(sjPlot)
library(stringr)
##install.packages("plm")
##install.packages("panelr")
library(plm)
library(stargazer)
library(panelr)
cpc2012 <- read_dta("2012.dta")
cpc2016 <- read_dta("2016.dta")
cpc2017 <- read_dta("2017.dta")
cpc2023 <- read_dta("2023.dta")


cpc2012 <- rename(cpc2012, thermppp_12 = y2012_Q9_5,
                  thermdpk_12 = y2012_Q9_6,
                  ideo_self_12_w2 = y2012_w2_Q2_7,
                  age_12 = y2012_age,
                  region_12 = y2012_region,
                  gender_12 = y2012_gender,
                  income_12 = y2012_Q16,
                  educ_12 = y2012_Q18
                  )

cpc2023 <- rename(cpc2023, thermppp_23 = y2023_QB1_5,
                  thermdpk_23 = y2023_QB1_4,
                  ideo_self_23 = y2023_QA2_11,
                  age_23 = y2023_SQ2,
                  region_23 = y2023_SQ3,
                  gender_23 = y2023_SQ1,
                  income_23 = y2023_DQ1,
                  educ_23 = y2023_DQ3)

cpc2012 <- cpc2012 %>% 
  mutate(educ_12 = case_when(educ_12 == 1 ~ 1,
                             educ_12 == 2 ~ 2,
                             educ_12 == 3 |educ_12 == 4 ~ 3))
cpc2012 <- cpc2012 %>% 
  mutate(region_12 = case_when(region_12 == 1 ~ "seoul",
                               region_12 == 2 | region_12 == 7 | region_12 == 15 ~ "pk",
                               region_12 == 3 | region_12 == 14 ~ "tk",
                               region_12 == 6 | region_12 == 10 | region_12 == 11 ~ "cheongchung",
                               region_12 == 8 | region_12 == 4 ~ "gyeonggiincheon",
                               region_12 == 5 | region_12 == 12 | region_12 == 13 ~ "honam",
                               region_12 == 9 | region_12 == 16 ~"gangwonjeju"))
cpc2023 <- cpc2023 %>% 
  mutate(region_23 = ifelse(region_23 == 17, 11, region_23),
         educ_23 = case_when(educ_23 == 1 ~ 1,
                             educ_23 == 2 ~ 2,
                             educ_23 == 3 | educ_23 == 4 | educ_23 == 5 ~ 3))
cpc2023 <- cpc2023 %>% 
  mutate(region_23 = case_when(region_23 == 1 ~ "seoul",
                               region_23 == 2 | region_23 == 7 | region_23 == 15 ~ "pk",
                               region_23 == 3 | region_23 == 14 ~ "tk",
                               region_23 == 6 | region_23 == 10 | region_23 == 11 ~ "cheongchung",
                               region_23 == 8 | region_23 == 4 ~ "gyeonggiincheon",
                               region_23 == 5 | region_23 == 12 | region_23 == 13 ~ "honam",
                               region_23 == 9 | region_23 == 16 ~"gangwonjeju"))
cpc2012$region_12 <- factor(cpc2012$region_12)
cpc2012$gender_12 <- factor(cpc2012$gender_12)
cpc2023$region_23 <- factor(cpc2023$region_23)
cpc2023$gender_23 <- factor(cpc2023$gender_23)


cpc2012 <- cpc2012 %>% 
  mutate(thermdiff_12 = thermdpk_12 - thermppp_12,
         thermdiffabs_12 = abs(thermdpk_12 - thermppp_12))
cpc2023 <- cpc2023 %>% 
  mutate(thermdiff_23 = thermdpk_23 - thermppp_23,
         thermdiffabs_23 = abs(thermdpk_23 - thermppp_23))
##pid strength 2012 w1 w2 compare. 1 -> 1// 2,3 -> 2

cpc2012 <- cpc2012 %>% 
  mutate(pid_12 = case_when(y2012_Q8 == 1 | y2012_Q8_1_1 == 1 ~ "ppp",
                            y2012_Q8 == 2 | y2012_Q8_1_1 == 2 ~ "dpk",
                            y2012_Q8 == 3 | y2012_Q8_1_1 == 3 ~ "liberalseries",
                            y2012_Q8 == 4 | y2012_Q8_1_1 == 4 ~ "others",
                            y2012_Q8 == 5 & y2012_Q8_1_1 == 5 ~ "independent"))

cpc2012 <- cpc2012 %>% 
  mutate(pidpower_12 = case_when(y2012_Q8_1 == 1 ~ 3,
                                  y2012_Q8_1 == 2 | y2012_Q8_1 == 3 ~ 2,
                                  y2012_Q8 == 5 & y2012_Q8_1_1 != 5 ~ 1,
                                  y2012_Q8 == 5 & y2012_Q8_1_1 == 5 ~ 0))
cpc2012 <- cpc2012 %>%
  mutate(ideoselfpower_12 = case_when(ideo_self_12_w2 == 0 | ideo_self_12_w2 == 10 ~ 5,
                                        ideo_self_12_w2 == 1 | ideo_self_12_w2 == 9 ~ 4,
                                        ideo_self_12_w2 == 2 | ideo_self_12_w2 == 8 ~ 3,
                                        ideo_self_12_w2 == 3 | ideo_self_12_w2 == 7 ~ 2,
                                        ideo_self_12_w2 == 4 | ideo_self_12_w2 == 6 ~ 1,
                                        ideo_self_12_w2 == 5 ~ 0))

cpc2023 <- cpc2023 %>% 
  mutate(pid_23 = case_when(y2023_QA1_1 == 1 & y2023_QA1_2 == 1 ~ "dpk",
                            y2023_QA1_1 == 2 & y2023_QA1_4 == 1 & y2023_QA1_5 == 1 ~ "dpk",
                            y2023_QA1_1 == 3 & y2023_QA1_4 == 1 & y2023_QA1_5 == 1 ~ "dpk",
                            y2023_QA1_1 == 1 & y2023_QA1_2 == 2 ~ "ppp",
                            y2023_QA1_1 == 2 & y2023_QA1_4 == 1 & y2023_QA1_5 == 2 ~ "ppp",
                            y2023_QA1_1 == 3 & y2023_QA1_4 == 1 & y2023_QA1_5 == 2 ~ "ppp",
                            y2023_QA1_1 == 1 & y2023_QA1_2 == 3 ~ "liberalseries",
                            y2023_QA1_1 == 2 & y2023_QA1_4 == 1 & y2023_QA1_5 == 3 ~ "justice",
                            y2023_QA1_1 == 3 & y2023_QA1_4 == 1 & y2023_QA1_5 == 3 ~ "justice",
                            y2023_QA1_1 == 1 & y2023_QA1_2 == 4 ~ "others",
                            y2023_QA1_1 == 2 & y2023_QA1_4 == 1 & y2023_QA1_5 == 4 ~ "others",
                            y2023_QA1_1 == 3 & y2023_QA1_4 == 1 & y2023_QA1_5 == 4 ~ "others",
                            y2023_QA1_1 == 2 & y2023_QA1_4 == 2 ~ "independent",
                            y2023_QA1_1 == 3 & y2023_QA1_4 == 2 ~ "independent"))
cpc2023 <- cpc2023 %>% 
  mutate(pidpower_23 = case_when(y2023_QA1_1 == 1 & y2023_QA1_3 == 1 ~ 3,
                                  y2023_QA1_1 == 1 & y2023_QA1_3 == 2 ~ 2,
                                  y2023_QA1_1 == 2 & y2023_QA1_4 == 1 ~ 1,
                                  y2023_QA1_1 == 3 & y2023_QA1_4 == 1 ~ 1,
                                  y2023_QA1_1 == 2 & y2023_QA1_4 == 2 ~ 0,
                                  y2023_QA1_1 == 3 & y2023_QA1_4 == 2 ~ 0))
cpc2023 <- cpc2023 %>% 
  mutate(ideoselfpower_23 = case_when(ideo_self_23 == 0 | ideo_self_23 == 10 ~ 5,
                                        ideo_self_23 == 1 | ideo_self_23 == 9 ~ 4,
                                        ideo_self_23 == 2 | ideo_self_23 == 8 ~ 3,
                                        ideo_self_23 == 3 | ideo_self_23 == 7 ~ 2,
                                        ideo_self_23 == 4 | ideo_self_23 == 6 ~ 1,
                                        ideo_self_23 == 5 ~ 0))
cpc1223 <- inner_join(cpc2012, cpc2023, by = "ID")
cpc1223 <- cpc1223 %>% 
  filter(pid_23 == "ppp" | pid_23 == "dpk") %>% 
  dplyr::select(pid_12, pid_23)

cpc2012 <- cpc2012 %>% 
  rename(growth12 = y2012_Q24_2, corp_punish12 = y2012_Q24_3, ally12 = y2012_Q24_5, parttime12 = y2012_Q24_6,
         land12 = y2012_Q24_7, asso12 = y2012_Q24_9, japan12 = y2012_Q24_10, northsup12 = y2012_Q24_11, 
         nuclear12 = y2012_Q24_12, union_auth12 = y2012_Q24_13, educ12 = y2012_Q24_14,
         china12 = y2012_Q24_16, regul12 = y2012_Q24_17, elitsm12 = y2012_Q24_18, queer12 = y2012_Q24_15,
         death12 = y2012_Q24_8
  )

##2.growth > welfare
##6. 비정규직 개별기업
##17. ease cooperate regulation

##3. 체벌 허용
##9. 집회시위 자유
##13. 노사분규 공권력

##recoding (+) is conservative, (-) is liberal
cpc2012$land12 <- 5 - cpc2012$land12
cpc2012$asso12 <- 5 - cpc2012$asso12
cpc2012$japan12 <- 5 - cpc2012$japan12
cpc2012$northsup12 <- 5 - cpc2012$northsup12
cpc2012$nuclear12 <- 5 - cpc2012$nuclear12
cpc2012$china12 <- 5 - cpc2012$china12
cpc2012$elitsm12 <- 5 - cpc2012$elitsm12
cpc2012$queer12 <- 5 - cpc2012$queer12
cpc2012$death12 <- 5 - cpc2012$death12

cpc2023 <- cpc2023 %>% 
  rename(growth23 = y2023_QA13_1, corp_punish23 = y2023_QA13_2, ally23 = y2023_QA13_3, parttime23 = y2023_QA13_4,
         land23 = y2023_QA13_5, asso23 = y2023_QA13_6, japan23 = y2023_QA13_7, northsup23 = y2023_QA13_8,
         nuclear23 = y2023_QA13_9, union_auth23 = y2023_QA13_10, china23 = y2023_QA13_11,
         regul23 = y2023_QA13_12, death23 = y2023_QA13_18, queer23 = y2023_QA13_19
  )
##recoding (+) is conservative, (-) is liberal
cpc2023$land23 <- 5 - cpc2023$land23
cpc2023$asso23 <- 5 - cpc2023$asso23
cpc2023$japan23 <- 5 - cpc2023$japan23
cpc2023$northsup23 <- 5 - cpc2023$northsup23

cpc2023$china23 <- 5 - cpc2023$china23

cpc2023$queer23 <- 5 - cpc2023$queer23
cpc2023$death23 <- 5 - cpc2023$death23

##make issue position for issue consistency
cpc2012 <- cpc2012 %>% 
  mutate(growthcon_12 = growth12 - 3,
         corppunishcon_12 = corp_punish12 - 3,
         allycon_12 = ally12 - 3,
         parttimecon_12 = parttime12 - 3,
         landcon_12 = land12 - 3,
         assocon_12 = asso12 - 3,
         japancon_12 = japan12 - 3,
         northsupcon_12 = northsup12 - 3,
         nuclearcon_12 = nuclear12 - 3,
         unionauthcon_12 = union_auth12 - 3,
         educcon_12 = educ12 - 3,
         chinacon_12 = china12 - 3,
         regulcon_12 = regul12 - 3,
         elitsmcon_12 = elitsm12 - 3,
         queercon_12 = queer12 - 3,
         deathcon_12 = death12 -3)

cpc2023 <- cpc2023 %>% 
  mutate(growthcon_23 = growth23 - 3,
         corppunishcon_23 = corp_punish23 - 3,
         allycon_23 = ally23 - 3,
         parttimecon_23 = parttime23 - 3,
         landcon_23 = land23 - 3,
         assocon_23 = asso23 - 3,
         japancon_23 = japan23 - 3,
         northsupcon_23 = northsup23 - 3,
         nuclearcon_23 = nuclear23 - 3,
         unionauthcon_23 = union_auth23 - 3,
         chinacon_23 = china23 - 3,
         regulcon_23 = regul23 - 3,
         queercon_23 = queer23 - 3,
         deathcon_23 = death23 -3)

cpc2012 <- cpc2012 %>% 
  mutate(issuecon_12 = allycon_12 + northsupcon_12 
         + growthcon_12 + parttimecon_12 + regulcon_12 
         + assocon_12 + unionauthcon_12 + queercon_12 + deathcon_12,
         issueconabs_12 = abs(issuecon_12))
cpc2023 <- cpc2023 %>% 
  mutate(issuecon_23 = allycon_23 + northsupcon_23
         + growthcon_23 + parttimecon_23 + regulcon_23
         + assocon_23 + unionauthcon_23 + queercon_23 + deathcon_23,
         issueconabs_23 = abs(issuecon_23))

##make issue position for issue strength
cpc2012 <- cpc2012 %>% 
  mutate(growthpower_12 = ifelse(growth12 == 1 | growth12 == 4, 2, 1),
         corppunishpower_12 = ifelse(corp_punish12 == 1 | corp_punish12 == 4, 2, 1),
         allypower_12 = ifelse(ally12 == 1 | ally12 == 4, 2, 1),
         parttimepower_12 = ifelse(parttime12 == 1 | parttime12 == 4, 2, 1),
         landpower_12 = ifelse(land12 == 1 | parttime12 == 4, 2, 1),
         assopower_12 = ifelse(asso12 == 1 | asso12 == 4, 2, 1),
         japanpower_12 = ifelse(japan12 == 1 | japan12 == 4, 2, 1),
         northsuppower_12 = ifelse(northsup12 == 1 | northsup12 == 4, 2, 1),
         nuclearpower_12 = ifelse(nuclear12 == 1 | nuclear12 == 4, 2, 1),
         unionauthpower_12 = ifelse(union_auth12 == 1 | union_auth12 == 4, 2, 1),
         educpower_12 = ifelse(educ12 == 1 | educ12 == 4, 2, 1),
         chinapower_12 = ifelse(china12 == 1 | china12 == 4, 2, 1),
         regulpower_12 = ifelse(regul12 == 1 | regul12 == 4, 2, 1),
         elitsmpower_12 = ifelse(elitsm12 == 1 | elitsm12 == 4, 2, 1),
         queerpower_12 = ifelse(queer12 == 1 | queer12 == 4, 2, 1),
         deathpower_12 = ifelse(death12 == 1 | death12 == 4, 2, 1)
         )
cpc2023 <- cpc2023 %>% 
  mutate(growthpower_23 = ifelse(growth23 == 1 | growth23 == 4, 2, 1),
         corppunishpower_23 = ifelse(corp_punish23 == 1 | corp_punish23 == 4, 2, 1),
         allypower_23 = ifelse(ally23 == 1 | ally23 == 4, 2, 1),
         parttimepower_23 = ifelse(parttime23 == 1 | parttime23 == 4, 2, 1),
         landpower_23 = ifelse(land23 == 1 | land23 == 4, 2, 1),
         assopower_23 = ifelse(asso23 == 1 | asso23 == 4, 2, 1),
         japanpower_23 = ifelse(japan23 == 1 | japan23 == 4, 2, 1),
         northsuppower_23 = ifelse(northsup23 == 1 | northsup23 == 4, 2, 1),
         nuclearpower_23 = ifelse(nuclear23 == 1 | nuclear23 == 4, 2, 1),
         unionauthpower_23 = ifelse(union_auth23 == 1 | union_auth23 == 4, 2, 1),
         chinapower_23 = ifelse(china23 == 1 | china23 == 4, 2, 1),
         regulpower_23 = ifelse(regul23 == 1 | regul23 == 4, 2, 1),
         queerpower_23 = ifelse(queer23 == 1 | queer23 == 4, 2, 1),
         deathpower_23 = ifelse(death23 == 1 | death23 == 4, 2, 1))
cpc2012 <- cpc2012 %>% 
  mutate(issuepower_12 = (allypower_12 + northsuppower_12 + 
                            growthpower_12 + parttimepower_12 + 
                            regulpower_12 + assopower_12 + 
                            unionauthpower_12 + queerpower_12 + 
                            deathpower_12) / 9)
cpc2023 <- cpc2023 %>% 
  mutate(issuepower_23 = (allypower_23 + northsuppower_23
         + growthpower_23 + parttimepower_23 + regulpower_23
         + assopower_23 + unionauthpower_23 + queerpower_23 + deathpower_23) / 9)

cpc2012 <- cpc2012 %>% 
  dplyr::select(ID, thermppp_12, thermdpk_12, thermdiff_12, thermdiffabs_12,
                pid_12, pidpower_12, ideoselfpower_12, issuecon_12, issueconabs_12, issuepower_12,
                age_12, gender_12, region_12, income_12, educ_12)
cpc2023 <- cpc2023 %>% 
  dplyr::select(ID, thermppp_23, thermdpk_23, thermdiff_23, thermdiffabs_23,
                pid_23, pidpower_23, ideoselfpower_23, issuecon_23, issueconabs_23, issuepower_23,
                age_23, gender_23, region_23, income_23, educ_23)


cpc1223 <- inner_join(cpc2012, cpc2023, by = "ID")

cpc1223 <- cpc1223 %>% 
  filter(pid_23 == "ppp" | pid_23 == "dpk")
cpc1223 %>% 
  filter(pid_23 == "ppp") %>% 
  summarize(ppp12 = mean(thermppp_12),
            dpk12 = mean(thermdpk_12),
            d12 = mean(thermdiff_12),
            ppp23 = mean(thermppp_23),
            dpk23 = mean(thermdpk_23),
            d23 = mean(thermdiff_23))
cpc1223 %>% 
  filter(pid_23 == "dpk") %>% 
  summarize(ppp12 = mean(thermppp_12),
            dpk12 = mean(thermdpk_12),
            d12 = mean(thermdiff_12),
            ppp23 = mean(thermppp_23),
            dpk23 = mean(thermdpk_23),
            d23 = mean(thermdiff_23))

cpc1223 <- cpc1223 %>% 
  mutate(thermdiff_12 = ifelse(pid_23 == "dpk", thermdpk_12 - thermppp_12, thermppp_12 - thermdpk_12),
         thermdiff_23 = ifelse(pid_23 == "dpk", thermdpk_23 - thermppp_23, thermppp_23 - thermdpk_23))

cpc1223 %>% 
  filter(pid_23 == "ppp") %>% 
  summarize(ppp12 = mean(thermppp_12),
            dpk12 = mean(thermdpk_12),
            d12 = mean(thermdiff_12),
            ppp23 = mean(thermppp_23),
            dpk23 = mean(thermdpk_23),
            d23 = mean(thermdiff_23))
cpc1223 %>% 
  filter(pid_23 == "dpk") %>% 
  summarize(ppp12 = mean(thermppp_12),
            dpk12 = mean(thermdpk_12),
            d12 = mean(thermdiff_12),
            ppp23 = mean(thermppp_23),
            dpk23 = mean(thermdpk_23),
            d23 = mean(thermdiff_23))
tab_xtab(cpc1223$pid_23, cpc1223$pid_12, show.col.prc = T)

long1223 <- long_panel(cpc1223, prefix = "_", periods = c(12, 23), label_location = "end")

long1223 <- long1223 %>% 
  mutate(year_dummy = ifelse(wave == 12, 0, 1))
aa <- pdata.frame(long1223, index = c("ID", "wave"))
aa <- aa %>%
  mutate(cindex=ID)

aa$pid <- factor(aa$pid)
aa$pid <- relevel(aa$pid, ref = "ppp")
aa$region <- relevel(aa$region, ref = "seoul")
form <- thermdiff ~ year_dummy + pidpower + ideoselfpower + issuepower + issueconabs + pid + age + gender + region + income + educ
#fe <- plm(form, data = aa, effect = "twoways")
fe <- plm(form, data = aa, model = "within")
re <- plm(form, data = aa, model = "random")
form2 <- thermdiff ~ 0+ year_dummy + pidpower + ideoselfpower + issuepower + issueconabs + pid + gender + region + income + educ + cindex
po <- plm(form2, data = aa, model = "pooling")

phtest(fe, re)
phtest(form, data = aa)

diff <- function(x) {x - dplyr::lag(x)}
aa_diff1 <- aa %>% 
  group_by(ID) %>% 
  mutate(dtherm = diff(thermdiff),
         dyeardummy = diff(year_dummy),
         dpidpower = diff(pidpower),
         dideoselfpower = diff(ideoselfpower),
         dissueconabs = diff(issueconabs),
         dissuepower = diff(issuepower),
         dpid = ifelse(pid == dplyr::lag(pid), 0, 1),
         dincome = diff(income)) %>%
  mutate(dpidpower = dpidpower + dpid) %>% #corporate pid change into pidpower
  ungroup()

aa_diff2 <- aa %>% 
  group_by(ID) %>% 
  mutate(dtherm = diff(thermdiff),
         dyeardummy = diff(year_dummy),
         dpidpower = diff(pidpower),
         dideoselfpower = diff(ideoselfpower),
         dissueconabs = diff(issueconabs),
         dissuepower = diff(issuepower),
         dpid = ifelse(pid == dplyr::lag(pid), 0, 1),
         dincome = diff(income)) %>%
  #mutate(dpidpower = dpidpower + dpid) %>% #corporating pid change into pidpower
  ungroup()

aa_diff3 <- aa %>% 
  group_by(ID) %>% 
  mutate(dtherm = diff(thermdiff),
         dpidpower = diff(pidpower),
         dideoselfpower = diff(ideoselfpower),
         dissueconabs = diff(issueconabs),
         dissuepower = diff(issuepower),
         pidgain = ifelse(dplyr::lag(pid) == "independent" & pid %in% c("dpk", "ppp"), 1, 0),
         pidconversion = ifelse(dplyr::lag(pid) != "independent" & dplyr::lag(pid) != pid & pid %in% c("dpk", "ppp"), 1, 0),
         dpid = ifelse(dplyr::lag(pid) == pid, 0, 1),
         dincome = diff(income)
         ) %>%
  ungroup()

fd1 <- lm(dtherm ~ dpidpower + dideoselfpower + dideoselfpower + dissuepower + dissueconabs, data = aa_diff1)
fd2 <- lm(dtherm ~ dpidpower + dideoselfpower + dideoselfpower + dissuepower + dissueconabs + dpid, data = aa_diff2)
fd3 <- lm(dtherm ~ dpidpower + dideoselfpower + dideoselfpower + dissuepower + dissueconabs + pidgain + pidconversion + dpid, data = aa_diff3)
summary(fd1)
summary(fd2)
summary(fd3)

stargazer(fe, re, po, type = "html",
          column.labels = c("FE", "RE", "Pooling"),
          omit = "cindex",
          star.cutoffs = c(.05, .01, .001),
          star.char = c("*", "**", "***"),
          keep.stat = c("N", "adj.rsq"),
          out = "fetest.doc")
stargazer(fd1, fd2, fd3, type = "html",
          column.labels = c("FD", "FD", "FD"),
          omit = "cindex",
          star.cutoffs = c(.05, .01, .001),
          star.char = c("*", "**", "***"),
          keep.stat = c("N", "adj.rsq"),
          out = "FDtest.doc")

tab_xtab(cpc1223$pid_23, cpc1223$pid_12, show.col.prc = T)
cpc1223_gender <- cpc1223 %>% 
  filter(pid_12 == "dpk" & pid_23 == "dpk")
tab1(cpc1223_gender$gender_23)
tab1(cpc1223$gender_23)

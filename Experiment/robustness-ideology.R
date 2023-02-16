library(tidyverse)
library(viridis)
library(haven)
library(labelled)
library(labeling)
library(modelsummary)
library(gridExtra)

#load df
df <- read_sav("Data/FLSU0010_OUTPUT.sav") 
var_label(df) <- NULL
#write_csv(df, "Data/FLSU0010_OUTPUT.csv")

#tidy and shape
gender <- df %>%
  select(caseid, applicant2_treat, APP_1_amt_1, APP_2_amt_1, STATE_amt_1,
         ideo5) %>%
  drop_na() %>%
  mutate(applicant_2_char = as.character(applicant2_treat),
         applicant_2_char = recode(applicant2_treat,
                                   "1" = "Misty (Exc)",
                                   "2" = "Misty (Poor)",
                                   "3" = "James (Exc)",
                                   "4" = "James (Poor)",
                                   "5" = "Sammie (Exc)",
                                   "6" = "Sammie (Poor)",
                                   "7" = "No Name (Exc)")) %>%
  mutate(applicant_2_name = as.character(applicant2_treat),
         applicant_2_name = recode(applicant2_treat,
                                   "1" = "Misty",
                                   "2" = "Misty",
                                   "3" = "James",
                                   "4" = "James",
                                   "5" = "Sammie",
                                   "6" = "Sammie",
                                   "7" = "No Name")) %>%
  mutate(applicant_2_rate = as.character(applicant2_treat),
         applicant_2_rate = recode(applicant2_treat,
                                   "1" = "Excellent",
                                   "2" = "Poor",
                                   "3" = "Excellent",
                                   "4" = "Poor",
                                   "5" = "Excellent",
                                   "6" = "Poor",
                                   "7" = "Excellent")) %>%
  mutate(applicant_2_comp = as.character(applicant_2_name),
         applicant_2_comp = recode(applicant_2_name,
                                   "Misty" = "Low",
                                   "James" = "High",
                                   "Sammie" = "Low",
                                   "No Name" = "None")) %>%
  mutate(applicant_2_high_comp = as.character(applicant_2_name),
         applicant_2_high_comp = recode(applicant_2_name,
                                        "Misty" = 0,
                                        "James" = 1,
                                        "Sammie" = 0,
                                        "No Name" = 2)) %>%
  mutate(applicant_2_sex = as.character(applicant_2_name),
         applicant_2_sex = recode(applicant_2_name,
                                  "Misty" = "Female",
                                  "James" = "Male",
                                  "Sammie" = "Male",
                                  "No Name" = "None")) %>%
  mutate(applicant_1_rate = "Excellent") %>%
  mutate(applicant_1_name = "Sandra") %>%
  mutate(applicant_1_char = "Sandra (Exc)") %>%
  mutate(ideo5_char = as.character(ideo5),
         ideo5_char = recode(ideo5,
                                  "1" = "Very Liberal",
                                  "2" = "Liberal",
                                  "3" = "Moderate",
                                  "4" = "Conservative",
                                  "5" = "Very Conservative"))
#  write_csv(gender, "Data/gender_df.csv")

gender$ideo5_char <- factor(gender$ideo5_char, 
                            levels=c('Very Conservative', 'Conservative',
                                     'Moderate', 'Liberal', 'Very Liberal'))


gender <- gender%>%
  group_by(applicant2_treat)%>%
  mutate(app_2_amt_av = mean(APP_2_amt_1))%>%
  mutate(app_1_amt_av = mean(APP_1_amt_1))%>%
  mutate(STATE_amt_av = mean(STATE_amt_1)) %>%
  subset(applicant2_treat < 7)
gender$amt_diff <- (gender$APP_2_amt_1 - gender$APP_1_amt_1)


for_fig <- gender %>%
  select(ideo5, ideo5_char, APP_1_amt_1, APP_2_amt_1, STATE_amt_1, 
         applicant_2_name, applicant_2_rate, applicant_2_sex)%>%
  pivot_longer(APP_1_amt_1:STATE_amt_1, names_to = "person", 
               values_to = "amount")
for_fig <- for_fig %>%
  group_by(person, ideo5) %>%
  mutate(av_amount = mean(amount))%>%
  mutate(n = length(av_amount)) %>%
  mutate(sd = sd(amount)) %>%
  mutate(se = sd/sqrt(n)) %>%
  ungroup() %>%
  filter(ideo5 < 6) #drop no-name

ggplot(for_fig, aes(x=ideo5_char, y=av_amount)) + 
  geom_point() +
  theme_bw() +
  facet_wrap(vars(applicant_2_rate, applicant_2_name))+
  geom_errorbar(aes(ymin =av_amount-1.96*se, ymax=av_amount+1.96*se)) +
  labs(x = "Ideology",
       y = "Average Dollars Awarded")


#Exc Misty to Baseline <- one group
excmist <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1, ideo5_char, amt_diff) %>%
  filter(applicant2_treat == 1)

excmist$sandra <- excmist$APP_1_amt_1
excmist$emist <- excmist$APP_2_amt_1

library(rstatix)
library(ggpubr)

rob_res_1 <- t_test(data = excmist, amt_diff ~ ideo5_char) %>%
  select(group1, group2, n1, n2, p)

stargazer::stargazer(rob_res_1, type = "latex", summary=F, 
          title="Ideological Differences in Excellent Misty Treatment")

#Exc James to Baseline
excjames <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1, amt_diff, ideo5_char) %>%
  filter(applicant2_treat == 3)

rob_res_2 <- t_test(data = excjames, amt_diff ~ ideo5_char)%>%
  select(group1, group2, n1, n2, p)
stargazer::stargazer(rob_res_2, type = "latex", summary=F, 
                     title="Ideological Differences in Excellent James Treatment")

##reg for amount given to state
df_reg <- gender %>%
  select(amt_diff, applicant_2_rate, applicant_2_high_comp, 
         applicant_2_sex, ideo5,
         applicant_2_name, APP_1_amt_1, APP_2_amt_1, STATE_amt_1) %>%
  mutate(app2_exc = as.numeric(applicant_2_rate),
         app2_exc = recode(applicant_2_rate,
                           "Excellent" = 1,
                           "Poor" = 0)) %>%
  mutate(app2_fem = as.numeric(applicant_2_sex),
         app2_fem = recode(applicant_2_sex,
                           "Female" = 1,
                           "Male" = 0,
                           "None" = 3)) %>%
  subset(app2_fem < 2)

p <- lm(STATE_amt_1 ~ ideo5 + app2_fem + app2_exc + 
          applicant_2_high_comp, 
        data = df_reg)
#summary(p)

stargazer::stargazer(p, type="latex", style = "apsr",
                     covariate.labels = c("Ideology", 
                                          "Treatment Female", 
                                          "Rated Excellent",
                                          "High Competence"),
                     dep.var.labels = c("Dollars Given to State"))
ggsave("Paper/figs/robust-results-R-gender.png", height = 6, width = 8)





######regression for robustness
###SOMETHING BROKE CHECK THIS
df_reg <- gender %>%
  select(amt_diff, applicant_2_rate, applicant_2_high_comp, applicant_2_sex,
         applicant_2_name, APP_1_amt_1, APP_2_amt_1, STATE_amt_1) %>%
  mutate(app2_exc = as.numeric(applicant_2_rate),
         app2_exc = recode(applicant_2_rate,
                           "Excellent" = 1,
                           "Poor" = 0)) %>%
  mutate(app2_fem = as.numeric(applicant_2_sex),
         app2_fem = recode(applicant_2_sex,
                           "Female" = 1,
                           "Male" = 0)) 

just_sex <- lm(amt_diff ~ app2_fem, data=df_reg)

w_controls <- lm(amt_diff ~ app2_fem + app2_exc + applicant_2_high_comp + 
                   STATE_amt_1, 
                 data=df_reg)

###this is what's breaking - throwing NAs ??????
w_comp_inter <- lm(amt_diff ~ app2_fem*applicant_2_high_comp + 
                     app2_exc + STATE_amt_1,
                   data=df_reg)


w_qual_inter <- lm(amt_diff ~ app2_fem*app2_exc + applicant_2_high_comp + STATE_amt_1,
                   data=df_reg)

stargazer::stargazer(just_sex, w_controls, w_comp_inter, w_qual_inter, 
                     style = "ajps", type="text")

just_sex2 <- lm(APP_2_amt_1 ~ app2_fem, data=df_reg)
w_controls2 <- lm(APP_2_amt_1 ~ app2_fem + app2_exc + applicant_2_high_comp + STATE_amt_1, 
                  data=df_reg)
w_comp_inter2 <- lm(APP_2_amt_1 ~ app2_fem*applicant_2_high_comp + app2_exc + STATE_amt_1,
                    data=df_reg)
w_qual_inter2 <- lm(APP_2_amt_1 ~ app2_fem*app2_exc + applicant_2_high_comp + STATE_amt_1,
                    data=df_reg)

stargazer::stargazer(just_sex2, w_controls2, w_comp_inter2, w_qual_inter2, 
                     style = "ajps", type="text")
library(tidyverse)
library(viridis)
library(haven)
library(labelled)
library(labeling)
library(modelsummary)

#load df
df <- read_sav("Data/FLSU0010_OUTPUT.sav") 
var_label(df) <- NULL
write_csv(df, "Data/FLSU0010_OUTPUT.csv")

#tidy and shape
gender <- df %>%
  select(caseid, applicant2_treat, APP_1_amt_1, APP_2_amt_1, STATE_amt_1) %>%
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
  mutate(applicant_1_char = "Sandra (Exc)")
write_csv(gender, "Data/gender_df.csv")

gender$amt_diff <- (gender$APP_2_amt_1 - gender$APP_1_amt_1)

gender <- gender%>%
  group_by(applicant2_treat)%>%
  mutate(app_2_amt_av = mean(APP_2_amt_1))%>%
  mutate(app_1_amt_av = mean(APP_1_amt_1))%>%
  mutate(STATE_amt_av = mean(STATE_amt_1))

##analysis

for_fig2 <- gender %>%
  select(applicant2_treat, app_1_amt_av, app_2_amt_av, STATE_amt_av, 
         applicant_2_name, applicant_2_rate)%>%
  pivot_longer(app_1_amt_av:STATE_amt_av, names_to = "person", 
               values_to = "amount")

for_fig2 <- for_fig2 %>%
  group_by(applicant2_treat) %>%
  mutate(se = (sd(amount)/sqrt(length((amount))))) %>%
  filter(applicant2_treat < 7) #drop no-name




ggplot(for_fig, aes(x=person, y=amount, color=female)) +
  geom_point(aes(shape=person, color=female)) + theme_bw()+
  scale_color_viridis_d(name="R Female?") +
  scale_shape_discrete(name="Recipient",
                     labels=c("Sandra", "Treatment", "State")) +
  geom_errorbar(aes(ymin=amount-(se*1.96), ymax=amount+(se*1.96)),
                width=.2) +
  facet_wrap(vars(applicant_2_rate)) +
  labs(x = "",
       y = "Average Dollars Awarded") +
  theme(axis.text.x = element_blank())
ggsave("figs/general_results_name.png")


##Difference in means
#Exc Misty to Baseline <- one group
gender <- gender %>%
  subset(applicant2_treat < 7)
excmist <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 1)
sandra <- excmist$APP_1_amt_1
emist <- excmist$APP_2_amt_1
res1 <- t.test(sandra, emist, paired = TRUE)
res1

#Poor Misty to Baseline <- one group
poormist <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 2)
sandra2 <- poormist$APP_1_amt_1
pmist <- poormist$APP_2_amt_1
res2 <- t.test(sandra2, pmist, paired = TRUE)
res2

#Exc James to Baseline
excjames <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 3)
sandra3 <- excjames$APP_1_amt_1
ejames <- excjames$APP_2_amt_1
res3 <- t.test(sandra3, ejames, paired = TRUE)
res3

#Poor James to Baseline
poorjames <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 4)
sandra4 <- poorjames$APP_1_amt_1
pjames <- poorjames$APP_2_amt_1
res4 <- t.test(sandra4, pjames, paired = TRUE)
res4

#Exc Sammie to Baseline
excsam <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 5)
sandra5 <- excsam$APP_1_amt_1
esam <- excsam$APP_2_amt_1
res5 <- t.test(sandra5, esam, paired = TRUE)
res5

#Poor Sammie to Baseline
poorsam <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1) %>%
  filter(applicant2_treat == 6)
sandra6 <- poorsam$APP_1_amt_1
psam <- poorsam$APP_2_amt_1
res6 <- t.test(sandra6, psam, paired = TRUE)
res6

#Excellent Misty to Poor Misty
res7 <- t.test(emist, pmist, paired = FALSE)
res7

#Excellent James to Poor James
res8 <- t.test(ejames, pjames, paired = FALSE)
res8

#Excellent Sammie to Poor Sammie
res9 <- t.test(esam, psam, paired = FALSE)
res9

#Excellent Misty to Excellent Sam
res10 <- t.test(emist, esam, paired = FALSE)
res10

#Poor Misty to Poor Sam
res11 <- t.test(pmist, psam, paired = FALSE)
res11

#Excellent James to Excellent Sam
res12 <- t.test(ejames, esam, paired = FALSE)
res12
#Poor James to Poor Sam
res13 <- t.test(pjames, psam, paired = FALSE)
res13

#Excellent Misty to Excellent James
res14 <- t.test(emist, ejames, paired = FALSE)
res14

#Poor Misty to Poor James
res15 <- t.test(pmist, pjames, paired = FALSE)
res15

##scatter
ggplot(gender, aes(x=APP_1_amt_1, y=APP_2_amt_1)) + 
  geom_point() +
  theme_bw()


df2 <- select(gender, -applicant_2_char)
ggplot(gender, aes(x=APP_1_amt_1, y=APP_2_amt_1)) + theme_bw() +
  geom_point(data = df2, color = "grey70", alpha=.4) +
  geom_point(alpha=.6, aes(color = applicant_2_char)) + 
  scale_color_manual(values=c("#000000", "#000000", "#000000",
                              "#000000", "#000000", "#000000",
                              "#000000")) +
  facet_wrap(~applicant_2_char) +
  theme(legend.position = "none") +
  labs(title = "Dollars Given By Treatment Condition",
       x = " $$ Given to Sandra (Exc)",
       y = "$$ Given to Treatment",
       color = "Treatment")
ggsave("figs/dollars-given-by-treatment-condition.png")

df_reg <- df2 %>%
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
w_controls <- lm(amt_diff ~ app2_fem + app2_exc + applicant_2_high_comp + STATE_amt_1, 
                 data=df_reg)
w_comp_inter <- lm(amt_diff ~ app2_fem*applicant_2_high_comp + app2_exc + STATE_amt_1,
                   data=df_reg)
w_qual_inter <- lm(amt_diff ~ app2_fem*app2_exc + applicant_2_high_comp + STATE_amt_1,
                   data=df_reg)

stargazer::stargazer(just_sex, w_controls, w_comp_inter, w_qual_inter, 
                     style = "ajps", type="latex")

just_sex2 <- lm(APP_2_amt_1 ~ app2_fem, data=df_reg)
w_controls2 <- lm(APP_2_amt_1 ~ app2_fem + app2_exc + applicant_2_high_comp + STATE_amt_1, 
                 data=df_reg)
w_comp_inter2 <- lm(APP_2_amt_1 ~ app2_fem*applicant_2_high_comp + app2_exc + STATE_amt_1,
                   data=df_reg)
w_qual_inter2 <- lm(APP_2_amt_1 ~ app2_fem*app2_exc + applicant_2_high_comp + STATE_amt_1,
                   data=df_reg)

stargazer::stargazer(just_sex2, w_controls2, w_comp_inter2, w_qual_inter2, 
                     style = "ajps", type="text")

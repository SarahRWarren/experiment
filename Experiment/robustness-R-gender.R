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
  select(caseid, applicant2_treat, APP_1_amt_1, APP_2_amt_1, 
         STATE_amt_1,
         gender) %>%
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
  mutate(gender_char = as.character(gender),
         gender_char = recode(gender,
                           "1" = "Male",
                           "2" = "Female")) %>%
  mutate(applicant_1_rate = "Excellent") %>%
  mutate(applicant_1_name = "Sandra") %>%
  mutate(applicant_1_char = "Sandra (Exc)")
#  write_csv(gender, "Data/gender_df.csv")

gender <- gender%>%
  group_by(applicant2_treat)%>%
  mutate(app_2_amt_av = mean(APP_2_amt_1))%>%
  mutate(app_1_amt_av = mean(APP_1_amt_1))%>%
  mutate(STATE_amt_av = mean(STATE_amt_1)) %>%
  subset(applicant2_treat < 7)
gender$amt_diff <- (gender$APP_2_amt_1 - gender$APP_1_amt_1)

##t-tests
excmist <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1, amt_diff, gender_char) %>%
  filter(applicant2_treat == 1)

excmist$sandra <- excmist$APP_1_amt_1
excmist$emist <- excmist$APP_2_amt_1

rob_res_1 <- t_test(data = excmist, amt_diff ~ gender_char)%>%
  select(group1, group2, n1, n2, p) %>%
  mutate(treatment_name = "Excellent Misty")

#Exc James to Baseline
excjames <- gender %>%
  select(applicant2_treat, APP_1_amt_1, APP_2_amt_1, amt_diff, gender_char) %>%
  filter(applicant2_treat == 3)

rob_res_2 <- t_test(data = excjames, amt_diff ~ gender_char)%>%
  select(group1, group2, n1, n2, p) %>%
  mutate(treatment_name = "Excellent James")

results <- full_join(rob_res_1, rob_res_2)
stargazer::stargazer(results, type = "latex", summary=F, 
                     title="Gender Differences in Allocations")





for_fig <- gender %>%
  select(gender, gender_char, APP_1_amt_1, APP_2_amt_1, STATE_amt_1, 
         applicant_2_name, applicant_2_rate, applicant_2_sex)%>%
  pivot_longer(APP_1_amt_1:STATE_amt_1, names_to = "person", 
               values_to = "amount")
for_fig <- for_fig %>%
  group_by(person, gender_char) %>%
  mutate(av_amount = mean(amount))%>%
  mutate(n = length(av_amount)) %>%
  mutate(sd = sd(amount)) %>%
  mutate(se = sd/sqrt(n)) 


ggplot(for_fig, aes(x=person, y=av_amount, color = gender_char)) + 
  geom_point(aes(shape = person)) +
  facet_wrap(vars(applicant_2_rate, applicant_2_name)) +
  geom_errorbar(aes(ymin =av_amount - 1.96*se, 
                    ymax=av_amount + 1.96*se), width = 0.3) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  scale_shape_discrete(labels=c('Baseline', 'Treatment',
                                'State')) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank()) +
  labs(x = "",
       y = "Average Dollars Awarded",
       shape = "Recipient",
       color = "Respondent Gender")
ggsave("Paper/figs/robust-results-R-gender.png", height = 6, width = 8)



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
write_csv(df, "Data/FLSU0010_OUTPUT.csv")

#tidy and shape
gender <- df %>%
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
  subset(faminc_new < 90) %>%
  subset(ideo5 < 6) %>%
  subset(pid7 < 8)
#  write_csv(gender, "Data/gender_df.csv")

gender$amt_diff <- (gender$APP_2_amt_1 - gender$APP_1_amt_1)

gender <- gender%>%
  group_by(applicant2_treat)%>%
  mutate(app_2_amt_av = mean(APP_2_amt_1))%>%
  mutate(app_1_amt_av = mean(APP_1_amt_1))%>%
  mutate(STATE_amt_av = mean(STATE_amt_1))

###family income distribution
ggplot(gender, aes(x=faminc_new)) + geom_histogram() +
  theme_bw() +
  labs(x = "Income Scale",
       y = "Count",
       title = "Income Distribution in Full Sample")
  

#gender by treatment condition

### ideology distribution

ggplot(gender, aes(x=ideo5)) + geom_histogram() +
  theme_bw() +
  labs(x = "5pt Ideology Scale",
       y = "Count",
       title = "Ideology Distribution in Full Sample")
ggsave("Paper/figs/ideo-dist.png")

### pid distribution
ggplot(gender, aes(x=pid7)) + geom_histogram() +
  theme_bw() +
  labs(x = "7pt Party-ID",
       y = "Count",
       title = "Partisan Distribution in Full Sample")
ggsave("Paper/figs/party-dist.png")

##welfare FT
ggplot(gender, aes(x=FT_WELFARE)) + geom_histogram() +
  theme_bw() +
  labs(x = "Welfare FT",
       y = "Count",
       title = "Welfare FT Distribution in Full Sample")
ggsave("Paper/figs/ft-welfare-dist.png")

##aid FT
ggplot(gender, aes(x=FT_AID)) + geom_histogram() +
  theme_bw() +
  labs(x = "Public Aid FT",
       y = "Count",
       title = "Public Aid FT Distribution in Full Sample")
ggsave("Paper/figs/ft-aid-dist.png")

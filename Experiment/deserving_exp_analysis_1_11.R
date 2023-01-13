library(tidyverse)
library(viridis)
library(haven)
library(labelled)
library(labeling)
library(modelsummary)
library(gridExtra)

#load df
df <- read_csv("Data/FLSU0010_OUTPUT.csv") %>%
  select(weight_genpop, party_treat, program_treat,
         FT_PROG_1, PARTICIPATE_PROG, VOTE_GOV, REELECT_GOV, pid3, pid7) %>%
  drop_na()

tidy <- df %>%
  mutate(party_treat_char = as.character(party_treat),
         party_treat_char = recode(party_treat,
                                   "1" = "Republican",
                                   "2" = "Democrat",
                                   "3" = "None"))%>%
  mutate(program_treat_char = as.character(program_treat),
         program_treat_char = recode(program_treat,
                                   "1" = "Universal",
                                   "2" = "Deserving Poor",
                                   "3" = "Poverty",
                                   "4" = "Control")) %>%
  mutate(VOTE_GOV_char = as.character(VOTE_GOV),
         VOTE_GOV_char = recode(VOTE_GOV,
                                     "1" = "Very Likely",
                                     "2" = "Likely",
                                     "3" = "Somewhat Likely",
                                     "4" = "Somewhat Unlikely",
                                    "5" = "Unlikely",
                                    "6" = "Very Unlikely",
                                    "8" = "skipped")) %>%
  mutate(REELECT_GOV_char = as.character(REELECT_GOV),
         REELECT_GOV_char = recode(REELECT_GOV,
                                "1" = "Very Likely",
                                "2" = "Likely",
                                "3" = "Somewhat Likely",
                                "4" = "Somewhat Unlikely",
                                "5" = "Unlikely",
                                "6" = "Very Unlikely")) %>%
  mutate(pid3_char = pid3,
         pid3_char = recode(pid3,
                            "1" = "2",
                            "2" = "1",
                            "3" = "3",
                            "4" = "4",
                            "5" = "5"))%>%
   mutate(pid3_char = as.character(pid3),
         pid3_char = recode(pid3,
                                   "1" = "Republican",
                                   "2" = "Democrat",
                                   "3" = "Independent",
                                   "4" = "Other",
                                   "5" = "Unsure"))%>%
  mutate(pid7_char = as.character(pid7),
         pid7_char = recode(pid7,
                            "1" = "Strong Democrat",
                            "2" = "Not very strong Democrat",
                            "3" = "Lean Democrat",
                            "4" = "Independent",
                            "5" = "Lean Republican",
                            "6" = "Not very strong Republican",
                            "7" = "Strong Republican",
                            "8" = "Unsure",
                            "9" = "Don't know"))

##create copartisan variable
##DROP nonpartisans and unsure
##drop no party treatment for robustness
#1 if pid3 and party_treat MATCH (1:1 or 2:2), 0 if not (1:2 or 2:1)
## matching is copartisan condition, mismatched is out-party condition
#H5-7 deal with this
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
   mutate(pid3_char = as.character(pid3),
         pid3_char = recode(pid3,
                                   "1" = "Democrat",
                                   "2" = "Republican",
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

##FOR ANALYSIS
##create copartisan variable
##DROP nonpartisans and unsure
##drop no party treatment for robustness
#1 if pid3 and party_treat MATCH (1:1 or 2:2), 0 if not (1:2 or 2:1)
## matching is copartisan condition, mismatched is out-party condition
tidy <- tidy %>%
  subset(pid7 != 4 &  pid7 != 8 &  pid7 != 8)

tidy[tidy$pid7 == 1 & tidy$party_treat == 2,'copartisan'] <- 1
tidy[tidy$pid7 == 2 & tidy$party_treat == 2,'copartisan'] <- 1
tidy[tidy$pid7 == 3 & tidy$party_treat == 2,'copartisan'] <- 1

tidy[tidy$pid7 == 5 & tidy$party_treat == 1,'copartisan'] <- 1
tidy[tidy$pid7 == 6 & tidy$party_treat == 1,'copartisan'] <- 1
tidy[tidy$pid7 == 7 & tidy$party_treat == 1,'copartisan'] <- 1

tidy[tidy$pid7 == 1 & tidy$party_treat == 1,'copartisan'] <- 0
tidy[tidy$pid7 == 2 & tidy$party_treat == 1,'copartisan'] <- 0
tidy[tidy$pid7 == 3 & tidy$party_treat == 1,'copartisan'] <- 0

tidy[tidy$pid7 == 5 & tidy$party_treat == 2,'copartisan'] <- 0
tidy[tidy$pid7 == 6 & tidy$party_treat == 2,'copartisan'] <- 0
tidy[tidy$pid7 == 7 & tidy$party_treat == 2,'copartisan'] <- 0

tidy[tidy$pid7 == 1 & tidy$party_treat == 3,'copartisan'] <- 2
tidy[tidy$pid7 == 2 & tidy$party_treat == 3,'copartisan'] <- 2
tidy[tidy$pid7 == 3 & tidy$party_treat == 3,'copartisan'] <- 2

tidy[tidy$pid7 == 5 & tidy$party_treat == 3,'copartisan'] <- 2
tidy[tidy$pid7 == 6 & tidy$party_treat == 3,'copartisan'] <- 2
tidy[tidy$pid7 == 7 & tidy$party_treat == 3,'copartisan'] <- 2

##coparisan condition is 1. outpartisan condition is 0.
#no party condition is 2
tidy <- tidy %>%
  mutate(copartisan_char = as.character(copartisan),
         copartisan_char = recode(copartisan,
                                   "1" = "Co-Party",
                                   "0" = "Out-Party",
                                   "2" = "No Party"))
#calculate the mean for each cell
tidy2 <- tidy %>%
  group_by(program_treat) %>%
  mutate(mean_rating_prog = mean(FT_PROG_1))%>%
  mutate(se_prog = (sd(FT_PROG_1)/sqrt(length((FT_PROG_1)))))%>%
  ungroup() %>%
  group_by(program_treat, party_treat) %>%
  mutate(mean_rating_prog_party = mean(FT_PROG_1))%>%
  mutate(se_prog_party = (sd(FT_PROG_1)/sqrt(length((FT_PROG_1)))))%>%
  ungroup() %>%
  group_by(program_treat, copartisan) %>%
  mutate(mean_rating_copart = mean(FT_PROG_1)) %>%
  mutate(se_copart = (sd(FT_PROG_1)/sqrt(length((FT_PROG_1)))))%>%
  ungroup() %>%
  group_by(pid3, program_treat, copartisan) %>%
  mutate(mean_rating_partisan_copart = mean(FT_PROG_1)) %>%
  mutate(se_party_copart = (sd(FT_PROG_1)/sqrt(length((FT_PROG_1)))))%>%
  ungroup() %>%
  subset(pid3 == 1 | pid3 == 2)

ggplot(tidy2, aes(x = program_treat_char, y= mean_rating_copart,
                  color = copartisan_char)) +
  geom_point() + 
  geom_errorbar(aes(ymin=mean_rating_copart - 1.96*se_copart, 
                    ymax=mean_rating_copart + 1.96*se_copart,
                    width = .3)) +
  theme_bw()



ggplot(tidy2, aes(x = program_treat_char, y= mean_rating_partisan_copart,
                  color = pid3_char)) +
  geom_errorbar(aes(ymin=mean_rating_partisan_copart - 1.96*se_party_copart, 
                    ymax=mean_rating_partisan_copart + 1.96*se_party_copart,
                    width = .3)) +
  facet_wrap(vars(copartisan_char)) +
  geom_point() +
  theme_bw()

ggplot(tidy2, aes(x = program_treat_char, y= mean_rating_partisan_copart,
                  color = copartisan_char)) +
  geom_errorbar(aes(ymin=mean_rating_partisan_copart - 1.96*se_party_copart, 
                    ymax=mean_rating_partisan_copart + 1.96*se_party_copart,
                    width = .3)) +
  facet_wrap(vars(pid3_char)) +
  geom_point() +
  theme_bw()

ggplot(tidy2, aes(x = REELECT_GOV, y=copartisan_char)) +
  geom_boxplot()
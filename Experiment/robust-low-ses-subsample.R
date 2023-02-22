##robust low SESoversample
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

gender <- df %>%
  select(caseid, applicant2_treat, APP_1_amt_1, APP_2_amt_1, STATE_amt_1, weight_oversample) %>%
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
#  write_csv(gender, "Data/gender_df.csv")

gender$amt_diff <- (gender$APP_2_amt_1 - gender$APP_1_amt_1)

gender <- gender%>%
  group_by(applicant2_treat)%>%
  mutate(app_2_amt_av = mean(APP_2_amt_1))%>%
  mutate(app_1_amt_av = mean(APP_1_amt_1))%>%
  mutate(STATE_amt_av = mean(STATE_amt_1))

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



##break into paired and unpaired tables

tests <- list(res1, res3, res5)
tab <- sapply(tests, function(x) {
  c(x$estimate[1],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

tab <- as.data.frame(tab) %>%
  rename("Excellent Misty vs Baseline" = V1) %>%
  rename("Excellent James vs Baseline" = V2)%>%
  rename("Excellent Sammie vs Baseline" = V3)%>%
  mutate(measure = c("Mean Difference", "Upper CI", "Lower CI",
                "p-value")) %>%
  pivot_longer(
    cols = !measure,
    names_to = "Condition",
    values_to = "Results"
  ) %>%
  pivot_wider(names_from = "measure", values_from = "Results")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

tab <- round_df(tab, 3)

stargazer::stargazer(tab, type = "latex", style = "ajps", 
                     summary = FALSE,
                     font.size = 'footnotesize', 
                     column.sep.width = "1pt", 
                     no.space=T,
                     digits.extra=3)





tests2 <- list(res7, res8, res9)
tab2 <- sapply(tests2, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
})

tab2 <- as.data.frame(tab2) %>%
  rename("Excellent Misty vs Poor Misty" = V1)%>%
  rename("Excellent James vs Poor James" = V2)%>%
  rename("Excellent Sammie vs Poor Sammie" = V3) %>%
  mutate(measure = c("Mean 1", "Mean 2", "Upper CI", "Lower CI",
                     "p-value")) %>%
  pivot_longer(
    cols = !measure,
    names_to = "Condition",
    values_to = "Results"
  ) %>%
  pivot_wider(names_from = "measure", values_from = "Results")

tab2 <- round_df(tab2, 3)

stargazer::stargazer(tab2, type = "latex", style = "ajps", 
                     summary = FALSE,
                     font.size = 'footnotesize', 
                     column.sep.width = "1pt", 
                     no.space=T,
                     digits.extra=3)


###figures
##PLOTS
for_fig2 <- gender %>%
  select(applicant2_treat, app_1_amt_av, app_2_amt_av, STATE_amt_av, 
         applicant_2_name, applicant_2_rate, amt_diff, applicant_2_sex)%>%
  pivot_longer(app_1_amt_av:STATE_amt_av, names_to = "person", 
               values_to = "amount")

for_fig2 <- for_fig2 %>%
  group_by(applicant2_treat) %>%
  mutate(se = (sd(amount)/sqrt(length((amount))))) %>%
  filter(applicant2_treat < 7) #drop no-name

summary(for_fig2$amount)
ggplot(for_fig2, aes(x=person, y=amount, color = applicant_2_rate)) + 
  geom_point(aes(shape = person)) +
  facet_wrap(vars(applicant_2_name)) +
  geom_errorbar(aes(ymin =amount - 1.96*se, 
                    ymax=amount + 1.96*se,  width = 0.3)) +
  theme_classic() +
  scale_color_viridis(discrete = TRUE) +
  scale_shape_discrete(labels=c('Baseline', 'Treatment', 'State')) +
  theme(axis.text.x = element_blank()) +
#  theme(legend.key.height = unit(.9, 'mm'),
#        legend.position = c(.8,.4)) +
  labs(x = "",
       y = "Average Dollars Awarded",
       shape = "Recipient",
       color = "Quality of Treatment Name")
ggsave("Paper/figs/results-low-ses.png", height = 4, width = 5)

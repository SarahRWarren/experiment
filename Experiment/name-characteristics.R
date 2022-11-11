library(tidyverse)
library(viridis)

names <- read.csv("Data/names.csv") %>%
  subset(char == "working class" | char == "professional" |
           char == "likable" | char == "honest" |
           char == "hardworking" | char == "competent")

ggplot(names, aes(x=char, y=est)) + 
  geom_errorbar(aes(x=char, ymin=est-se, ymax=est+se), 
                width=0.4, alpha=0.9, size=1) +
  geom_point(aes(shape=name), size=3, alpha=.8) +
  coord_flip() + theme_minimal() +
  labs(title = "Name Characteristics",
       subtitle = "From Hayes and Mitchell (2020)",
       x = "Characteristics",
       y = "Estimates",
       shape = "Name")
ggsave("figs/characteristics.png", width = 6, height = 4)

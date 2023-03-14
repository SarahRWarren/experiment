library(tidyverse)
library(viridis)

names <- read.csv("Data/names.csv") %>%
  subset(char == "working class" | char == "professional" |
           char == "likable" | char == "honest" |
           char == "hardworking" | char == "competent")

ggplot(names, aes(x=char, y=est)) + 
  geom_errorbar(aes(x=char, ymin=est-se, ymax=est+se, color=name), 
                width=0.4, size=1) +
  geom_point(aes(color=name, shape = ), size=3, alpha=.8) +
  coord_flip() + theme_minimal() +
  scale_color_viridis_d() +
  labs(title = "Name Characteristics",
       subtitle = "From Hayes and Mitchell (2022)",
       x = "Characteristics",
       y = "Estimates",
       color = "Name")
ggsave("Paper/figs/characteristics.png", width = 6, height = 4)

library(tidyverse)
library(viridis)

names <- read.csv("~/Practicum/experiment/Experiment/Data/names.csv")

ggplot(names, aes(x=char, y=est, color=name)) + geom_point() +
  geom_errorbar(aes(x=char, ymin=est-se, ymax=est+se), 
                width=0.4, alpha=0.9, size=1.3) +
  coord_flip() + theme_minimal() +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Name Characteristics",
       subtitle = "From Hayes and Mitchell (2020)",
       x = "Characteristics",
       y = "Estimates")
ggsave("characteristics.png")

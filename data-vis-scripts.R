library(tidyverse)

penguins <- read.csv('data/penguins.csv')

head(penguins)

penguins_not2007 <- penguins %>% filter(year != 2007)
penguins %>% filter(species %in% c("Gentoo", "Chinstrap"))
penguins %>% select(species, island, flipper_length_mm)

penguins %>% 
  filter(year == 2007) %>% 
  select(species, island)

penguins_kg <- penguins %>% 
                mutate(body_mass_kg = body_mass_g/1000)

penguins_test <- penguins %>% 
  mutate(species_island = paste(species, island, sep = "-"))

penguins_summary <- penguins %>% 
  group_by(species) %>% 
  summarize(mean_bodymass = mean(body_mass_g, na.rm = TRUE),
            se = sd(body_mass_g, na.rm = TRUE) / sqrt(n()))

penguins_summary %>% 
  ggplot(aes(x = species, y = mean_bodymass)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_bodymass - se, ymax = mean_bodymass + se, width = 0.2))

bodymassplot <- penguins %>% 
              ggplot(aes(x = species, y = body_mass_g)) +
              geom_violin() +
              geom_jitter(width = 0.2, alpha = 0.5)
bodymassplot

ggsave('body_mass_plot.png', bodymassplot, width = 8, height = 8, units = "cm")



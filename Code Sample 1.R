library (palmerpenguins)
library (tidyverse)
data("penguins")


CleanPenguins <- penguins %>%
  na.omit() %>%
  select (-year, -island)


LongPenguins <- CleanPenguins %>%
  pivot_longer(cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g), names_to = "measurement")


PenguinStats <- LongPenguins %>%
  group_by(sex, measurement) %>%
  summarize (Min = min(value),
             Avg = mean (value),
             Max = max (value),
             StdDev = sd(value))%>%
  arrange (measurement)

PenguinAvg <- PenguinStats %>%
  select (-Min, -Max,-StdDev) %>%
  pivot_wider(names_from =   sex, values_from = Avg )


CleanPenguins %>%
  ggplot (aes (x= bill_length_mm, y = bill_depth_mm, color = species))+geom_point()

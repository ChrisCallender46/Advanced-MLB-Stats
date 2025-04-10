# Add OPS+ to data

# What is wRC+

# wRC+ =

library(tidyverse)

# Exploratory Data Analysis (EDA)
data <- WAR_vs_wRC_ |>
  select(!Player) |>
  filter(WAR_vs_wRC_$`Career wRC+` < 200)

data <- data |>
  mutate(
    OPS = OBP + SLG
  )

View(cor(data))
# wOBA, OBP, wRC+ and WAR are positively correlated
# One reason correlation may not be higher is WAR considers longevity
# A player who played 1 season may have a high wrc+, but a low WAR
# G and PA also strong correlation, but that is due to longevity
# A player that plays for longer, will accrue more WAR

ggplot(data, aes(x = `Career wRC+`, y = `Career WAR`)) +
  geom_point()

ggplot(data, aes(x = `Career wRC+`, y = `Career WAR`)) +
  geom_smooth()

ggplot(data, aes(x = `Career wRC+`, y = `Career WAR`)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = wOBA, y = `Career WAR`)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = OBP, y = `Career WAR`)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = OPS, y = `Career WAR`)) +
  geom_point() +
  geom_smooth()

# We can see that the line of best fit has positive slope,
# implying positive correlation

data_norm <- scale(data)
data_pca <- princomp(data_norm)
summary(data_pca)

data_pca$loadings[, 1:3]

# wOBA, OPS+, and wRC+ have the highest weights
# This means they explain the most variation of the data

# I think wOBA, wRC+, and OPS are the most important stats for determining hitting ability
# WAR encompasses every aspect of the game



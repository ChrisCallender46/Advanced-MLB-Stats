library(tidyverse)

# Exploratory Data Analysis (EDA)
data <- WAR_vs_wRC_ |>
  select(!Player) |>
  filter(WAR_vs_wRC_$`wRC+` < 200)

data <- data |>
  mutate(
    OPS = OBP + SLG
  )

View(cor(data))

ggplot(data, aes(x = `wRC+`, y = `WAR`)) +
  geom_point() +
  geom_smooth()

ggplot(data, aes(x = wOBA, y = `WAR`)) +
  geom_point() +
  geom_smooth()

# ggplot(data, aes(x = OBP, y = `WAR`)) +
#   geom_point() +
#   geom_smooth()
# 
# ggplot(data, aes(x = OPS, y = `WAR`)) +
#   geom_point() +
#   geom_smooth()

ggplot(data, aes(x = `OPS+`, y = `WAR`)) +
  geom_point() +
  geom_smooth()

# We can see that the line of best fit has positive slope,
# That means positive correlation

data_norm <- scale(data)
data_pca <- princomp(data_norm)
summary(data_pca)

data_pca$loadings[, 1:3]



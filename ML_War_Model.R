# Build ML model to predict WAR

library(e1071)
library(caret)
library(Metrics)

model_data <- data |>
  select(!`WAR`)

WAR_equation <- "`WAR` ~ ."
WAR_formula <- as.formula(WAR_equation)

war_model <- lm(formula = WAR_formula, data = data, seed = 42)
WAR <- predict(war_model, newdata = model_data)

train <- WAR_vs_wRC_ |>
  filter(`wRC+` < 200)

player <- train$Player

output_df <- as.data.frame(player)
output_df$WAR <- WAR

true_war <- WAR_vs_wRC_ |>
  select(Player, `WAR`) |>
  filter(WAR_vs_wRC_$`wRC+` < 200)

# Calculate r**2 value
summary(war_model)$r.squared
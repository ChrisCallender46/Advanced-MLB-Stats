# Build ML model to predict WAR

model_data <- data |>
  select(!`Career WAR`)

WAR_equation <- "`Career WAR` ~ ."
WAR_formula <- as.formula(WAR_equation)

war_model <- lm(formula = WAR_formula, data = data, seed = 42)
WAR <- predict(war_model, newdata = model_data)

train <- WAR_vs_wRC_ |>
  filter(`Career wRC+` < 200)

player <- train$Player

output_df <- as.data.frame(player)
output_df$WAR <- WAR

# Compare output_df to train dataset
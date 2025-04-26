# Build ML model to predict WAR

library(e1071)
library(caret)

model_data <- data |>
  select(!`WAR`)

WAR_equation <- "`WAR` ~ ."
WAR_formula <- as.formula(WAR_equation)

war_model <- svm(formula = WAR_formula, data = data, seed = 42)
WAR <- predict(war_model, newdata = model_data)

train <- WAR_vs_wRC_ |>
  filter(`wRC+` < 200)

player <- train$Player

output_df <- as.data.frame(player)
output_df$WAR <- WAR

true_war <- WAR_vs_wRC_ |>
  select(Player, `WAR`) |>
  filter(WAR_vs_wRC_$`wRC+` < 200)

# Calculate precision and recall
ref_values <- train$`Career WAR`
ref_values_fact <- as.factor(ref_values)

WAR_values <- as.numeric(WAR)
WAR_values_fact <- as.factor(WAR_values)

precision <- posPredValue(WAR_values_fact, ref_values_fact, positive="1")
recall <- sensitivity(WAR, true_war$`WAR`, positive="1")

conf_matrix <- confusionMatrix(data = WAR_values_fact, reference = ref_values_fact)

                               
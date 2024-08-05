data <- read.csv("simul_freqRT.csv")
data$logFreq <- log(data$Freq)
head(data)


# Increase the standard deviation to introduce more variation
sd_value <- 100

# Simulate RT for each item and each person
data <- data %>%
  group_by(id) %>%
  mutate(RT = 500 + 500 / (Freq + 1) + rnorm(n(), mean = 0, sd = sd_value)) %>%
  ungroup()

write.table(data, file = "simul_freqRT_all.csv", row.names=FALSE, ,col.names=TRUE, sep="\t")


# Régression linéaire
model <- lm(RT ~ logFreq, data = data)
summary(model)
# Prédicmodel# Prédictions
data$Predictions <- predict(model, data)

# Graphique
ggplot(data, aes(x = logFreq, y = RT)) +
  geom_point(color = 'blue') + # Points de données
  geom_line(aes(y = Predictions), color = 'red') + # Ligne de régression
  labs(title = "Régression linéaire entre fréquence de mots et temps de réaction",
       x = "Fréquence de mots",
       y = "Temps de réaction") +
  theme_minimal()


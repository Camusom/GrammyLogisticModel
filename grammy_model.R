library('readxl')
library(bestglm)
library(ggplot2)
library(DescTools)
library(dplyr)
grammy_data = read_excel("C://Users//Massimo Camuso//Desktop//Academics//Spring 2025//Capstone//Grammy project//grammy_full_data.xlsx")

grammy_data$rhythm.bpm <- as.numeric(grammy_data$rhythm.bpm)
grammy_data$rhythm.danceability <- as.numeric(grammy_data$rhythm.danceability)
grammy_data$`Duration (ms)` <- as.numeric(grammy_data$`Duration (ms)`)
duration <- grammy_data$`Duration (ms)`/1000
grammy_data$Popularity <- as.numeric(grammy_data$Popularity)
grammy_data$tonal.chords_key <- as.factor(grammy_data$tonal.chords_key)
grammy_data$tonal.chords_scale <- as.factor(grammy_data$tonal.chords_scale)


grammy_model <- glm(win ~ rhythm.bpm , data=grammy_data, family=binomial)
summary(grammy_model)
best_grammy_model <- glm(win ~ rhythm.danceability + duration + Popularity, data=grammy_data, family=binomial)
summary(best_grammy_model)

nrow(na.omit(grammy_data))
data_subset <- grammy_data[, c("rhythm.bpm", "rhythm.danceability", "Duration (ms)", "Popularity", "tonal.chords_scale", "win")]
data_subset <- as.data.frame(data_subset)
best_model <- bestglm(
  Xy = data_subset,
  family = binomial,
  IC = "AIC",
  method = "exhaustive"
)

summary(best_model)
best_model$BestModel

PseudoR2(best_grammy_model, which="McFadden")

winners <- grammy_data %>% filter(win==1)
summary(winners$Popularity)

losers <- grammy_data %>% filter(win==0)
summary(losers$Popularity)


boxplot(
  duration ~ win,
  data=grammy_data,
  xlab = "Win",
  ylab = "danceability",
  col = c("lightblue", "salmon")
)

recent_win <- grammy_data %>% filter(Year >= 2000 & win == 1)
glm(recent_win ~ Popularity + rhythm.danceability + duration, data = recent_win, family = binomial)

avg_duration <- mean(duration)
avg_danceability <- mean(grammy_data$rhythm.danceability)
avg_popularity <- mean(grammy_data$Popularity)

avg_duration
avg_danceability
avg_popularity

# Define baseline values
baseline <- data.frame(
  rhythm.danceability = avg_danceability,
  duration = avg_duration,
  Popularity = avg_popularity
)

# Function to compute 1-unit effect
get_1unit_effect <- function(model, predictor, baseline_data, step = 1) {
  # Baseline probability
  p_baseline <- predict(model, newdata = baseline_data, type = "response")
  
  # New data: increment predictor by 1 unit
  new_data <- baseline_data
  new_data[[predictor]] <- new_data[[predictor]] + step
  
  # New probability
  p_new <- predict(model, newdata = new_data, type = "response")
  
  # Return difference
  return(p_new - p_baseline)
}

# Example: Danceability effect
get_1unit_effect(best_grammy_model, "Popularity", baseline)
# Output: ~ -0.568 (56.8% decrease)






# Define your song's features
not_like_us <- data.frame(
  rhythm.danceability = 1.169,  # Danceability value         #0.231
  duration = 274,             # Duration in seconds
  Popularity = 88            # Popularity score
)
texas_hold <- data.frame(
  rhythm.danceability = 1.236,  # Danceability value        #0.224
  duration = 233,             # Duration in seconds
  Popularity = 77            # Popularity score
)
espresso <- data.frame(
  rhythm.danceability = 1.277,  # Danceability value      #0.065
  duration = 175,             # Duration in seconds
  Popularity = 89            # Popularity score
) 
charli <- data.frame(
  rhythm.danceability = 1.111,  # Danceability value      #0.08
  duration = 133,             # Duration in seconds
  Popularity = 83            # Popularity score
) 
birds <- data.frame(
  rhythm.danceability = 1.467,  # Danceability value      #0.034
  duration = 210,             # Duration in seconds
  Popularity = 99            # Popularity score
) 
good_luck <- data.frame(
  rhythm.danceability = 1.222,  # Danceability value      #0.09
  duration = 218,             # Duration in seconds
  Popularity = 94            # Popularity score
) 
fortnight <- data.frame(
  rhythm.danceability = 1.215,  # Danceability value      #0.16
  duration = 228,             # Duration in seconds
  Popularity = 84            # Popularity score
) 
# Predict probability
prob_win <- predict(best_grammy_model, newdata = fortnight, type = "response")
prob_win  # Output: 0.423 (42.3%)




library(ggplot2)

# Create a data frame with song names and probabilities
nominees <- data.frame(
  song = c("not_like_us", "texas_hold", "espresso", "charli", "birds", "good_luck", "fortnight"),
  probability = c(
    predict(best_grammy_model, newdata = not_like_us, type = "response"),
    predict(best_grammy_model, newdata = texas_hold, type = "response"),
    predict(best_grammy_model, newdata = espresso, type = "response"),
    predict(best_grammy_model, newdata = charli, type = "response"),
    predict(best_grammy_model, newdata = birds, type = "response"),
    predict(best_grammy_model, newdata = good_luck, type = "response"),
    predict(best_grammy_model, newdata = fortnight, type = "response")
  )
)

# Plot
ggplot(nominees, aes(x = reorder(song, -probability), y = probability, fill = song)) +
  geom_col() +
  geom_text(aes(label = scales::percent(probability, accuracy = 0.1)), vjust = -0.5) +
  labs(
    title = "Predicted Probability of Winning Record of the Year (2025)",
    x = "Song",
    y = "Probability of Winning",
    fill = "Song"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.25)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


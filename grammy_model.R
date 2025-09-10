library('readxl')
library(bestglm)
library(ggplot2)
library(DescTools)
library(dplyr)
library(scales)
library(stringr)
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
get_1unit_effect(best_grammy_model, "duration", baseline)
# Output: ~ -0.568 (56.8% decrease)


winners <- grammy_data %>% filter(win == 1)
losers <- grammy_data %>% filter(win == 0)

# Duration difference (in seconds)
mean_duration_winners <- mean(winners$`Duration (ms)` / 1000, na.rm = TRUE)
mean_duration_losers <- mean(losers$`Duration (ms)` / 1000, na.rm = TRUE)
duration_diff <- mean_duration_winners - mean_duration_losers
print(paste("Duration difference:", round(duration_diff, 0), "seconds"))

# Popularity difference  
mean_pop_winners <- mean(winners$Popularity, na.rm = TRUE)
mean_pop_losers <- mean(losers$Popularity, na.rm = TRUE)
pop_diff <- mean_pop_winners - mean_pop_losers
print(paste("Popularity difference:", round(pop_diff, 0), "points"))

# Danceability difference
mean_dance_winners <- mean(winners$rhythm.danceability, na.rm = TRUE)
mean_dance_losers <- mean(losers$rhythm.danceability, na.rm = TRUE)
dance_diff <- mean_dance_winners - mean_dance_losers
print(paste("Danceability difference:", round(dance_diff, 3)))

summary(best_grammy_model)





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




#Graphing results
nominees <- data.frame(
  song = c("Not Like Us", "TEXAS HOLD 'EM", "Espresso", "Brat", "Birds of a Feather", "Good Luck, Babe!", "Fortnight"),
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

# --- 3. Process the data for plotting ---
nominees_processed <- nominees %>%
  mutate(Is_Winner = ifelse(probability == max(probability), 'Yes', 'No')) %>%
  mutate(song = str_wrap(song, width = 20)) %>% # Keep this at 20
  mutate(Song_Label = ifelse(Is_Winner == 'Yes', paste('🏆', song), song)) %>%
  mutate(Song_Label = reorder(Song_Label, probability)) %>%
  mutate(Label_Color = ifelse(Is_Winner == 'Yes', 'black', 'white'))

# --- 4. Create the plot ---
grammy_plot_final <- ggplot(nominees_processed, aes(x = probability, y = Song_Label, fill = Is_Winner)) +
  geom_col() +
  geom_text(
    aes(label = percent(probability, accuracy = 1), color = Label_Color), 
    hjust = 1.2,
    size = 4,
    fontface = "bold"
  ) +
  scale_color_identity() + 
  scale_fill_manual(values = c("Yes" = "#FFD700", "No" = "grey"), guide = "none") +
  scale_x_continuous(
    limits = c(0, max(nominees_processed$probability) * 1.05),
    labels = percent_format(accuracy = 1) 
  ) +
  labs(
    title = "Predicting the 2025 Grammy Winner",
    subtitle = "Record of the Year Win Probabilities",
    x = "Predicted Probability of Winning",
    y = ""
  ) +
  
 
  theme_minimal(base_size = 12) + 
  
  theme(
    plot.title = element_text(face = "bold", size = 15), 
    plot.subtitle = element_text(size = 11), 
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )

# --- 5. Save and display the plot ---
ggsave(
  "upwork_thumbnail_final_fit.png", 
  plot = grammy_plot_final, 
  width = 8,
  
  
  height = 6.0,  
  
  dpi = 300,
  
  bg='white'
)

print(grammy_plot_final)











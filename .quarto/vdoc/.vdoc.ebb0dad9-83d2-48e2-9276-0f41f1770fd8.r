#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
movies <- read_csv("data/movies.csv")
#
#
#
#| cache: true
movies_clean <- movies |>
  separate_longer_delim(genre, delim = ",") |>
  mutate(genre = str_trim(genre))
#
#
#
movies |>
  ggplot(aes(x = tomatoMeter)) +
  geom_histogram(binwidth = 5, na.rm = TRUE) +
  labs(
    title = "Distribution of Tomatometer Scores",
    subtitle = paste(nrow(movies), "films"),
    x = "Tomatometer Score",
    y = "Count"
  )
#
#
#
movies_gap <- movies_clean |>
  filter(tomatoMeter != 0, audienceScore != 0) |>
  mutate(gap = tomatoMeter - audienceScore)

movies_summary <- movies_gap |>
  group_by(genre) |>
  summarise(
    mean_gap = mean(gap, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 100) |>
  arrange(mean_gap)
#
#
#
movies_summary |>
  ggplot(aes(x = reorder(genre, mean_gap), y = mean_gap)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(
    title = "Average Critic-Audience Score Gap by Genre",
    subtitle = "Genres with at least 100 films",
    x = "Genre",
    y = "Mean gap (Tomatometer minus audience score)"
  )
#
#
#
#

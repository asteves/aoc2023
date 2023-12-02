## Day 2 Puzzle 
library(dplyr)
library(tibble)
library(tidyr)

limits = tibble(
  limits = c(12,13,14),
  color = c("red", "green", "blue")
)

day2 = tibble::as.tibble(readLines("../data/day2.txt")) 

day2 |>
  separate(value, into = c("game", "colors"), sep = ":") |>
  separate_rows(colors, sep = ";") |>
  group_by(game) |>
  mutate(shows = row_number()) |>
  separate_rows(colors, sep = ",") |>
  mutate(colors = colors |> trimws()) |>
  separate(colors, into = c("count", "color"),
           sep = " ", convert = T) |>
  separate(game, into = c("game", "game_num"), 
           sep = " ", convert = T) |>
  left_join(limits) |>
  mutate(good = count <= limits) |>
  summarise(valid = all(good), .by = "game_num") |>
  filter(valid) |> 
  pull(game_num) |>
  sum()
  

## part 2
day2 |>
  separate(value, into = c("game", "colors"), sep = ":") |>
  separate_rows(colors, sep = ";") |>
  group_by(game) |>
  mutate(shows = row_number()) |>
  separate_rows(colors, sep = ",") |>
  mutate(colors = colors |> trimws()) |>
  separate(colors, into = c("count", "color"),
           sep = " ", convert = T) |>
  separate(game, into = c("game", "game_num"), 
           sep = " ", convert = T) |>
  summarise(valid_max = max(count), .by = c("game_num", "color")) |>
  summarise(power = prod(valid_max),.by = game_num)|>
  pull(power) |> 
  sum()

#-------------------------------------------------------------------------------
#' day 22: Monkey Map
#-------------------------------------------------------------------------------

df_22 <- tibble(lines = readLines('inputs/day22.txt'))


# board dimensions --------------------------------------------------------

map_22 <- df_22 |> 
  mutate(
    row = row_number()
  ) |> 
  filter(row < n() - 1) |> 
  mutate(
    board = str_split(lines, pattern = '')
  ) |> 
  unnest_longer(board) |> 
  group_by(row) |> 
  mutate(col = row_number()) |> 
  ungroup() |> 
  filter(board != ' ') |> 
  select(-lines) |> 
  print()

# plot board --------------------------------------------------------------

map_22 |> 
  ggplot(aes(x = col, y = row, label = board)) +
  geom_text() +
  scale_y_reverse() +
  labs(
    x = NULL, y = NULL,
    title = 'Monkey map'
  ) +
  theme_aoc_null() +
  coord_fixed()

# get path directions -----------------------------------------------------

path <- df_22 |> 
  filter(row_number() == n()) |>
  mutate(
    nums = str_split(lines, pattern = 'R|L'),
    direction = str_split(lines, pattern = '[:digit:]+'),
    direction = map(direction, ~head(.x, -1)),
  ) |> 
  unnest_longer(c(direction, nums)) |> 
  mutate(turn = row_number()) |> 
  select(-lines) |> 
  print()


# starting ----------------------------------------------------------------

current_coords <- map_22 |> 
  filter(row == 1) |> 
  filter(col == min(col)) |> 
  select(row, col) |> 
  as.list()

current_dir <- 'e'

markers <- c('n' = '^', 'e' = '>', 'w' = '<', 's' = 'v')

# loop --------------------------------------------------------------------

# get the column or row
steps <- if(current_dir == 'e') {
  filter(map_22, row == current_coords$row)
} else if(current_dir  == 'w') {
  filter(map_22, row == current_coords$row) |> 
    arrange(-col)
} else if(current_dir  == 's') {
  filter(map_22, col == current_coords$col)
} else { # current_dir  == 'n'
  filter(map_22, col == current_coords$col) |> 
    arrange(-row)
}

# get the length of this for the wrapping

map_length <- nrow(steps)

# get the marker to use

marker <- markers[current_dir]

# inner while loop for each step that
# - checks the move is possible
# - increments the coordinates
# - drops a marker
# - sets the next direction

# i <- 1


# inner loop --------------------------------------------------------------


for(i in map_length) {
  
  step <- steps |> 
    filter(row_number() == i) |> 
    as.list()
  
  if(step$board == '#') {break}  
  board <- marker

# end inner loop ----------------------------------------------------------
}

(((1:20) - 1) %% 7) + 1


#-------------------------------------------------------------------------------
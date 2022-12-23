#-------------------------------------------------------------------------------
#' day 23: Unstable Diffusion
#-------------------------------------------------------------------------------

df_23 <- tibble(lines = readLines('inputs/day23.txt')) |> 
  mutate(
  row = row_number()
) |> 
  mutate(
    board = str_split(lines, pattern = '')
  ) |> 
  unnest_longer(board) |> 
  group_by(row) |> 
  mutate(col = row_number()) |> 
  ungroup() |> 
  filter(board != '.') |> 
  select(-lines) |> 
  print()

# plot board --------------------------------------------------------------

df_23 |> 
  ggplot(aes(x = col, y = row, label = board)) +
  geom_text() +
  scale_y_reverse() +
  labs(
    x = NULL, y = NULL,
    title = 'Planting map'
  ) +
  theme_aoc_null() +
  coord_fixed()



# initial settings --------------------------------------------------------

look_order <- c('n', 's', 'w', 'e')

elf_positions <- df_23 |> 
  pull(row, col)


num_loops <- 10

# loop --------------------------------------------------------------------

for (i in num_loops) {

  # find move
  
  # check clashes
  
  # make move
  
  # reorder the look order
  look_order = look_order[2:4, 1]
  
  
}


# empty ground tiles ------------------------------------------------------



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#' day 8: Treetop Tree House part 2
#-------------------------------------------------------------------------------


# read in the file --------------------------------------------------------

raw_8 <- readLines('inputs/day8-part1.csv')


# dataset for part 1 ------------------------------------------------------

visible <- tibble(trees = raw_8) |> 
  mutate(
    tree = str_split(trees, ""),
    row = row_number()
  ) |> 
  group_by(row) |> 
  unnest(tree) |> 
  #filter(row == 1) |> 
  mutate(
    tree = as.numeric(tree),
    column = row_number(),
    row_down = cummax(tree),
    visible = if_else(
      row_number() == 1,
      TRUE,
      tree > lag(row_down)
    ),
    row_up = order_by(-column, cummax(tree)),
    visible = if_else(
      visible | row_number() == n(),
      TRUE,
      tree > lead(row_up)
    )
  ) |> 
  group_by(column) |> 
  mutate(
    row_right = cummax(tree),
    visible = if_else(
      visible | row_number() == 1,
      TRUE,
      tree > lag(row_right)
    ),
    row_left = order_by(-row, cummax(tree)),
    visible = if_else(
      visible | row_number() == n(),
      TRUE,
      tree > lead(row_left)
    ),
  ) |> 
  ungroup()

# function for each direction ---------------------------------------------


look_count <- function(df) {

df |> 
  filter(row_number() != 1) |> 
  mutate(
    cummax_tree = pmax(z, cummax(tree)),
    taller = tree >= cummax_tree,
    after_taller = coalesce(lag(cummax(taller)), 0)
  ) |> 
    
  filter(! after_taller)  |> 
  nrow()
  
}  

# function to repeat ------------------------------------------------------

look_directions <- function(x, y) {

left <-  visible |> 
  select(tree, row, column) |> 
  mutate( z = sum((row == y & column == x) * tree)) |> 
  filter(row == y) |> 
  filter(column <= x) |> 
  arrange(-column) |> 
  look_count()

right <-  visible |> 
  select(tree, row, column) |> 
  mutate( z = sum((row == y & column == x) * tree)) |> 
  filter(row == y) |> 
  filter(column >= x) |> 
  look_count() 

up <- visible |> 
  select(tree, row, column) |> 
  mutate( z = sum((row == y & column == x) * tree)) |> 
  filter(column == x) |> 
  filter(row <= y) |> 
  arrange(-row) |> 
  look_count()

down <- visible |> 
  select(tree, row, column) |> 
  mutate( z = sum((row == y & column == x) * tree)) |> 
  filter(column == x) |> 
  filter(row >= y) |> 
  look_count() 

scenic_score <- left * right * up * down

}

# run for each tree -------------------------------------------------------

scenic <- visible |> 
  select(tree, row, column) |> 
  mutate(
    scenic_score = map2(column, row, look_directions)
  ) |> 
  unnest_longer(scenic_score) 


# get maximum -------------------------------------------------------------

max_scenic <- scenic |> 
  filter(scenic_score == max(scenic_score)) |> 
  slice(1) |> 
  pull(scenic_score)

# plot scenic scores ------------------------------------------------------

scenic |> 
  ggplot(aes(column, -row, fill = scenic_score, label = tree)) + 
  geom_tile() +
  scale_fill_continuous(low = aoc_yellow, high = aoc_dgreen) +
  theme_aoc_null() +
  labs(
    title = 'Scenic score of trees'
  ) +
  guides(fill = 'none')

# increment stars ---------------------------------------------------------

stars <- stars + 1

#-------------------------------------------------------------------------------

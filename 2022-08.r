# day 8: Treetop Tree House

raw_8 <- readLines('inputs/day8-part1-eg.csv')

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





# part 2

#visible_eg <- visible


#visible <- visible_eg

t <- visible |> 
   select(1:4) |> 
   filter(row == 4) |> 
   pull(tree)

see_direction <- function(t) {
  n <- length(t)
  see <- accumulate(t, ~ c(.x, .y), .dir = 'backward')
  lower <- unlist(map(1:n, ~sum(cummax(see[[.x]][-1]) <= t[[.x]])))
}

t <- visible_eg |> 
  select(1:4) |> 
  filter(row == 4) |> 
  arrange(-column) |> 
  pull(tree)


t
see <- accumulate(t, ~ c(.x, .y), .dir = 'backward')
see[[3]]
cummax(see[[3]])
lag(cummax(see[[3]]))
lag(cummax(see[[3]])) <= cummax(see[[3]])
sum(lag(cummax(see[[3]])) <= cummax(see[[3]]), na.rm = TRUE)


t <- visible_eg |> 
  select(1:4) |> 
  filter(column == 3) |> 
  arrange(-row) |> 
  pull(tree)


t
see <- accumulate(t, ~ c(.x, .y), .dir = 'backward')
see[[2]]
cummax(see[[2]])
lag(cummax(see[[2]]))
lag(cummax(see[[2]])) <= cummax(see[[2]])
sum(lag(cummax(see[[2]])) <= see[[2]], na.rm = TRUE)


scenic <- visible |> 
  select(1:4) |> 
  group_by(row) |> 
  mutate(
    see_up = see_direction(tree),
    see_down = order_by(-column, see_direction(tree))
  ) |> 
  group_by(column) |> 
  mutate(
    see_right = see_direction(tree),
    see_left = order_by(-row, see_direction(tree)),
    scenic_score = see_up + see_down + see_right + see_left
  ) |> 
  ungroup()

max(scenic$scenic_score)

# 196 is too low



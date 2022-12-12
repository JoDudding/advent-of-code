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



see_direction <- function(t) {
  n <- length(t)
  see <- accumulate(t, ~ c(.x, .y), .dir = 'backward')
  ex_me <- map(1:n, ~see[[.x]][-1])
  lower <- map(1:n, ~coalesce(sum(t[[.x]] > cummax(ex_me[[.x]]), 0)))
  first_higher <- map(1:n, ~coalesce(max(t[[.x]] <= cummax(ex_me[[.x]]),0)))
  unlist(map(1:n, ~lower[[.x]] + first_higher[[.x]]))
}

.x <- 1
see[[.x]]
ex_me[[.x]]
lower[[.x]]
first_higher[[.x]]


trow <- 1
tcol <- 2

t <- visible |> 
   select(1:4) |> 
   #filter(row == trow) |> 
   filter(row == tcol) |> 
   pull(tree)

  t
  t[[tcol]]
see <- accumulate(t, ~ c(.x, .y), .dir = 'backward')
  see
  see[[tcol]]
ex_me <- map(1:n, ~see[[.x]][-1])
  ex_me
  ex_me[[tcol]]
  
lower <-  sum(t[[tcol]] > cummax(ex_me[[tcol]]))
first_higher <- max(t[[tcol]] <= cummax(ex_me[[tcol]]))
lower + first_higher  




cmax <- map(1:n, ~cummax(ex_me[[.x]]))
  cmax
  cmax[[tcol]]
lower <- map(1:n, ~(cmax[[.x]]))


  see[[tcol]] # left/down
cummax(see[[tcol]])
lag(cummax(see[[3]]))
lag(cummax(see[[3]])) <= cummax(see[[3]])
sum(lag(cummax(see[[3]])) <= cummax(see[[3]]), na.rm = TRUE)





t <- visible_eg |> 
  select(1:4) |> 
  filter(row == 4) |> 
  arrange(-column) |> 
  pull(tree)





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
    scenic_score = see_up * see_down * see_right * see_left
  ) |> 
  ungroup()


scenic |> 
  #filter(row == 4 & column == 3) #- up okay, down wrong, left okay, right wrong
  filter(row == 4 & column == 4) #- up wrong (3 not 1), down okay (1), left okay, right wrong
  filter(
    (row == 2 & column == 3) |
    (row == 4 & column == 3)
  )


highest_scenic <- scenic |> 
  filter(scenic_score == max(scenic_score))

max(highest_scenic$scenic_score)



v <- visible |> 
  ggplot(aes(column, -row, fill =  tree, label = tree)) + 
  geom_tile() +
  geom_text(colour = aoc_black) +
  scale_fill_gradient(high = aoc_dgreen, low = aoc_yellow) +
  coord_fixed() +
  theme_aoc_null() +
  labs(
    title = 'Tree heights'
  ) +
  guides(fill = 'none')

s <- scenic |> 
  ggplot(aes(column, -row, fill = scenic_score, label = scenic_score)) + 
  geom_tile() +
  geom_text(colour = aoc_black) +
  scale_fill_gradient(high = aoc_dgreen, low = aoc_yellow) +
  coord_fixed() +
  theme_aoc_null() +
  labs(
    title = 'Scenic scores for trees'
  ) +
  guides(fill = 'none')

v + s


# 196 is too low

#-------------------------------------------------------------------------------

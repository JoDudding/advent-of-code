#-------------------------------------------------------------------------------
#' code for day 19 part 1
#-------------------------------------------------------------------------------

minutes <- 24

df_19 <- tibble(lines = readLines('inputs/day19.txt')) |> 
  separate(lines, sep = ': ', into = c('blueprint', 'details')) |> 
  separate(
    details, 
    sep = '\\. ', 
    into = c('ore', 'clay', 'obsidian', 'geode')
  ) |>
  gather(-blueprint, key = 'robot', value = 'cost') |> 
  mutate(
    cost = str_sub(cost, str_length(robot) + 19) |> 
      str_remove('\\.') |> 
      str_split(' and ')
  ) |> 
  unnest_longer(cost) |> 
  separate(cost, sep = ' ', into = c('cost_num', 'cost_type'), convert = TRUE) |> 
  mutate(
    robot = factor(robot, levels = c('ore', 'clay', 'obsidian', 'geode')),
    cost_type = factor(cost_type, levels = c('ore', 'clay', 'obsidian', 'geode'))) |> 
  arrange(blueprint, robot, cost_type) |> 
  spread(key = cost_type, value = cost_num, fill = 0)

b <- unique(df_19$blueprint)[1]

bank_type <- list(
  ore = 0,
  clay = 0,
  obsidian = 0,
  geode = 0)

bank_robot <- list(
  ore_robot = 1,
  clay_robot = 0,
  obsidian_robot = 0
)

spend_collect <- function(b) {
  
}

df_19 |> 
  filter(blueprint == b) |> 
  mutate(
    cum_ore = cumsum(ore)
  )


##  Each ##


#stars <- stars + 0

#-------------------------------------------------------------------------------

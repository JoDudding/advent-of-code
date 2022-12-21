#-------------------------------------------------------------------------------
#' day 15 Beacon Exclusion Zone
#-------------------------------------------------------------------------------

df_15 <- tibble(lines = readLines('inputs/day15-full.txt')) |> 
  separate(lines, sep = ':', into = c('sensor', 'beacon')) |> 
  separate(sensor, sep = ',', into = c('sensor_x', 'sensor_y')) |> 
  separate(beacon, sep = ',', into = c('beacon_x', 'beacon_y')) |> 
  mutate_all(parse_number) |> 
  mutate(
    pair = row_number(),
    manhattan = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y)
  )

row_of_interest <- if(nrow(df_15) == 14) {10} else {2000000}

row_of_interest_beacons <- df_15 |>
  filter(beacon_y == row_of_interest) |>
  distinct(beacon_x) |>
  nrow()



drop_overlap <- function(x) {
  
}


df_15 |>
  filter(abs(sensor_y - row_of_interest) <= manhattan) |>
  mutate(
    y_dist = abs(sensor_y - row_of_interest),
    x_dist = manhattan - y_dist,
    min_x = sensor_x - x_dist,
    max_x = sensor_x + x_dist,
    seq_x = map2(min_x, max_x, seq)
  ) |>
  select(min_x, max_x) |>
  arrange(min_x) |>
  mutate(
    min_x_adj = if_else(
      #row_number() != 1 | 
      min_x > lag(max_x),
      as.double(NA),
      min_x
    ),
    max_x_adj = if_else(
      #row_number() != 1 | 
      max_x > lead(min_x),
      as.double(NA),
      max_x
    )
  )
  
  
  
  
  select(min_x, max_x) |>
  mutate(
    adj_min = case_when(
      row_number() == 1 ~ min_x,
      min_x < lag(min_x) ~ min_x,
      min_x > lag(max_x) ~ min_x,
      TRUE ~ lag(min_x)
    ),
    adj_max = case_when(
      row_number() == 1 ~ max_x,
      max_x > lag(max_x) ~ max_x,
      max_x < lag(min_x) ~ max_x,
      TRUE ~ lag(max_x)
    )
  )

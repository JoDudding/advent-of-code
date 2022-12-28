#-------------------------------------------------------------------------------
#' day 25: Full of Hot Air
#-------------------------------------------------------------------------------


#        1              1
#        2              2
#        3             1=
#        4             1-
#        5             10
#        6             11
#        7             12
#        8             2=
#        9             2-
#       10             20
#       15            1=0
#       20            1-0
#     2022         1=11-2
#    12345        1-0---0
#314159265  1121-1110-1=0


# read in file ------------------------------------------------------------

df_25 <- tibble(lines = readLines('inputs/day25-full.txt')) |> 
  mutate(
    row = row_number(),
    code = str_split(lines, pattern = '')
  ) |> 
  group_by(row) |> 
  unnest(code) |> 
  mutate(power5 = n() - row_number()) |> 
  select(-lines) |> 
  ungroup() 


# mapping of code to decimal ----------------------------------------------

code5 <- tibble(
  code = c('=', '-', '0', '1', '2'),
  normal = c(-2, -1,0, 1, 2)
)


# convert to decimal number -----------------------------------------------

converted <- df_25 |> 
  inner_join(code5, by = 'code') |> 
  mutate(
    powered = 5 ^ power5,
    converted = powered * normal
  ) |> 
  group_by(row) |> 
  summarise(converted = sum(converted)) |> 
  ungroup() |> 
  summarise(converted = sum(converted)) |> 
  pull(converted)


# find the number of powers of 5 ------------------------------------------

num_inter <- floor(log(converted, 5))
val <- converted
track_it <- c()

for(i in num_inter:0) {
  
  start <- val
  power <- 5^i
  val_plus <- val + (power/2)
  step <- val_plus %/% power
  val <- val - (step * power)
  code <- code5 |> 
    filter(normal == step) |> 
    pull(code)

  track_it <- c(track_it, code)
}

paste(track_it, collapse = '')



# back to the code --------------------------------------------------------



# 2=-1=0

#-------------------------------------------------------------------------------
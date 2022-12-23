#-------------------------------------------------------------------------------
#' day 20 Grove Positioning System
#-------------------------------------------------------------------------------

df_20 <- tibble(lines = readLines('inputs/day20.txt')) |> 
  transmute(
    move = as.numeric(lines),
    orig_order = as.double(row_number()),
    current_order = as.double(orig_order)
  )

last_index <- nrow(df_20)
loop <- 1:(last_index)

zeroth <- df_20 |> 
  filter(move == 0) |> 
  pull(orig_order)


# wrap modulus function ---------------------------------------------------


mod_shift <- function(num, base) {
  ((num - 1) %% base) + 1
}


# loop function -----------------------------------------------------------


make_change <- function(l) {
  
l_move <<- df_20 |> 
  filter(orig_order == l) |> 
  pull(move)

from_position <<- after_loop |>
  filter(orig_order == l) |> 
  pull(current_order)

raw_shift <- if((from_position + l_move) > last_index) {
  from_position + l_move + 1
} else if( l_move > 0 & (from_position + l_move) > 0) {
  from_position + l_move
} else {
  from_position + l_move - 1
}

to_position <- mod_shift(raw_shift,  last_index)

#paste(l, l_move, from_position, to_position, sep = ' ~ ') |> 
#  print()

after_loop <<- after_loop |>
    mutate(
      old_order = as.double(current_order),
      move_position = from_position,
      new_position = to_position,
      current_order = case_when(
        l_move == 0 ~ old_order, 
        
        old_order == move_position ~ new_position,
        
        move_position < new_position &
          move_position <= old_order &
          old_order <= new_position ~ 
          mod_shift(old_order - 1, last_index),
        
        move_position > new_position &
          new_position <= old_order &
          old_order <= move_position ~ 
          mod_shift(old_order + 1, last_index),
        
        TRUE ~ old_order
      )
    ) 

}


# prepare for loop --------------------------------------------------------

after_loop <- df_20 |> 
  group_by(orig_order)  


# run loops ---------------------------------------------------------------

walk(loop, make_change)

#walk(loop[1], make_change)
#after_loop |> 
#  arrange(current_order) |> 
#  pull(move) |> 
#  paste( collapse = ', ')


# get selected values -----------------------------------------------------


values_wanted <- c(0, 1000, 2000, 3000) %% last_index

resort_zero <- after_loop |> 
  ungroup() |> 
  mutate(
    zero_position = sum((orig_order == zeroth) * current_order),
    final_position = (current_order - zero_position) %% last_index
  ) 

df_20_sum <- resort_zero |> 
  filter(final_position %in% values_wanted) |> 
  summarise(final = sum(move)) |> 
  pull(final)

as.character(df_20_sum)

# 589 is too low
# 3082 is too low
# -2515 is too low

#-------------------------------------------------------------------------------

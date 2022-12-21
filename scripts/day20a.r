#-------------------------------------------------------------------------------
#' day 20 Grove Positioning System
#-------------------------------------------------------------------------------

df_20 <- tibble(lines = readLines('inputs/day20-full.txt')) |> 
  transmute(
    move = as.numeric(lines),
    orig_order = row_number() - 1,
    current_order = orig_order
  )

last_index <- nrow(df_20)
loop <- 1:(last_index)

zeroth <- df_20 |> 
  filter(move == 0) |> 
  pull(orig_order)


#' To mix the file, move each number forward or backward in the file a number of 
#' positions equal to the value of the number being moved. The list is circular, 
#' so moving a number off one end of the list wraps back around to the other end 
#' as if the ends were connected.

make_change <- function(l) {
  
l_move <<- df_20 |> 
  filter(orig_order == l - 1) |> 
  pull(move)
  
after_loop <<- after_loop |>
    mutate(
      old_order = current_order,
      move_position = sum((orig_order == (l - 1)) * current_order),
      move_to = case_when(
        move_position + l_move <= 0 ~ ((move_position + l_move - 1) %% last_index),
        move_position + l_move > last_index ~ ((move_position + l_move + 1) %% last_index),
        TRUE ~ move_position + l_move
      ),
      current_order = case_when(
        current_order == move_position ~ move_to,
        move_to <= move_position & 
          current_order >= move_to & 
          current_order <= move_position ~
          (current_order + 1)  %% last_index,
        move_to >= move_position  & 
          current_order >= move_position & 
          current_order <= move_to ~
          (current_order - 1)  %% last_index,
        TRUE ~ current_order
      )
    )

}

after_loop <- df_20
walk(loop, make_change)
paste(after_loop$move, collapse = ', ')

values_wanted <- c(0, 1000, 2000, 3000) %% last_index

resort_zero <- after_loop |> 
  mutate(
    zero_position = sum((orig_order == zeroth) * current_order),
    final_position = (current_order - zero_position) %% last_index
  ) 

df_20_sum <- resort_zero |> 
  filter(final_position %in% values_wanted) |> 
  summarise(final = sum(move)) |> 
  pull(final)

df_20_sum

# 589 is too low
# 3082 is too low
# -2515 is too low

#-------------------------------------------------------------------------------

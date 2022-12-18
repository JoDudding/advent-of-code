#-------------------------------------------------------------------------------
#' code for day 18 part 1
#-------------------------------------------------------------------------------


df_18 <- tibble(lines = readLines('inputs/day18-full.txt')) |> 
  separate(lines, sep = ',', into = c('x', 'y', 'z'), convert = TRUE) 


xy <- df_18 |> 
      inner_join(df_18, by = c('x', 'y')) |> 
      filter(abs(z.x - z.y) == 1)

xz <- df_18 |> 
  inner_join(df_18, by = c('x', 'z')) |> 
  filter(abs(y.x - y.y) == 1)

yz <- df_18 |> 
  inner_join(df_18, by = c('y', 'z')) |> 
  filter(abs(x.x - x.y) == 1)

(nrow(df_18) * 6) - nrow(xy) - nrow(xz) - nrow(yz)

stars <- stars + 1

#-------------------------------------------------------------------------------

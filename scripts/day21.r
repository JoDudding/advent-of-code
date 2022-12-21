#-------------------------------------------------------------------------------
#' day 21: Monkey Math
#-------------------------------------------------------------------------------

df_21 <- tibble(lines = readLines('inputs/day21.txt')) |> 
  separate(lines, into = c('monkey', 'equation'), sep = ': ') |> 
  mutate(
    type = case_when(
      str_count(equation, '\\s') == 0 ~ 'Number',
      TRUE ~ 'Monkeys'
    )
  ) |> 
  arrange(type)

monkey_numbers <- df_21 |> 
  filter(type == 'Number') |> 
  transmute(monkey_name = monkey, number = parse_number(equation))

still_equation <- df_21 |> 
  filter(type == 'Monkeys') |> 
  separate(equation, sep = '\\s', into = c('monkey_1', 'equation', 'monkey_2')) |> 
  gather(
    c(monkey_1, monkey_2),
    key = order_monkey,
    value = monkey_name
  ) |> 
  arrange(monkey, order_monkey) |> 
  mutate(number_last = as.double(NA))


# start while loop
while(nrow(still_equation) > 0) {

# see if all child monkeys have been replaced by a number
  
working <- still_equation |> 
  left_join(monkey_numbers, by = 'monkey_name') |> 
  group_by(monkey) |> 
  mutate(
    number = coalesce(number, number_last),
    fully = sum(is.na(number)) == 0
  ) |> 
  ungroup()

# add the newly calculated monkey numbers back
monkey_numbers <- working |> 
  filter(fully) |> 
  group_by(monkey)  |> 
  mutate(
    tot_number = case_when(
      equation == '+' ~ first(number) + last(number),
      equation == '-' ~ first(number) - last(number),
      equation == '*' ~ first(number) * last(number),
      equation == '/' ~ first(number) / last(number)
    )
  ) |> 
  ungroup() |> 
  distinct(monkey_name  = monkey, number = tot_number) |> 
  bind_rows(monkey_numbers)

# remove these monkeys from the list
still_equation <- working |> 
  filter(!fully) |> 
  mutate(number_last = number) |> 
  select(-number)

# end while
}

# find the number for the root monkey
root_number <- monkey_numbers |> 
  filter(monkey_name == 'root') |> 
  pull(number)

comma(root_number)

stars <- stars + 1

#-------------------------------------------------------------------------------
#' part 2
#' need to stop for now
#' for monkeys with numbers flag how many loops were required
#' loop one less step
#' flag for whether humn has been used in the path
#' looks like its only referenced once
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
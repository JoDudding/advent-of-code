#-------------------------------------------------------------------------------
#' code for day 1 part 1
#-------------------------------------------------------------------------------

df1 <- tibble(cals = readLines('inputs/day1-part1.csv')) |>
  mutate(
    cals = as.numeric(cals),
    row = row_number(),
    elf = case_when(
      row == 1 ~ row,
      is.na(cals) ~ row
    )
  ) |>
  fill(elf) |>
  filter(!is.na(cals)) |>
  group_by(elf) |>
  summarise(cals = sum(cals)) |>
  ungroup() |>
  arrange(-cals)

stars <- stars + 1

#-------------------------------------------------------------------------------
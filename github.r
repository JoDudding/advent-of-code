#-------------------------------------------------------------------------------
#' managing github credentials
#' https://usethis.r-lib.org/articles/git-credentials.html
#-------------------------------------------------------------------------------

library(tidyverse)

key <- readr::read_csv('github.csv') |> 
  pull(key) |> 
  print()


gitcreds::gitcreds_set()

#usethis::git_sitrep()

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#' managing github credentials
#' https://usethis.r-lib.org/articles/git-credentials.html
#-------------------------------------------------------------------------------

key <- readr::read_csv('github.csv') |> 
  pull(key)


gitcreds::gitcreds_set()

#-------------------------------------------------------------------------------

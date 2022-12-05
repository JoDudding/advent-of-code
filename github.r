#-------------------------------------------------------------------------------
#' managing github credentials
#' https://usethis.r-lib.org/articles/git-credentials.html
#-------------------------------------------------------------------------------

key <- readr::read_csv('github.csv') |> 
  pull(key)


gitcreds::gitcreds_set()

usethis::gh_token_help()

usethis::create_github_token()

usethis::git_sitrep()

#-------------------------------------------------------------------------------

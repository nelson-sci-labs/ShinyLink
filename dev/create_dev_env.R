# Create develop environment for simple match and manual inspection
library(magrittr)

dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
dfA$birthday <- as.character(dfA$birthday)
dfB <- readxl::read_excel('inst/app/www/redcapoutput_cleaned.xlsx')
dfB$birthday <- as.character(dfB$birthday)
matches.out <- fastLink::fastLink(
  dfA = dfA, dfB = dfB,
  # varnames = c("firstname", "middlename", "lastname"),
  # stringdist.match = c("firstname", "middlename", "lastname"),
  varnames = c("firstname", "middlename", "lastname", "birthday", "race", "sex"),
  stringdist.match = c("firstname", "middlename", "lastname", "birthday", "race", "sex"),
  # numeric.match =
  partial.match = c("firstname", "middlename", "lastname", "birthday", "race", "sex"),
  n.cores = 1
)

matched_dfs <- fastLink::getMatches(
  dfA = dfA,
  dfB = dfB,
  fl.out = matches.out,
  threshold.match = 0.85
)

matched_dfs <- tibble::as_tibble(matched_dfs)

colnames(matched_dfs)[grep("gamma", colnames(matched_dfs))] <-
  paste0("match_status_", c("firstname", "middlename", "lastname", "birthday", "race", "sex"))

colnames(matched_dfs)[colnames(matched_dfs) == "posterior"] <- "posterior_score"

name_matching_fields <- colnames(matched_dfs)[grep("match_status_", colnames(matched_dfs))]
matched_dfs <- matched_dfs %>% dplyr::mutate(sum_match = rowSums(dplyr::across(name_matching_fields), na.rm=TRUE))

matched_dfs <- matched_dfs %>% dplyr::mutate(manual_selection = NA)

matched_dfs <- set_match_levels(matched_dfs)

matched_dfs$inds.a <- matches.out$matches$inds.a
matched_dfs$inds.b <- matches.out$matches$inds.b



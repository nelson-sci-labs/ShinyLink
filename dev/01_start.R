# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "ShinyLink", # The Name of the package containing the App
  pkg_title = "RShiny Based Record Linkage Tool", # The Title of the package containing the App
  pkg_description = "ShinyLink is the bridge between existing vigorous open-source record linkage algorithms and an urgently needed accessible platform that eliminates cost and programming barriers and delivers a public health and bioinformatics precedent toward increased data interchangeability.", # The Description of the package containing the App
  author_first_name = "Yaoxiang", # Your First Name
  author_last_name = "Li", # Your Last Name
  author_email = "liyaoxiang@outlook.com", # Your Email
  author_orcid = "0000-0001-9200-1016",
  repo_url = NULL # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license("Nelson Scientific Labs, LLC") # You can set another license here
usethis::use_readme_rmd(open = FALSE)
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
usethis::use_code_of_conduct(contact = "Nelson Scientific Labs, LLC")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)

## Adding dependencies
usethis::use_package("dplyr", min_version = TRUE, type = "Suggests")

usethis::use_package("DT", min_version = TRUE)
usethis::use_package("fastLink", min_version = TRUE)
usethis::use_package("gender", min_version = TRUE)
usethis::use_package("ggplot2", min_version = TRUE)
usethis::use_package("ggvenn", min_version = TRUE)
usethis::use_package("ggpubr")

usethis::use_package("haven", min_version = TRUE)
usethis::use_package("htmlwidgets", min_version = TRUE)
usethis::use_package("jsonlite", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("magrittr", min_version = TRUE)

usethis::use_package("purrr", min_version = TRUE, type = "Suggests")

usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("shiny")
usethis::use_package("shinyjs")

usethis::use_package("shinybusy")

usethis::use_package("shinydashboard", min_version = TRUE)
usethis::use_package("shinydashboardPlus", min_version = TRUE)
usethis::use_package("shinyWidgets", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)

usethis::use_package("vroom", min_version = TRUE)


usethis::use_pipe(export = TRUE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

golem::use_recommended_deps()
golem::use_recommended_deps()
## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon(path = "inst/app/www/favicon.ico") # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")

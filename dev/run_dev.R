# Sass code compilation
# sass::sass(input = sass::sass_file("inst/app/www/custom.sass"), output = "inst/app/www/custom.css", cache = NULL)

# Set options here
options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()

# https://fontawesome.com/v4/icons/


# TODO New features and bug fixes
# [x] - 01. Able to download the uncertainty table in manual review module.
# [x] - 02. Allow all upper/lower case.
# [x] - 03. Add save and load settings for variable assignment.
# [x] - 04. Add save and load settings for gender/race re-coding.
# [x] - 05. Do we need finding duplicates within a data set? No
# [x] - 06. Add functionality to download all results together.
# [ ] - 07. Question for CDC, do they need blocking? What is the maximum size of data set that they have in their workflow.
# [x] - 08. Add calculation of levels of uncertainty module.
# [x] - 09. Add manual inspection module, allow users to decide level of uncertainty before review matches.
# [x] - 10. Fix the issue that different data input format generates different matching results


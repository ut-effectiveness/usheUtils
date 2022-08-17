
# This file should be used to keep track of your development throughout the project.


# package dependencies ####
usethis::use_package("stringr")


# Add Fake Data Sets ####
# Adding fake data sets to the package (in the form it would come as from a data warehouse)

## Student #####
usethis::use_data_raw(name = "fake_student_df", open = FALSE)
# ACTION: Code to generate this data frame will need to be written and ran to save the data in the package.

## Course ####
usethis::use_data_raw(name = "fake_course_df", open = FALSE)
# ACTION: Code to generate this data frame will need to be written and ran to save the data in the package.

## Student Course ####
usethis::use_data_raw(name = "fake_student_course_df", open = FALSE)
# ACTION: Code to generate this data frame will need to be written and ran to save the data in the package.

## Program ####
usethis::use_data_raw(name = "fake_program_df", open = FALSE)
# ACTION: Code to generate this data frame will need to be written and ran to save the data in the package.

## Mission ####
usethis::use_data_raw(name = "fake_mission_df", open = FALSE)
# ACTION: Code to generate this data frame will need to be written and ran to save the data in the package.

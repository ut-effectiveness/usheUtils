## code to prepare `fake_program_df` dataset goes here

sample_size <- 10000

fake_program_df <- data.frame(
  major_desc = sample(c("", NA), sample_size, replace = TRUE),
  required_credits = sample(c("", NA), sample_size, replace = TRUE),
  is_perkins = sample(c(TRUE, FALSE, NA), sample_size, replace = TRUE),
  degree_id = sample(c("", NA), sample_size, replace = TRUE),
  ipeds_award_level_code = sample(c("", NA), sample_size, replace = TRUE),
  cip_code = sample(c("", NA), sample_size, replace = TRUE),
  academic_year = sample(c("", NA), sample_size, replace = TRUE)
)

usethis::use_data(fake_program_df, overwrite = TRUE)

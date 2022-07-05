
#' Append Column S14 (Ethnic Origin)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element S14 appended.
#' @export
#'
#' @examples
#' append_column_S14()
#'
append_column_S14 <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    mutate(H = case_when( is_hispanic_latino_ethnicity ~ "H",
                          TRUE ~ " "),
           A = case_when(is_asian ~ "A",
                          TRUE ~ " "),
           B = case_when(is_black ~ "B",
                          TRUE ~ " "),
           I = case_when(is_american_indian_alaskan ~ "I",
                          TRUE ~ " "),
           P = case_when(is_hawaiian_pacific_islander ~ "P",
                          TRUE ~ " "),
           W = case_when(is_white ~ "W",
                          TRUE ~ " "),
           N = case_when(is_international ~ "N",
                          TRUE ~ " "),
           U = case_when(is_other_race ~ "U",
                          TRUE ~ " "))

  output_df$"S14" = paste0(output_df$H, output_df$A, output_df$B,
                           output_df$I, output_df$P, output_df$W,
                           output_df$N, output_df$U)

  return(output_df)

}

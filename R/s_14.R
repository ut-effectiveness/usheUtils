
#' Append Column S14 (Ethnic Origin)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_14 appended.
#' @export
#'
#' @examples
#' s_14()
#'
s_14 <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    mutate(H = case_when(is_hispanic_latino_ethnicity ~ "H",
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
                          TRUE ~ " ")) %>%
    mutate( s_14=paste0(H, A, B, I, P, W, N, U) ) %>%
    # Remove data fields used for intermediate calculations
    select( -c(H, A, B, I, P, W, N, U) )

  return(output_df)

}


#' Append Column s_02 (Year, Term, & Extract)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_02 appended.
#' @export
#'
#' @examples
#' s_02()
#'
s_02 <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    # TODO: Implement code to generate element s_02 here.
    mutate(s_02="")

  return(output_df)
}

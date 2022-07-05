
#' Append Column s_01 (Institution)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_01 appended.
#' @export
#'
#' @examples
#' s_01()
#'
s_01 <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    mutate(s_01="3671")

  return(output_df)
}

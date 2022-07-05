
#' Append Column s_12 (Birth Date)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_12 appended.
#' @export
#'
#' @examples
#' s_12()
#'
s_12 <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    mutate( s_12 = gsub("-", "", birth_date) )

  return(output_df)

}

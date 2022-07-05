
#' Append Column S01 (Institution)
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element S01 appended.
#' @export
#'
#' @examples
#' append_column_S01()
#'
append_column_S01 <- function(input_df=fake_student_df) {
  output_df <- input_df

  output_df$"S01" <- "3671"

  return(output_df)
}

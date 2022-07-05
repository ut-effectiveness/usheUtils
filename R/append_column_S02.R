
#' Append Column S02 (Year, Term, & Extract)
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element S02 appended.
#' @export
#'
#' @examples
#' append_column_S02()
#'
append_column_S02 <- function(input_df=fake_student_df) {
  output_df <- input_df

  # TODO: Implement code to generate element S02 here.
  output_df$"S02" <- ""

  return(output_df)
}

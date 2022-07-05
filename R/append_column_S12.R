
#' Append Column S12 (Birth Date)
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element S14 appended.
#' @export
#'
#' @examples
#' append_column_S12()
#'
append_column_S12 <- function(input_df=fake_student_df) {

  output_df <- input_df

  output_df$"S12" <- gsub("-", "", output_df$birth_date)

  return(output_df)

}

#' Clean Data Frame
#'
#' Performs various operations over each value in data frame.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#'
#' @param input_df A Data Frame.
#'
#' @return Original data frame, with cleaning operations applied.
#' @export
#'
#' @examples
#' clean()
#'

clean <- function(input_df=usheUtils::fake_student_df) {

  # Remove special characters
  input_df[] <- lapply(input_df, function(x) { str_replace_all(x, "[,.;:-?]", "") } )
  # Replace & characters with and
  input_df[] <- lapply(input_df,  function(x) { str_replace_all(x, "&", "and") } )

  return(input_df)
}

#' Calculate USHE Element s_50 (Degree Intent of Second Degree)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Degree Intent of Second Degree
#' - FIELD NAME: S_DEG_INTENT2
#' - FIELD FORMAT: Varchar, 2 Character
#' - DEFINITION: The length/nature of the degree that the student is working toward.
#'               S_DEG_INTENT2 with S_CURR_CIP2 represents the student’s major and ties to S_MAJOR2
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_ipeds_award_level_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_50 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_50()
#'
s_50 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_deg_intent = secondary_ipeds_award_level_code) %>%
    # Append USHE data element s_50
    mutate( s_50 = s_deg_intent )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_deg_intent) )
  }

  return(output_df)
}

#' Calculate USHE Element d_06 ( Description)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: E Description
#' - FIELD NAME:  Description
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: Institutions may use this field according to their preference or to differentiate students if submitting multiple files.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (program_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element d_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' d_06()
#'
d_06 <- function(input_df=usheUtils::fake_dws_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( description = program_id ) %>%
    # Append USHE data element d_06
    mutate( d_06 = description)

  return(output_df)

  }

#' Check for valid SSN characters ()
#'
#' @details
#'
#' Is valid ssn id
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ssn).
#'
#'
#' @return A Boolean TRUE or FALSE if SSN is valid.
#' @export
#'
#' @examples
#' is_valid_ssn()
#'
is_valid_ssn <- function() {

ssn_regex <- "^[0-8][0-9][0-9]-[0-9][0-9]-[0-9][0-9][0-9][0-9]$"

  valid_ssn_check = (str_detect(ssn, ssn_regex) )

  return(output_df)
}


zipcode_regex <- "^[0-9]{5}(-[0-9]{4})?$"

output_df <- input_df %>%

  # Calculate intermediate fields
  mutate(s_curr_zip = coalesce(if_else(str_detect(local_address_zip_code, zipcode_regex),
                                       local_address_zip_code, NA_character_ ),
                               if_else(str_detect(mailing_address_zip_code, zipcode_regex),
                                       mailing_address_zip_code, NA_character_ ))) %>%

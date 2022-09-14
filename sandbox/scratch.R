
#' Calculate USHE Element g_08 (Date of Graduation)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Date of Graduation
#' - FIELD NAME: gen_ushe_grad_date, g_date & d_start_dt
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD Format)
#' - DEFINITION: The calendar date the formal award was conferred by the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (graduation_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_08 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' gen_ushe_grad_date()
#'
gen_ushe_grad_date <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(date = gsub("-", "", graduation_date) ) %>%
    # Append USHE data element gen_ushe_grad_date
    mutate( gen_ushe_grad_date = date,
            g_24 = date,
            d_start_dt = date,
            d_04 = date )

  return(output_df)
}

#' @rdname gen_ushe_grad_date
#' @examples g_08()
#' @export
g_24 <- gen_ushe_grad_date

#' @rdname gen_ushe_grad_date
#' @examples d_04()
#' @export
d_04 <- gen_ushe_grad_date

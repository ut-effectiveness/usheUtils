#' Calculate USHE Element (Degree Type)
#'
#' @details
#'
#' **USHE Documentation**
#' ELEMENT NAME: Degree Type
#' FIELD NAME: gen_ushe_deg_type, pf_deg_type & g_deg_type
#' FIELD FORMAT: Varchar, 6 Characters
#' DEFINITION: The Level of Degree or Certificate. Refer to the  Degree Type Table for all degrees.

#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (degree_id).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_deg_type appended.
#' @export
#'
#' @examples
#' gen_ushe_deg_type()
#'
gen_ushe_deg_type <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( deg_type = if_else( degree_id == "MMFT",
                                   "MMF",
                                   degree_id ) ) %>%
    # Append USHE data element gen_ushe_deg_type
    mutate( gen_ushe_deg_type = deg_type,
            pf_05 = deg_type,
            g_10 = deg_type)

  return(output_df)
}

#' @rdname gen_ushe_deg_type
#' @examples pf_05()
#' @export
pf_05 <- gen_ushe_deg_type

#' @rdname gen_ushe_deg_type
#' @examples g_10()
#' @export
g_10 <- gen_ushe_deg_type

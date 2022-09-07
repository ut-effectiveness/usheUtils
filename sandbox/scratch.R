#' Calculate USHE Element (Birth Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Date of Birth
#' - FIELD NAME: S_BIRTH_DT, M_BIRTH_DT, & G_BIRTH_DT
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD),
#' - DEFINITION: The calendar student date of birth, as designated by student.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (birth_date).
#'
#'
#' @return Original data frame, with USHE data elements gen_ushe_birth_date and m_03 appended.
#' @export
#'
#' @examples
#' gen_ushe_birth_date()
#'
gen_ushe_birth_date <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( birth_dt = gsub("-", "", birth_date) ) %>%
    # Append USHE data element gen_ushe_birth_date
    mutate( gen_ushe_birth_date = birth_dt,
            s_12 = birth_dt,
            m_03 = birth_dt,
            g_05 = birth_dt)

  return(output_df)
}

#' @rdname gen_ushe_birth_date
#' @examples s_12()
#' @export
s_12 <- gen_ushe_birth_date


#' @rdname gen_ushe_birth_date
#' @examples m_03()
#' @export
m_03 <- gen_ushe_birth_date

#' @rdname gen_ushe_birth_date
#' @examples g_05()
#' @export
g_05 <- gen_ushe_birth_date

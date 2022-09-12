#' Calculate USHE Element Institution (Institution)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institution
#' - FIELD NAME: C_INST, S_INST, G_INST, SC_INST, M_INST, F_INST, R_INST, B_INST, & PF_INST
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The identification number used by the U.S. Department of Education’s Office of Postsecondary Education (OPE) to identify schools that have Program Participation Agreements (PPA) so that its students are eligible to participate in Federal Student Financial Assistance programs under Title IV regulations.
#'               This is a 6-digit number followed by a 2-digit suffix used to identify branches,  additional locations, and other entities that are part of the eligible institution.
#'               USHE will use the last 4 digits of the 6-digit number for the field value as additional locations will be specified by C-09 Line Item.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (no data fields required).
#'
#'
#' @return Original data frame, with USHE data element s_01 appended.
#' @export
#'
#' @examples
#' gen_ushe_inst()
#'
gen_ushe_inst <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_inst <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( institution = "3671") %>%
    # Append USHE data element gen_ushe_inst
    mutate( gen_ushe_inst = institution,
            s_01 = institution,
            c_01 = institution,
            sc_01 = institution,
            pf_01 = institutiont,
            m_01 = institutiont,
            b_01 = institution,
            r_01 = institution,
            g_01 = institution )

  return(output_df)
}

#' @rdname gen_ushe_inst
#' @examples s_01()
#' @export
s_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples c_01()
#' @export
c_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples sc_01()
#' @export
sc_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples pf_01()
#' @export
pf_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples m_01()
#' @export
m_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples b_01()
#' @export
b_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples r_01()
#' @export
r_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples g_01()
#' @export
g_01 <- gen_ushe_inst

#' Calculate USHE Elements for Names (Student Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name, name_suffix).
#'
#'
#' @return Original data frame, with USHE data element s_06 appended.
#' @export
#'
#' @examples
#' gen_ushe_name()
#'
gen_ushe_name <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( last = coalesce(last_name, ''),
            first = coalesce(first_name, ''),
            middle = coalesce(middle_name, ''),
            suffix = coalesce(name_suffix, '') ) %>%
    # Append USHE data element gen_ushe_name
    mutate( name = paste(last, first, middle, suffix, sep = "|")) %>%
    mutate(gen_ushe_name = name,
           s_06 = name,
           g_03 = name)

  return(output_df)
}

#' @rdname gen_ushe_name
#' @examples s_06()
#' @export
s_06 <- gen_ushe_name

#' @rdname gen_ushe_name
#' @examples g_03()
#' @export
g_03 <- gen_ushe_name

#' Calculate USHE Element Ethnicity (Ethnic Origin)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Ethnicity
#' - FIELD NAME: s_ethnic, g_ethnic
#' - FIELD FORMAT: Varchar, 8 Characters,
#' - DEFINITION: The racial and ethnic categories used to classify students.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_hispanic_latino_ethnicity, is_asian, is_black, is_american_indian_alaskan, is_hawaiian_pacific_islander, is_white, is_international, is_other_race).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_ethnicty appended.
#' @export
#'
#' @examples
#' gen_ushe_ethnicty()
#'
gen_ushe_ethnicty <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(ethnic_h = if_else(is_hispanic_latino_ethnicity, "H", ""),
           ethnic_a = if_else(is_asian, "A", ""),
           ethnic_b = if_else(is_black, "B", ""),
           ethnic_i = if_else(is_american_indian_alaskan, "I", ""),
           ethnic_p = if_else(is_hawaiian_pacific_islander, "P", ""),
           ethnic_w = if_else(is_white, "W", ""),
           ethnic_n = if_else(is_international, "N", ""),
           ethnic_u = if_else(is_other_race, "U", "") ) %>%
    mutate( ethnicty_intermediate = paste(ethnic_h, ethnic_a, ethnic_b, ethnic_i, ethnic_p, ethnic_w, ethnic_n, ethnic_u, sep= "|" )) %>%
    # Append USHE data element gen_ushe_ethnicty
    mutate( ethnicty = if_else(is_international, paste("", "", "", "", "", "", ethnic_n, "", sep = "|"), ethnicty_intermediate) ) %>%
    mutate(gen_ushe_ethnicty = ethnicty,
           s_14 = ethnicty,
           g_07 = ethnicty )

  return(output_df)
}

#' @rdname gen_ushe_ethnicty
#' @examples s_14()
#' @export
s_14 <- gen_ushe_ethnicty

#' @rdname gen_ushe_ethnicty
#' @examples g_07()
#' @export
g_07 <- gen_ushe_ethnicty

#' Calculate USHE Element Utah County Code (Utah County Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Utah County Code
#' - FIELD NAME: S_COUNTY_ORIGIN, and G_COUNTY_ORIGIN,
#' - FIELD FORMAT: Varchar, 5 Characters,
#' - DEFINITION: The Utah county code indicating the student’s county of origin as described at the time of first application to the institution for enrollment and if the S_STATE_ORIGIN is UT.
#'               Enter UT030 if  county is Unknown.
#'               Enter UT097 if student is Out of State, Out of US.
#'               Enter UT099 if student is Out  of State, In the US.
#'               This element should be logically consistent with S-11: S_STATE_ORIGIN and S 27: S_COUNTRY_ORIGIN.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_county_code, first_admit_state_code, first_admit_country_iso_code).
#'
#'
#' @return Original data frame, with USHE data element s_10 appended.
#' @export
#'
#' @examples
#' gen_ushe_county_origin()
#'
gen_ushe_county_origin <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(county_origin = case_when(
      ( first_admit_state_code == "UT" & !is.na(first_admit_county_code) ) ~ paste0("UT", first_admit_county_code),
      first_admit_country_iso_code == "US" ~ "UT099",
      first_admit_country_iso_code != "US" ~ "UT097",
      TRUE ~ "UT030")) %>%
    # Append USHE data element gen_ushe_county_origin
    mutate( gen_ushe_county_origin = county_origin,
            s_10 = county_origin,
            g_04 = county_origin)

  return(output_df)
}

#' @rdname gen_ushe_county_origin
#' @examples s_10()
#' @export
s_10 <- gen_ushe_county_origin

#' @rdname gen_ushe_county_origin
#' @examples g_04()
#' @export
g_04 <- gen_ushe_county_origin

#' Calculate USHE Element Social Security Number or Institutionally Assigned ID (Student ID)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institutionally Assigned ID
#' - FIELD NAME: s_id, sc_id, and g_id
#' - FIELD FORMAT: Varchar, 9 Characters
#' - DEFINITION: The unique institutionally assigned identification number for each student intended to be used in lieu of using a student’s social security number.
#'               G_Banner_ID must begin with an Alpha character representing the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_starts
#' @importFrom stringr str_length
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id, ssn).
#'
#'
#' @return Original data frame, with USHE data element s_03 appended.
#' @export
#'
#' @examples
#' gen_ushe_id()
#'
gen_ushe_id <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    # Note: ssn that start with 9 are actually IRS issued tax IDs; USHE only wants valid ssn.
    mutate( id_intermediate = str_replace_all(ssn, "-", "")) %>%
    mutate( id = if_else(is.na(s_id_intermediate) |
                           str_starts(id_intermediate, "9") |
                           str_length(id_intermediate) >= 10 |
                           str_length(id_intermediate) <= 8,
                         paste0('D', student_id),
                         id_intermediate ) )  %>%

    # Append USHE data element gen_ushe_id
    mutate( gen_ushe_id = id,
            s_03 = id,
            sc_03 = id,
            g_02 = id )

  return(output_df)
}

#' @rdname gen_ushe_id
#' @examples sc_03()
#' @export
sc_03 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples sc_03()
#' @export
sc_03 <- gen_ushe_id

#' @rdname gen_ushe_id
#' @examples g_02()
#' @export
g_02 <- gen_ushe_id

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

#' Calculate USHE Element ushe_banner_id (Institution Assigned ID)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institutionally Assigned ID
#' - FIELD NAME: gen_ushe_banner_id, s_banner_id, m_banner_id & g_banner_id
#' - FIELD FORMAT: Varchar, 5 Characters,
#' - DEFINITION: The unique institutionally assigned identification number for each student.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id).
#'
#'
#' @return Original data frame, with USHE data element s_35, m_06, g_21 appended.
#' @export
#'
#' @examples
#' gen_ushe_banner_id()
#'
gen_ushe_banner_id <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( banner_id = paste0('D', student_id )) %>%
    # Append USHE data element gen_ushe_banner_id
    mutate( gen_ushe_banner_id = banner_id,
            s_35 = banner_id,
            m_06 = banner_id,
            g_21 = banner_id)

  return(output_df)
}

#' @rdname gen_ushe_banner_id
#' @examples s_35()
#' @export
s_35 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples m_06()
#' @export
m_06 <- gen_ushe_banner_id

#' @rdname gen_ushe_banner_id
#' @examples g_21()
#' @export
g_21 <- gen_ushe_banner_id

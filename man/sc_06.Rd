% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/students_courses.R
\name{sc_06}
\alias{sc_06}
\title{Calculate USHE Element sc_06 (Course Section)}
\usage{
sc_06(input_df = usheUtils::fake_student_course_df, with_intermediates = FALSE)
}
\arguments{
\item{input_df}{A Data Frame. Must contain the following data fields: (section_number).}

\item{with_intermediates}{Boolean: Option to include intermediate calculated fields.}
}
\value{
Original data frame, with USHE data element sc_06 appended. Will also return appended intermediate calculated fields, if option is set.
}
\description{
Calculate USHE Element sc_06 (Course Section)
}
\details{
\strong{USHE Documentation}
}
\examples{
sc_06()

}
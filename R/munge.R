# LIBRARIES ####
library(here)

# DATA IMPORT ####
source(here::here('R', 'data_io_util.R'))


s_14 <- get_data_from_sql_file('s_14.sql', 'edify')

c_17 <- get_data_from_sql_file('c_17.sql', 'edify')

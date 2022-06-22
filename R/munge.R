# LIBRARIES ####
library(here)

# DATA IMPORT ####
source(here::here('R', 'data_io_util.R'))


s_14 <- get_data_from_sql_file('s_14.sql', 'edify')

c_17 <- get_data_from_sql_file('c_17.sql', 'edify')

c_02 <- get_data_from_sql_file('c_02.sql', 'edify')

c_10 <- get_data_from_sql_file('c_10.sql', 'edify')

c_44  <- get_data_from_sql_file('c_44.sql', 'edify')

c_14  <- get_data_from_sql_file('c_14.sql', 'edify')

c_51  <- get_data_from_sql_file('c_51.sql', 'edify')

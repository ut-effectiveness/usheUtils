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

c_50  <- get_data_from_sql_file('c_50.sql', 'edify')

s_04  <- get_data_from_sql_file('s_04.sql', 'edify')

s_12  <- get_data_from_sql_file('s_12.sql', 'edify')

s_15  <- get_data_from_sql_file('s_15.sql', 'edify')

s_17  <- get_data_from_sql_file('s_17.sql', 'edify')

s_20  <- get_data_from_sql_file('s_20.sql', 'edify')

s_21  <- get_data_from_sql_file('s_21.sql', 'edify')

s_26  <- get_data_from_sql_file('s_26.sql', 'edify')

s_44  <- get_data_from_sql_file('s_44.sql', 'edify')

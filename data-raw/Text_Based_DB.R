# The first part of this script generates an empty SQL data base provided
# that you already have the right software installed. The second part
# sends the new data to it. Note that if you've already set up
# a data base and simply want to add new data to it, you will
# to change the SQL commands below to use something like
# INSERT instead of create

rm(list = ls())

library(dplyr)
library(stringr)
library(RPostgreSQL)

# create a connection
# In case you forget, your password for postgres is postgres
pw <- {
  "postgres"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Padrino",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)

# Generate new tables to store information in Postgres data base
create_metadata_table_command <- "CREATE TABLE metadata
(
  ipm_id character varying NOT NULL,
  species_author character varying NOT NULL,
  species_accepted character varying,
  tax_genus character varying NOT NULL,
  tax_family character varying,
  tax_order character varying,
  tax_class character varying,
  tax_phylum character varying,
  kingdom character varying,
  organism_type character varying,
  dicot_monocot character varying,
  angio_gymno character varying,
  authors character varying NOT NULL,
  journal character varying,
  pub_year integer NOT NULL,
  doi character varying,
  corresponding_author character varying,
  email_year character varying,
  remark character varying,
  apa_citation character varying,
  demog_appendix_link character varying,
  duration integer NOT NULL,
  start_year integer NOT NULL,
  start_month integer,
  end_year integer NOT NULL,
  end_month integer,
  periodicity decimal (7, 4) NOT NULL,
  number_populations integer NOT NULL,
  lat decimal (15, 12),
  lon decimal (15, 12),
  altitude decimal (7, 2),
  country character(3),
  continent character varying,
  ecoregion character(3),
  studied_sex character varying,
  eviction_used boolean,
  evict_type character varying,
  treatment character varying,
  CONSTRAINT metadata_pkey PRIMARY KEY (ipm_id)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE metadata
OWNER TO postgres;"

dbGetQuery(con, create_metadata_table_command)

create_model_expression_table_command <- "CREATE TABLE model_expression
(
  ipm_id character varying NOT NULL REFERENCES metadata,
  demographic_parameter character varying NOT NULL,
  model_formula character varying NOT NULL,
  model_type character varying NOT NULL,
  model_family character varying,
  CONSTRAINT model_expression_pkey PRIMARY KEY (ipm_id, demographic_parameter)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE model_expression
OWNER TO postgres;
"

dbGetQuery(con, create_model_expression_table_command)
  
create_model_values_table_command <- "CREATE TABLE model_values
(
  ipm_id character varying NOT NULL REFERENCES metadata,
  demographic_parameter character varying NOT NULL,
  state_variable character varying NOT NULL,
  parameter_type character varying NOT NULL,
  parameter_name character varying NOT NULL,
  parameter_value double precision NOT NULL,
  CONSTRAINT model_values_pkey PRIMARY KEY (ipm_id, demographic_parameter, parameter_name)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE model_values
OWNER TO postgres;
"

dbGetQuery(con, create_model_values_table_command)


create_model_statevar_table_command <- "CREATE TABLE state_variables
(
  ipm_id character varying NOT NULL REFERENCES metadata,
  state_variable character varying NOT NULL,
  lower real NOT NULL,
  upper real NOT NULL,
  n_meshpoints integer NOT NULL,
  discrete boolean NOT NULL,
  discrete_type character varying,
  CONSTRAINT state_variables_pkey PRIMARY KEY (ipm_id, state_variable)
)
WITH (
OIDS=FALSE
);
ALTER TABLE state_variables
OWNER TO postgres;
"
  
dbGetQuery(con, create_model_statevar_table_command)
  
## ---------------------------------------------------------- ##

# Create tables for pushing to SQL 
# First option - enter data in excel sheets, convert to csv, read
# into R and then prepare for sending to SQL. This seems like
# best idea to retain similarity with COM(P)ADRE workflows

Model <- list(Metadata = read.csv('metadata/Flat_Raw/metadata_SCL.csv',
                                  stringsAsFactors = FALSE) %>%
                filter(ipm_id != ''),
              StateVar = read.csv('metadata/Flat_Raw/state_vars_SCL.csv',
                                  stringsAsFactors = FALSE) %>%
                filter(ipm_id != ''),
              ModelExp = read.csv('metadata/Flat_Raw/model_exprs_SCL.csv',
                                  stringsAsFactors = FALSE) %>%
                filter(!is.na(ipm_id)),
              ModelVal = read.csv('metadata/Flat_Raw/model_values_SCL.csv',
                                  stringsAsFactors = FALSE) %>%
                filter(!is.na(ipm_id)))

dbWriteTable(conn = con,
             name = 'metadata',
             value = Model$Metadata,
             append = TRUE,
             row.names = FALSE)
dbWriteTable(conn = con,
             name = 'state_variables',
             value = Model$StateVar,
             append = TRUE,
             row.names = FALSE)
dbWriteTable(conn = con,
             name = 'model_expression',
             value = Model$ModelExp,
             append = TRUE,
             row.names = FALSE)
dbWriteTable(conn = con,
             name = 'model_values',
             value = Model$ModelVal,
             append = TRUE,
             row.names = FALSE)


  
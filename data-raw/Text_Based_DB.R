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
  start_month integer NOT NULL,
  end_year integer NOT NULL,
  end_month integer NOT NULL,
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
  n_meshpoints smallint NOT NULL,
  CONSTRAINT state_variables_pkey PRIMARY KEY (ipm_id, state_variable)
)
WITH (
OIDS=FALSE
);
ALTER TABLE state_variables
OWNER TO postgres;
"
  
dbGetQuery(con, create_model_statevar_table_command)
  
## NOTE: CODE BELOW IS NOT TESTED AND DOES NOT ACTUALLY WORK
## PROVIDED TO GIVE AN IDEA OF WHAT THE WORKFLOW WOULD 
## LOOK LIKE 

## ---------------------------------------------------------- ##

# Create tables for pushing to SQL 
# First option - enter data in excel sheets, convert to csv, read
# into R and then prepare for sending to SQL.

Model <- list(Metadata = read.csv('data-raw/metadata.csv',
                                  stringsAsFactors = FALSE),
              StateVar = read.csv('data-raw/state_vars.csv',
                                  stringsAsFactors = FALSE),
              ModelExp = read.csv('data-raw/model_exprs.csv',
                                  stringsAsFactors = FALSE),
              ModelVal = read.csv('data-raw/model_values.csv',
                                  stringsAsFactors = FALSE))

# OR - Create a sample model representing the expressions to build an
# IPM using a list in R. This is definitely more reproducible,
# but comes at the expense of requiring a bit more tech savvy. 
Model <- list(Parameters = list(Growth = list(Formula = "sizeNext ~ Normal(mu, sd)",
                                              mu = "2.751 + 0.407 * Size",
                                              sd = "sqrt(9.119 * exp(-0.228 * Size))",
                                              Model = c("GLM", "Gaussian")),
                                Survival = list(Formula = "logit(s_z) ~ SurvInt + SurvSlope * Size",
                                                SurvInt = -2.27,
                                                SurvSlope = 0.569,
                                                Model = c("GLM", "Logistic")),
                                Fecundity = list(FlowerP = list(Formula = "logit(f_z) ~ FlowerPInt +
                                                                                  FlowerPSlope * Size",
                                                                FlowerPInt = -2.107,
                                                                FlowerPSlope = 0.86,
                                                                Model = c("GLM", "Logistic")),
                                                 FlowerN = list(Formula = "FlowerN ~ FlowerNInt + 
                                                                                FlowerNSlope * exp(Size)",
                                                              FlowerNInt = 6.363,
                                                              FlowerNSlope = 0.0056,
                                                              Model = c("GLM", "Poisson")),
                                                 Seeds = list(Formula = "Constant",
                                                              Constant = 374,
                                                              Model = c("", "Constant")),
                                                 EstProb = list(Formula = "Constant",
                                                                Constant = "0.019",
                                                                Model = c("", "Constant")),
                                                 RecrSize = list(Formula = "SizeNext ~ Normal(mu, sd)",
                                                                 mu = -0.771,
                                                                 sd = 1.719,
                                                                 Model = c("PDensity", "Gaussian")),
                                                 EnterDiscrete = list(Formula = "Constant",
                                                                      Constant = 0.157,
                                                                      Model = c("", "Constant")),
                                                 StayDiscrete = list(Formula = "Constant",
                                                                     Constant = 0.038,
                                                                     Model = c("", "Constant")),
                                                 LeaveDiscrete = list(Formula = "Constant",
                                                                      Constant = 0.1847,
                                                                      Model = c("", "Constant")))),
              Metadata = data.frame(species_name = "Carduus_nutans",
                                    ipm_id = "A11111",
                                    author = c('Jongejans E; Shea K; Skarpas O; Kelly D; Ellner S'),
                                    doi = "10.1890/09-2226.1"))

# Eelke did not include the upper and lower size bounds of his populations (!!!!)
# but I do have the real boundaries from the ipmBase3_EJ.R script. Future
# Padrino digitizers will not have that luxury. Emailing the author is probably the best
# option here.
Metadata <- Model$Metadata
ModelExpressions <- data.frame(ipm_id = rep("A11111", 4),
                               demographic_parameter = c("Survival", "Growth", "PFlowering", "Seeds"),
                               model_formula = c(Model$Parameters$Survival$Formula,
                                                 Model$Parameters$Growth$Formula,
                                                 Model$Parameters$Fecundity$FlowerP$Formula,
                                                 Model$Parameters$Fecundity$Seeds$Formula),
                               model_type = c(Model$Parameters$Survival$Model[1],
                                              Model$Parameters$Growth$Model[1],
                                              Model$Parameters$Fecundity$FlowerP$Model[1],
                                              Model$Parameters$Fecundity$Seeds$Model[1]),
                               model_family = c(Model$Parameters$Survival$Model[2],
                                                Model$Parameters$Growth$Model[2],
                                                Model$Parameters$Fecundity$FlowerP$Model[2],
                                                Model$Parameters$Fecundity$Seeds$Model[2]))

FecLen <- lapply(Model$Parameters$Fecundity, FUN = function(x) x$Model[2] == 'Constant') %>%
  unlist %>%
  .[!.] %>%
  length() * 2
NotFecLen <- lapply(Model$Parameters$Fecundity, FUN = function(x) x$Model[2] == 'Constant') %>%
  unlist %>%
  .[. == TRUE] %>%
  length

TotalParms <- sum((length(Model$Parameters$Growth) - 2), 
                  (length(Model$Parameters$Survival) - 2),
                  FecLen, NotFecLen)
ModelValues <- data.frame(ipm_code = rep("A11111", TotalParms),
                          demographic_parameter = c(rep('Survival', 2),
                                                    rep('Growth', 2),
                                                    rep('Fecundity', 11 )),
                          state_variable = rep('Size', TotalParms),
                          parameter_type = c('Mean','Mean', # Survival
                                             'Mean', 'Mean', 'SD', # Growth
                                             ''),
                                             rep('Constant', NotFecLen))

dbWriteTable(conn = con,
             name = 'metadata',
             value = Metadata,
             append = TRUE,
             row.names = FALSE)
dbWriteTable(conn = con,
             name = 'model_expression',
             value = ModelExpressions,
             append = TRUE,
             row.names = FALSE)

  
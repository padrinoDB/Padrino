# Attempt to create an IPM out of expressions and coefficients
# Using Jongejans et al 2011 spatial IPM (while ignoring the
# spatial part for now)

# I'm going to create a list and then try writing it to an SQL data base that
# I set up locally. I'm not good at with SQL, but I can't really
# think of another way to generate a "flat" representation of all of this




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
  species_name character varying NOT NULL,
  author character varying NOT NULL,
  doi character varying NOT NULL,
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
  CONSTRAINT model_values_pkey PRIMARY KEY (ipm_id, demographic_parameter, parameter_type)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE model_values
OWNER TO postgres;
"

dbGetQuery(con, create_model_values_table_command)


# Create a sample model representing the expressions to build an
# IPM
Model <- list(Parameters = list(Growth = list(Formula = "sizeNext ~ Normal(mu, sd)",
                                              mu = quote(2.751 + 0.407 * Size),
                                              sd = quote(sqrt(9.119 * exp(-0.228 * Size))),
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
                                                 Seeds = list(Formula = "Seeds ~ SeedInt + 
                                                                                SeedSlope * exp(Size)",
                                                              SeedInt = 6.363,
                                                              SeedSlope = 0.0056,
                                                              Model = c("GLM", "Poisson")))),
              Metadata = data.frame(species_name = "Carduus_nutans",
                                    ipm_id = "A11111",
                                    author = c('Jongejans E; Shea K; Skarpas O; Kelly D; Ellner S'),
                                    doi = "10.1890/09-2226.1"))

# Create tables for pushing to SQL 
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

  
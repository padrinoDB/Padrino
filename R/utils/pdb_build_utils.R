# Utility functions to build PADRINO

as_numeric <- function(x) suppressWarnings(as.numeric(x))
as_integer <- function(x) suppressWarnings(as.integer(x))

set_pdb_col_types <- function(pdb) {
  
  # Metadata issues
  pdb$Metadata$lat         <- as_numeric(pdb$Metadata$lat)
  pdb$Metadata$lon         <- as_numeric(pdb$Metadata$lon)
  pdb$Metadata$altitude    <- as_numeric(pdb$Metadata$altitude)
  pdb$Metadata$pub_year    <- as_integer(pdb$Metadata$pub_year)
  pdb$Metadata$start_year  <- as_integer(pdb$Metadata$start_year)
  pdb$Metadata$start_month <- as_integer(pdb$Metadata$start_month)  
  pdb$Metadata$end_year    <- as_integer(pdb$Metadata$end_year)
  pdb$Metadata$end_month   <- as_integer(pdb$Metadata$end_month) 
  pdb$Metadata$periodicity <- as_numeric(pdb$Metadata$periodicity)

  return(pdb)
  
}

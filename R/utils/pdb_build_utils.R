# Utility functions to build PADRINO

library(stringi)

as_numeric <- function(x) suppressWarnings(as.numeric(x))
as_integer <- function(x) suppressWarnings(as.integer(x))

pdb_set_col_types <- function(pdb) {
  
  # Metadata issues - type the fields
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

pdb_correct_enc_issues <- function(pdb) {  
  
  # pdb$Metadata$species_author   <- stri_trans_general(pdb$Metadata$species_author,
  #                                                     "Any-ascii")
  # pdb$Metadata$species_accepted <- stri_trans_general(pdb$Metadata$species_accepted,
  #                                                     "Any-ascii")
  # pdb$Metadata$tax_genus        <- stri_trans_general(pdb$Metadata$tax_genus,
  #                                                     "Any-ascii")
  # pdb$Metadata$tax_family       <- stri_trans_general(pdb$Metadata$tax_family,
  #                                                     "Any-ascii")
  # pdb$Metadata$tax_order        <- stri_trans_general(pdb$Metadata$tax_order,
  #                                                     "Any-ascii")
  # pdb$Metadata$tax_class        <- stri_trans_general(pdb$Metadata$tax_class,
  #                                                     "Any-ascii")
  # pdb$Metadata$tax_phylum       <- stri_trans_general(pdb$Metadata$tax_phylum,
  #                                                     "Any-ascii")
  # pdb$Metadata$corresponding_author <- stri_trans_general(pdb$Metadata$corresponding_author,
  #                                                         "Any-ascii")
  
  
  return(pdb)
}

pdb_correct_spp_names <- function(pdb) {
  
  pdb$Metadata$species_author   <- gsub("\\.", "", pdb$Metadata$species_author)
  pdb$Metadata$species_author   <- gsub(" ", "_", pdb$Metadata$species_author)
  pdb$Metadata$species_accepted <- gsub("\\.", "", pdb$Metadata$species_accepted)
  pdb$Metadata$species_accepted <- gsub(" ", "_", pdb$Metadata$species_accepted)
  
  return(pdb)
}

pdb_correct_data_cases <- function(pdb) {
  
  pdb$Metadata$dicot_monocot <- tolower(pdb$Metadata$dicot_monocot)
  pdb$Metadata$continent     <- tolower(pdb$Metadata$continent)
  pdb$Metadata$organism_type <- tolower(pdb$Metadata$organism_type)
  
  
  pdb$Metadata$email_year[pdb$Metadata$email_year == "na"] <- NA_character_
  pdb$Metadata$doi[pdb$Metadata$doi == "na"]               <- NA_character_
  pdb$Metadata$demog_appendix_link[pdb$Metadata$demog_appendix_link == "na"] <- NA_character_
  
  pdb$Metadata$duration[pdb$Metadata$duration %in% c("na", "N/A")] <- NA_integer_
  pdb$Metadata$number_populations[pdb$Metadata$number_populations == "N/A"] <- NA_integer_
  pdb$Metadata$country[pdb$Metadata$country == "N/A"] <- NA_character_
  pdb$Metadata$treatment <- stri_trans_totitle(pdb$Metadata$treatment)
  
  return(pdb)
}

pdb_correct_common_metadata_issues <- function(pdb) {
  
  pdb <- pdb_set_col_types(pdb)
  pdb <- pdb_correct_enc_issues(pdb)
  pdb <- pdb_correct_spp_names(pdb)
  pdb <- pdb_correct_data_cases(pdb)
  
  return(pdb)
}

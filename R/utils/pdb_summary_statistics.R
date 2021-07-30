# summary stats

library(dplyr)
library(RPadrino)

# pdb <- pdb_download(save = FALSE)
name_ind <- c("Metadata", 
              "StateVariables",
              "DiscreteStates", 
              "ContinuousDomains", 
              "IntegrationRules", 
              "StateVectors",
              "IpmKernels",
              "VitalRateExpr", 
              "ParameterValues", 
              "EnvironmentalVariables",
              "HierarchTable", 
              "UncertaintyTable", 
              "TestTargets")

rd_nms <- paste('padrino-database/clean/', name_ind, ".csv", sep = "")

pdb <- lapply(rd_nms, function(x) {
  read.csv(x,
           stringsAsFactors = FALSE,
           fileEncoding = "UTF-8")
})

names(pdb) <- name_ind
class(pdb) <- c("pdb", "list")


tab <- pdb$Metadata %>% 
  group_by(kingdom) %>%
  summarise(
    IPMs = length(unique(ipm_id)),
    spp  = length(unique(species_accepted)),
    pap  = length(unique(apa_citation))
  )
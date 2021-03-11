# uses test_that to see which models are ready for production, and which ones
# need editing.

library(fs)
library(dplyr)
library(pander)
library(lubridate)
library(RPadrino)

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

rd_nms <- paste('padrino-database/raw/', name_ind, ".csv", sep = "")

pdb <- lapply(rd_nms, function(x) {
  read.csv(x,
           stringsAsFactors = FALSE,
           fileEncoding = "UTF-8")
})

names(pdb) <- name_ind
class(pdb) <- c("pdb", "list")

int_rules <- c("midpoint")

rm_ind <- pdb$IntegrationRules$ipm_id[!pdb$IntegrationRules$integration_rule %in% int_rules]

# These are models that use GAMs. Haven't worked out how to include those yet.

rm_ind <- c(rm_ind, "aaa337", "aaa338")

pdb    <- lapply(pdb,
                 function(x, ind) {
                   x[!x$ipm_id %in% ind, ]
                 },
                 ind = rm_ind)


out <- list()
errs <- data.frame(
  ipm_id = pdb$Metadata$ipm_id,
  error  = NA_character_
)

out_it <- 1

for(i in seq_along(pdb$Metadata$ipm_id)) {
  
  use_id <- pdb$Metadata$ipm_id[i]
  
  # evals contains restarts that alow us to capture both warnings and errors
  # from a single expression. This isn't possible w/ tryCatch, as it does
  # a first-in first-out with condition handling
  
  temp <- evals("pdb_make_proto_ipm(pdb, ipm_id = use_id, det_stoch = 'det')")
  
  if(!is.null(temp[[1]]$msg$errors) || 
     !is.null(temp[[1]]$msg$warnings)) {
    
    msgs <- paste("Warnings found: ", temp[[1]]$msg$warnings, 
                 "Errors found: ", temp[[1]]$msg$errors,
                 sep = "; ")
    
    errs$error[i] <- msgs
    
  }
  
  # Some models have innocuous warnings (e.g. assuming all kernels are built w
  # the same int_rule). We can still store those results and try to make_ipm with
  # them
  
  if(!is.null(temp[[1]]$result)) {
    
    out[[out_it]]      <- temp[[1]]$result
    names(out)[out_it] <- use_id
    out_it             <- out_it + 1
    
  } 
  
} 

errs <- errs[!is.na(errs$error), ]

write.csv(errs, "metadata/build_artefacts/failing_models.csv", row.names = FALSE)

# Now, check models for which we can build a proto_ipm to see whether we can build
# an actual IPM object. After that, we'll check against the TestTargets table.
# For now, lambda is the only metric available. However, I'm going to add others
# for models where that isn't an option

good_mods <- names(out)

good_db <- lapply(pdb, 
                  function(x, use_ids) x[x$ipm_id %in% use_ids, ],
                  use_ids = good_mods)

test_cases <- pdb$TestTargets

completed_mods <- list()
incomplete_mods <- list()

target_missed <- data.frame(
  ipm_id          = good_db$Metadata$ipm_id,
  error           = NA_character_,
  error_magnitude = NA_real_,
  error_perc      = NA_real_
)

no_target <- data.frame(
  ipm_id = good_db$Metadata$ipm_id,
  lambda = NA_real_
)

out_it <- 1
inc_it <- 1

for(i in seq_along(good_db$Metadata$ipm_id)) {
  
  use_id <- good_db$Metadata$ipm_id[i]
  
  test_proto <- pdb_make_proto_ipm(pdb, ipm_id = use_id)
  
  append_args <- rlang::list2(!!use_id := list(iterate = TRUE,
                                               iterations = 100))
  
  test_ipm <- evals("pdb_make_ipm(test_proto, addl_args = append_args)")
  
  
  if(!is.null(test_ipm[[1]]$msg$errors) || 
     !is.null(test_ipm[[1]]$msg$warnings)) {
    
    msgs <- paste("Warnings found: ", test_ipm[[1]]$msg$warnings, 
                 "Errors found: ", test_ipm[[1]]$msg$errors,
                 sep = "; ")
    
    target_missed$error[i] <- msgs
    
  } else {
    
    test_ipm <- test_ipm[[1]]$result
    
    if(!any(is.na(test_ipm[[1]]$pop_state))){
      
      test_lam <- lambda(test_ipm[[1]], type_lambda = "last")
      
    } else {
      
      test_lam <- lambda(test_ipm[[1]], comp_method = "eigen")
      
      test_lam <- unname(test_lam)
    }
    
    if(use_id %in% test_cases$ipm_id) {
      
      target_prec <- test_cases$precision[test_cases$ipm_id == use_id]
      
      test_tol    <- 10^(-target_prec)
      
      test_lam <- round(test_lam, digits = target_prec)
      
      target_lam  <- test_cases$target_value[test_cases$ipm_id == use_id]
      
      # Test for rounding error OR lambda within 1% of the reported value
      res <- isTRUE(all.equal(test_lam, target_lam, tolerance = test_tol)) ||
        (abs((test_lam - target_lam) / target_lam) < 0.01)
      
      if(res) {
        
        completed_mods[[out_it]]      <- test_ipm
        names(completed_mods)[out_it] <- use_id
        out_it                        <- out_it + 1
        
      } else {
        
        target_missed$error_magnitude[i] <- test_lam - target_lam
        target_missed$error_perc[i]      <- (test_lam - target_lam) / target_lam
        incomplete_mods[[inc_it]]        <- test_ipm
        names(incomplete_mods)[inc_it]   <- use_id
        inc_it                           <- inc_it + 1
        
      }
    } else {
      
      no_target$lambda[no_target$ipm_id == use_id] <- paste(test_lam, collapse = ", ")
      
      completed_mods[[out_it]]       <- test_ipm
      names(completed_mods)[out_it] <- use_id
      out_it                        <- out_it + 1
      
    }
  }
  
}

target_missed <- target_missed[!is.na(target_missed$error) | 
                                 !is.na(target_missed$error_magnitude), ]
add_tests     <- no_target[!is.na(no_target$lambda), ]

write.csv(target_missed, "metadata/unreliable_models.csv", row.names = FALSE)
write.csv(add_tests, "metadata/test_targets_needed.csv", row.names = FALSE)

# Get the models that hit their targets, and drop out the ones where authors
# have requested an embargo period

prod_db_ids <- names(completed_mods)
embargo_ids <- good_db$Metadata$ipm_id[!good_db$Metadata$.embargo]

use_ids     <- c(prod_db_ids, embargo_ids)

prod_db     <- pdb_subset(good_db, ipm_ids = use_ids)

prod_db$Metadata <- prod_db$Metadata %>%
  select(-c(.embargo, .embargo_date))

for(i in seq_along(prod_db)) {
  
  nm <- names(prod_db)[i]
  
  write.csv(prod_db[[i]], 
            file         = paste("padrino-database/clean/", nm,".csv", sep = ""),
            row.names    = FALSE,
            quote        = TRUE,
            na           = "NA",
            fileEncoding = "UTF-8")
  
}

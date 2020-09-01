# uses test_that to see which models are ready for production, and which ones
# need editing.

library(fs)
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


out <- list()
errs <- data.frame(
  ipm_id = pdb$Metadata$ipm_id,
  error  = NA_character_
)

out_it <- 1

for(i in seq_along(pdb$Metadata$ipm_id)) {
  
  use_id <- pdb$Metadata$ipm_id[i]
  
  temp <- tryCatch({
    pdb_make_proto_ipm(pdb, ipm_id = use_id, det_stoch = "det")
  },
  error   = function(x) x,
  warning = function(w) w)
  
  if(inherits(temp, "simpleError") ||
     inherits(temp, "rlang_error")) {
    
    errs$error[i] <- temp$message
    
  } else {
    
    out[[out_it]]      <- temp
    names(out)[out_it] <- use_id
    out_it             <- out_it + 1
    
  }
  
} 

write.csv(errs, "metadata/failing_models.csv", row.names = FALSE)

good_mods <- names(out)

good_db <- lapply(pdb, 
                  function(x, use_ids) x[x$ipm_id %in% use_ids, ],
                  use_ids = good_mods)

test_cases <- pdb$TestTargets

completed_mods <- list()

target_missed <- data.frame(
  ipm_id          = good_db$Metadata$ipm_id,
  error           = NA_character_,
  error_magnitude = NA_real_
)

out_it <- 1

for(i in seq_along(good_db$Metadata$ipm_id)) {
  
  use_id <- good_db$Metadata$ipm_id[i]
  
  if(!use_id %in% test_cases$ipm_id) next
  
  test_proto <- pdb_make_proto_ipm(pdb, ipm_id = use_id)
  
  append_args <- rlang::list2(!!use_id := list(iterate = TRUE,
                                               iterations = 100))
  
  test_ipm   <- tryCatch({
    pdb_make_ipm(test_proto, addl_args = append_args)
  },
  error   = function(x) x,
  warning = function(w) w)
  
  if(inherits(test_ipm, "simpleError") ||
     inherits(test_ipm, "rlang_error")) {
    
    target_missed$error[i] <- test_ipm$message
    
  } else {
    
    if(!is.na(test_ipm[[1]]$pop_state)){
      
      test_lam <- lambda(test_ipm[[1]], type_lambda = "last")
      
    } else {
      
      test_lam <- lambda(test_ipm[[1]], comp_method = "eigen")
      
      test_lam <- unname(test_lam)
    }
    
    target_prec <- test_cases$precision[test_cases$ipm_id == use_id]
    
    test_tol    <- 10^(-target_prec)
    
    test_lam <- round(test_lam, digits = (target_prec - 1))
    
    target_lam  <- test_cases$target_value[test_cases$ipm_id == use_id]
    
    res <- isTRUE(all.equal(test_lam, target_lam, tolerance = test_tol))
    
    if(res) {
      
      completed_mods[[out_it]]      <- test_ipm
      names(completed_mods)[out_it] <- use_id
      out_it                        <- out_it + 1
      
    } else {
      
      target_missed$error_magnitude[i] <- test_lam - target_lam
      
    }
  }
  
}

target_missed <- target_missed[!is.na(target_missed$error) | 
                                 !is.na(target_missed$error_magnitude), ]

write.csv(target_missed, "metadata/unreliable_models.csv", row.names = FALSE)

prod_db_ids <- names(completed_mods)

prod_db     <- lapply(good_db, 
                      function(x, ids) {
                        x[x$ipm_id %in% ids, ]
                      },
                      ids = prod_db_ids)

for(i in seq_along(prod_db)) {
  
  nm <- names(prod_db)[i]
  
  write.csv(prod_db[[i]], 
            file         = paste("padrino-database/clean/", nm,".csv", sep = ""),
            row.names    = FALSE,
            quote        = TRUE,
            na           = "NA",
            fileEncoding = "UTF-8")
  
}

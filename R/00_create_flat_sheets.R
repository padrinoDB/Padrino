library(fs)

source("../RPadrino/data-raw/dev_utils.R")
library(pdbDigitUtils)

xl <- read_pdb("../RPadrino/data-raw/hand_cleaned_padrino.xlsx")
tl <- read_pdb("../RPadrino/data-raw/pdb_tomos.xlsx")

xl <- lapply(seq_along(xl),
             function(ind, pdb, tomos) {
               
               rbind(pdb[[ind]], tomos[[ind]])
               
             },
             pdb = xl,
             tomos = tl)

names(xl) <- names(tl)

if(dir_exists("padrino-database")) {

  dir_delete("padrino-database")
  
  dir_create("padrino-database/raw")
  dir_create("padrino-database/clean")
  
} 

xl <- lapply(xl, function(x) {
  class(x) <- 'data.frame'
  x[x == "NA"] <- NA
  return(x)
})



for(i in seq_along(xl)) {
  
  temp <- names(xl)[i]
  
  write.csv(xl[[i]], 
            file         = paste("padrino-database/raw/", temp,".csv", sep = ""),
            row.names    = FALSE,
            quote        = TRUE,
            na           = "NA",
            fileEncoding = "UTF-8")
  
}

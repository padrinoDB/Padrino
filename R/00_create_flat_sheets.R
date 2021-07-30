library(fs)

library(pdbDigitUtils)

xl <- read_pdb("padrino-database/xl/hand_cleaned_padrino.xlsx")
tl <- read_pdb("padrino-database/xl/pdb_tomos.xlsx")
tl$Metadata$.test_passed <- NA

xl <- lapply(seq_along(xl),
             function(ind, pdb, tomos) {
               
               rbind(pdb[[ind]], tomos[[ind]])
               
             },
             pdb = xl,
             tomos = tl)

names(xl) <- names(tl)

dir_walk("padrino-database/raw", fun = file_delete)

 
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

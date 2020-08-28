library(fs)

source("../RPadrino/data-raw/dev_utils.R")

xl <- .read_all_sheets("../RPadrino/data-raw/hand_cleaned_padrino.xlsx")

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
  
  out <- xl[[i]]
  
  out[out == "NA"] <- NA
  
  write.csv(xl[[i]], 
            file         = paste("padrino-database/raw/", temp,".csv", sep = ""),
            row.names    = FALSE,
            quote        = TRUE,
            na           = "NA",
            fileEncoding = "UTF-8")
  
}

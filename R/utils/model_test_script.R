# Testing script for Hiwis

library(pdbDigitUtils)

# Using development version that may be ahead of main branch
devtools::load_all("../RPadrino")
# library(RPadrino)

# x <- read_pdb("padrino-database/xl/hand_cleaned_padrino.xlsx")
# 
# test_model(x, "aaa312")
# 
# y <- read_pdb("padrino-database/xl/pdb_test_baer_corrected.xlsx")
# 
# test_model(y, "test02")
# 
# z <- read_pdb("padrino-database/xl/pdb_tomos_baer.xlsx")
# test_model(z, "test02")
# 
# z <- read_pdb("padrino-database/xl/pdb_test_moore_2016.xlsx")
# test_model(z, "ccccc1")
# 
# a <- pdb_make_proto_ipm(z, "ccccc1")
# b <- pdb_make_ipm(a)

x <- read_pdb("padrino-database/xl/pdb_tomos.xlsx")

test_model(x, "ddddd1")

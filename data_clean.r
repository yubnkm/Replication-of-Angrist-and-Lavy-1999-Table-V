#----------------------------------------------------------
# data_clean.R
#----------------------------------------------------------
# Packages required: data.table
renv::activate()
renv::install("data.table")
renv::install("languageserver")
renv::install("httpgd")
library(data.table)

dt <- fread("final4.csv")

# Subtract 100 if avg score > 100
dt[avgverb > 100, avgverb := avgverb - 100]
dt[avgmath > 100, avgmath := avgmath - 100]

# Generate instrument variables
dt[, func1 := c_size / (floor((c_size - 1)/40) + 1)]
dt[, func2 := cohsize / (floor(cohsize/40) + 1)]

# Replace avgverb, passverb, avgmath, passmath  with NA if verbsize or mathsize is 0
dt[verbsize == 0, c("avgverb","passverb") := .(NA, NA)]
dt[mathsize == 0, c("avgmath","passmath") := .(NA, NA)]

# 1 < classize < 45, c_size > 5, c_leom==1, c_pik<3, avgverb not NA
dt <- dt[classize > 1 & classize < 45 &
         c_size > 5 &
         c_leom == 1 & c_pik < 3 &
         !is.na(avgverb)]

# disc indicator
dt[, disc := as.integer(
  (c_size >= 36 & c_size <= 45) |
  (c_size >= 76 & c_size <= 85) |
  (c_size >= 116 & c_size <= 125)
)]

# c_size^2
dt[, c_size2 := (c_size^2)/100]

# trend
dt[, trend := fcase(
  c_size >= 0 & c_size <= 40,     as.numeric(c_size),
  c_size >= 41 & c_size <= 80,    as.numeric(20 + (c_size / 2)),
  c_size >= 81 & c_size <= 120,   as.numeric((100 / 3) + (c_size / 3)),
  c_size >= 121 & c_size <= 160,  as.numeric((130 / 3) + (c_size / 4)),
  rep(TRUE, .N), NA_real_  
)]

# save it
fwrite(dt, "final_data_clean.csv")


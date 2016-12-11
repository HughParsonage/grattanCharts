library(data.table)

avbl_fractions <-
  data.table(val = c(0.1, 0.2, 0.25, 1/3, 1/2, 2/3, 3/4), 
             txt = c("one-tenth", "one-fifth", "one-quarter", "one-third", "one-half", 
                     "two-thirds", "three-quarters"), 
             Txt = c("One-tenth", "One-fifth", "One-quarter", "One-third", "One-half", 
                     "Two-thirds", "Three-quarters"))

devtools::use_data(avbl_fractions, internal = TRUE, overwrite = TRUE)

  


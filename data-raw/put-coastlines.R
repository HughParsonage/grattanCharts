library(data.table)
library(magrittr)

Australian_coastline <- 
  fread("https://media.githubusercontent.com/media/HughParsonage/Australian-coastline/master/Australian-coastline.tsv")

Sydney_coastline <- 
  Australian_coastline %>% 
  .[lat %between% c(-34.1, -33.40)] %>% 
  .[long %between% c(150.7, 153)] %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)] %>%
  .[, c("group", "N", "rowname", "hole", "piece", "id") := NULL]

Melbourne_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-38.2, -37.5)] %>%
  .[long %between% c(144.5, 145.2)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)] %>%
  .[, c("group", "N", "rowname", "hole", "piece", "id") := NULL]

Brisbane_coastline <- 
  Australian_coastline %>%
  .[lat %between% c(-28.14, -26.79)] %>%
  .[long %between% c(150.0, 153.40)]  %>%
  # Islands cause difficulties
  .[, N := .N, by = group] %>%
  .[N == max(N)]

devtools::use_data(Sydney_coastline,
                   Melbourne_coastline,
                   overwrite = TRUE)


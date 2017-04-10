
out_width <- column_width <- 4.47222  # inches
fig_height <- 7.25
fig_width <- 11 
out_height <- column_width * fig_height / fig_width

devtools::use_data(column_width, 
                   fig_height,
                   fig_width,
                   out_height, 
                   out_width, 
                   overwrite = TRUE)


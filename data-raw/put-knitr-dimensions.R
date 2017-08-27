
out_width <- column_width <- 4.47222  # inches
fig_height <- 5.8
fig_width <- 8.864 
out_height <- column_width * fig_height / fig_width

devtools::use_data(column_width, 
                   fig_height,
                   fig_width,
                   out_height, 
                   out_width, 
                   overwrite = TRUE)


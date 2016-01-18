# Theme used to create journal ready figures easilys
#
# base_size Font size
# base_family Font used for everything, unless axis fonts etc. are defined
# lines_lwd = width of lines
# plot_grid = Do you want a grid?
# font_type Font type
# title_size = font size of title
# legend_size = font size of legend
# bg_col = background colour
# title_font = font use for title
# base_col  = font colour
# horz_grid = Do you want a horizontal grid?
# bord_size = width of a rectangular border
# alpha_leg = opacity of the legend. 0 = totally transparent
# strip_bg = colour background for facets
# grid_thick = A multiplier to apply to the grid lines. 0.8 would reduce thickness by 20%
# grid_type = Grid type. Default is a solid line
# ticks_xy = Do you want ticks on the x or y axis? "x" = x-axis only, "y" = y-axis only, "xy" = both axes.
# grid_cols = Colour of the grid. 2 element vector. First element is major grid colour. If only one element, the first will be used for minor grid.



theme_agile <- function(base_size = 11, base_family = "Arial", lines_lwd = 0.50, plot_grid = TRUE, axis_font = base_family, title_size = base_size*1.2, legend_size = base_size,
												bg_col = "white",title_font = base_family , base_col  = "black", axis_lines = TRUE,
												minor_grid = ifelse(plot_grid, TRUE, FALSE), vert_grid = ifelse(plot_grid, TRUE, FALSE), ticks_type = "outer", horz_grid = ifelse(plot_grid, TRUE, FALSE), alpha_leg = 0.1, bord_size = 0,
												legend_bg = "white", strip_bg = "white", grid_thick = 1,
												grid_type = "solid", ticks_xy  = "xy", grid_cols = c("grey50", "grey70")){
	theme_bw()+
		ggplot2::theme(
			plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),
			text = ggplot2::element_text(family = base_family, size = base_size),
			axis.line =  element_line(size = ifelse(axis_lines, grid::unit(lines_lwd, "mm"),0), color = "black"),
			axis.ticks.length = grid::unit(ifelse(ticks_type == "outer", 0.15, -0.15), "cm"),
			axis.ticks.x =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "x"), grid::unit(lines_lwd, "cm"),0), color = "black"),
			axis.ticks.y =  element_line(size = ifelse(stringr::str_detect(ticks_xy, "y"), grid::unit(lines_lwd, "cm") ,0), color = "black"),
			axis.text.x = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font,margin=margin(ifelse(ticks_type == "inner", 11, 5),5,10,5,"pt")),
			axis.text.y = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font, margin=margin(5,ifelse(ticks_type == "inner", 11, 5),10,5,"pt")),
			axis.title.y = ggplot2::element_text(size =  base_size, colour = base_col , vjust = 1.5, family = axis_font),
			axis.title.x = ggplot2::element_text(size = base_size,colour = base_col ,vjust = -.5, family = axis_font),
			panel.background = ggplot2::element_rect(fill = bg_col),
			plot.background = ggplot2::element_rect(fill = bg_col),
			panel.border = ggplot2::element_rect(colour = "black", fill=NA, size = bord_size),
			panel.grid.major.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, grid_cols[1],bg_col), size = ifelse(vert_grid,0.25 * grid_thick, 0)),
			panel.grid.minor.x = ggplot2::element_line(linetype = grid_type,colour = ifelse(vert_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)   ],bg_col),bg_col), size = ifelse(vert_grid,0.15* grid_thick, 0)),
			panel.grid.major.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, grid_cols[1],bg_col), size = ifelse(horz_grid,0.25* grid_thick, 0)),
			panel.grid.minor.y = ggplot2::element_line(linetype = grid_type,colour = ifelse(horz_grid, ifelse(minor_grid, grid_cols[2 - (length(grid_cols) == 1)  ],bg_col),bg_col), size = ifelse(horz_grid,0.15* grid_thick, 0)),
			plot.title = ggplot2::element_text(face="bold",vjust = 2, colour = base_col , size = title_size, family = title_font),
			legend.background = ggplot2::element_rect(fill = scales::alpha(legend_bg, alpha_leg)), legend.key = ggplot2::element_blank(),
			legend.text = ggplot2::element_text(size = legend_size, family = base_family),
			legend.title = element_blank(),
			strip.background =  ggplot2::element_rect(colour = strip_bg, fill = strip_bg),
			strip.text.x = ggplot2::element_text(size = base_size + 1),
			strip.text.y = ggplot2::element_text(size = base_size + 1)
		)
}

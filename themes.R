#' Theme used for blog posts
#'
#' Theme used for blogs
#'
#' @param base_size Font size
#' @param base_family Font used for everything, unless axis fonts etc. are defined
#' @param plot_type = the type of plot. "Formal" or "Informal"
#' @param lines_lwd = width of lines
#' @param plot_grid = Do you want a grid?
#' @param font.type Font type
#' @param title.size = font size of title
#' @param legend_size = font size of legend
#' @param bg_col = background colour
#' @param title_font = font use for title
#' @param base_col  = font colour
#' @param ticks_length = length of the axis ticks
#' @param horz_grid = Do you want a horizontal grid?
#' @param bord_size = width of a rectangular border
#' @param alpha_leg = opacity of the legend. 0 = totally transparent
#' @param strip_bg = colour background for facets
#' @param grid_thick = A multiplier to apply to the grid lines. 0.8 would reduce thickness by 20%
#' @return ggplot2 theme
#'
#' @export



theme_agile <- function(base_size = 11, base_family = "Arial", plot_type = "formal", lines_lwd = 0.25, ticks_type = "outer", plot_grid = TRUE, axis_font = base_family, title_size = base_size*1.2, legend_size = base_size,
											 bg_col = ifelse(plot_type == "formal", "white", "#F0F0F0"), title_font = base_family , base_col  = "black", axis_lines = TRUE,
											 minor_grid = ifelse(plot_grid, TRUE, FALSE), vert_grid = ifelse(plot_grid, TRUE, FALSE), ticks_length = ifelse(ticks_type == "outer", 0.2, -0.2), horz_grid = ifelse(plot_grid, TRUE, FALSE), alpha_leg = 0.1, bord_size = 0,
											 legend_bg = ifelse(plot_type == "formal", "white", "#F0F0F0"), strip_bg = ifelse(plot_type == "formal", "white", "khaki1"), grid_thick = 1){
	theme_bw()+
		ggplot2::theme(
		plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),
 		text = ggplot2::element_text(family = base_family, size = base_size),
		axis.line =  element_line(size = ifelse(axis_lines, grid::unit(lines_lwd, "mm"),0), color = "black"),
		axis.ticks_length = grid::unit(ticks_length, "cm"),
		axis.ticks.margin = grid::unit(ifelse(ticks_length > 0,0.25, -ticks_length + 0.25) , "cm"),
		axis.text.x = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font),
		axis.text.y = ggplot2::element_text(size = base_size, colour = base_col , family = axis_font),
		axis.title.y = ggplot2::element_text(size =  base_size, colour = base_col , vjust = 1.5, family = axis_font),
		axis.title.x = ggplot2::element_text(size = base_size,colour = base_col ,vjust = -.5, family = axis_font),
		panel.background = ggplot2::element_rect(fill = bg_col),
		plot.background = ggplot2::element_rect(fill = bg_col),
		panel.border = ggplot2::element_rect(colour = "black", fill=NA, size = bord_size),
		panel.grid.major.x = ggplot2::element_line(colour = ifelse(vert_grid, "grey60",bg_col), size = ifelse(vert_grid,0.45 * grid_thick, 0)),
		panel.grid.minor.x = ggplot2::element_line(colour = ifelse(vert_grid, ifelse(minor_grid, "grey80",bg_col),bg_col), size = ifelse(vert_grid,0.35* grid_thick, 0)),
		panel.grid.major.y = ggplot2::element_line(colour = ifelse(horz_grid, "grey60",bg_col), size = ifelse(horz_grid,0.45* grid_thick, 0)),
		panel.grid.minor.y = ggplot2::element_line(colour = ifelse(horz_grid, ifelse(minor_grid, "grey80",bg_col),bg_col), size = ifelse(horz_grid,0.35* grid_thick, 0)),
		panel.grid.major = ggplot2::element_line(colour = "grey40", size=0.45),
		plot.title = ggplot2::element_text(face="bold",hjust = ifelse(plot_type == "formal", 0.5, 0) ,vjust = 2, colour = base_col , size = title.size, family = title_font),
		legend.background = ggplot2::element_rect(fill = scales::alpha(legend_bg, alpha_leg)), legend.key = ggplot2::element_blank(),
		legend.text = ggplot2::element_text(size = legend_size),
		legend.title = element_blank(),
		strip.background =  ggplot2::element_rect(colour = strip_bg, fill = strip_bg),
		strip.text.x = ggplot2::element_text(size = base_size + 1),
		strip.text.y = ggplot2::element_text(size = base_size + 1)
	)
}

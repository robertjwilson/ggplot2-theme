
# base_size = Font size
# base_family = Font used for everything, unless axis fonts etc. are defined
# plot.type = the type of plot. "Formal" or "Informal"
# plot.grid = Do you want a grid?
# font.type Font type
# title.size = font size of title
# legend.size = font size of legend
# bg.col = background colour
# title.font = font use for title
# base.col = font colour
# ticks.length = length of the axis ticks
# horz.grid = Do you want a horizontal grid?
# bord.size = width of a rectangular border
# alpha.leg = opacity of the legend. 0 = totally transparent
# strip.bg = colour background for facets
# ggplot2 theme



theme_agile <- function(base_size = 12, base_family = "Arial", plot.type = "formal", lines.lwd = 0.25, ticks.type = "outer", plot.grid = TRUE, axis.font = base_family, title.size = base_size*1.2, legend.size = base_size,
											 bg.col = ifelse(plot.type == "formal", "white", "#F0F0F0"), title.font = base_family , base.col = "black", axis.lines = TRUE,
											 minor.grid = ifelse(plot.grid, TRUE, FALSE), vert.grid = ifelse(plot.grid, TRUE, FALSE), ticks.length = ifelse(ticks.type == "outer", 0.2, -0.2), horz.grid = ifelse(plot.grid, TRUE, FALSE), alpha.leg = 0.1, bord.size = 0,
											 legend.bg = ifelse(plot.type == "formal", "white", "#F0F0F0"), strip.bg = ifelse(plot.type == "formal", "white", "grey80")){
	theme_bw()+
		ggplot2::theme(
		# Plot margins and finally line annotations
		plot.margin = grid::unit(c(1, 1, .5, .7), "cm"),

 		text = ggplot2::element_text(family = base_family, size = base_size),
		axis.line =  element_line(size = ifelse(axis.lines, grid::unit(lines.lwd, "mm"),0), color = "black"),
		axis.ticks.length = grid::unit(ticks.length, "cm"),
		axis.ticks.margin = grid::unit(ifelse(ticks.length > 0,0.25, -ticks.length + 0.25) , "cm"),
		axis.text.x = ggplot2::element_text(size = base_size, colour = base.col, family = axis.font),
		axis.text.y = ggplot2::element_text(size = base_size, colour = base.col, family = axis.font),
		axis.title.y = ggplot2::element_text(size =  base_size, colour = base.col, vjust = 1.5, family = axis.font),
		axis.title.x = ggplot2::element_text(size = base_size,colour = base.col,vjust = -.5, family = axis.font),
		panel.background = ggplot2::element_rect(fill = bg.col),
		plot.background = ggplot2::element_rect(fill = bg.col),
		panel.border = ggplot2::element_rect(colour = "black", fill=NA, size = bord.size),
		panel.grid.major.x = ggplot2::element_line(colour = ifelse(vert.grid, "grey60",bg.col), size = ifelse(vert.grid,0.45, 0)),
		panel.grid.minor.x = ggplot2::element_line(colour = ifelse(vert.grid, ifelse(minor.grid, "grey80",bg.col),bg.col), size = ifelse(vert.grid,0.35, 0)),
		panel.grid.major.y = ggplot2::element_line(colour = ifelse(horz.grid, "grey60",bg.col), size = ifelse(horz.grid,0.45, 0)),
		panel.grid.minor.y = ggplot2::element_line(colour = ifelse(horz.grid, ifelse(minor.grid, "grey80",bg.col),bg.col), size = ifelse(horz.grid,0.35, 0)),
		panel.grid.major = ggplot2::element_line(colour = "grey40", size=0.45),
		plot.title = ggplot2::element_text(face="bold",hjust = ifelse(plot.type == "formal", 0.5, 0) ,vjust = 2, colour = base.col, size = title.size, family = title.font),
		legend.background = ggplot2::element_rect(fill = scales::alpha(legend.bg, alpha.leg)), legend.key = ggplot2::element_blank(),
		legend.text = ggplot2::element_text(size = legend.size),
		legend.title = element_blank(),
		strip.background = ggplot2::element_rect(fill = strip.bg),
		strip.text.x = ggplot2::element_text(size = base_size + 1),
		strip.text.y = ggplot2::element_text(size = base_size + 1)
	)
}

---
title: "theme_agile"
output: html_document
---

This is an outline of the core functionality of theme_agile.


```{r setup, include=FALSE}
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


```

## Some core theme_agile functionality

theme_agile is a ggplot2 theme designed to make journal ready figures easier, but also to make nice figures for a general audience.

I will use the mtcars data set to show the core functionality of theme_agile.

First, a plot with the defaults

## Including Plots

You can also embed plots, for example:

```{r echo=TRUE, warning= FALSE, message=FALSE}
require(ggplot2)


ggplot(mtcars, aes(mpg, disp))+
	geom_point()+
	theme_agile()

```

We may not want to have a minor grid, so let's remove that. This is done by simply setting minor.grid = FALSE.


```{r echo=TRUE, warning= FALSE, message=FALSE}

ggplot(mtcars, aes(mpg, disp))+
	geom_point()+
	theme_agile(minor.grid = FALSE)

```


If we want to get rid off the vertical grid we can simply set vert.grid = FALSE.


```{r echo=TRUE, warning= FALSE, message=FALSE}

ggplot(mtcars, aes(mpg, disp))+
	geom_point()+
	theme_agile(vert.grid = FALSE)

```


Now, let's consider the problem of creating plots that are acceptable for scientific journals.

Imagine we have a journal that does not allow grids and also requires inner axis ticks. We can do this quite easily.

```{r echo=TRUE, warning= FALSE, message=FALSE}

ggplot(mtcars, aes(mpg, disp))+
	geom_point()+
	theme_agile(plot.grid = FALSE, ticks.type = "inner")

```



```{r, include=FALSE}
   # add this chunk to end of mycode.rmd
   file.rename(from="Readme.rmd", 
               to="README.md")
```

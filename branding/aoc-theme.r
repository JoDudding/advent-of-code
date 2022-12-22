#-------------------------------------------------------------------------------
#' font
#-------------------------------------------------------------------------------

#library(showtext)
#font_add_google("Fira Code", family = "firacode")
#showtext_auto()

#-------------------------------------------------------------------------------
#' set colours
#-------------------------------------------------------------------------------

aoc_black <- '#0f0f23'
aoc_white <- '#cccccc'
aoc_green <- '#00cc00'
aoc_dgreen <- '#009900'
aoc_grey <- '#666666'
aoc_dgrey <- '#333333'
aoc_yellow <- '#ffff66'

#-------------------------------------------------------------------------------
#' geom defaults
#-------------------------------------------------------------------------------

update_geom_defaults("col", list(fill = aoc_green, colour = NA))
update_geom_defaults("line", list(fill = NA, colour = aoc_green))
update_geom_defaults("path", list(fill = NA, colour = aoc_green))
update_geom_defaults("text", list(fill = NA, colour = aoc_white))

#-------------------------------------------------------------------------------
#' create theme
#-------------------------------------------------------------------------------


theme_aoc <- function(
    base_size = 10, 
    base_family = "mono"
) {
  
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      line = element_line(
        colour = aoc_grey,
        size = 0.75,
        linetype = 1,
        lineend = "butt"
      ),
      rect = element_rect(
        fill = "white",
        colour = "white",
        size = 0.1,
        linetype = 1
      ),
      text = element_text(
        family = base_family,
        face = "plain",
        colour = aoc_white,
        size = base_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.7,
        margin = ggplot2::margin(),
        debug = FALSE
      ),
      
      axis.line.x = element_line(colour = aoc_grey, size = 0.25),
      axis.line.y = element_blank(),
      axis.text = element_text(colour = aoc_white),
      axis.ticks = element_blank(),
      axis.title = element_text(
        colour = aoc_white, 
        size = rel(0.8), 
        face = 'bold'
      ),
      
      panel.border = element_rect(fill = NA, colour = NA),
      panel.background = element_rect(fill = aoc_black, colour = NA),
      # gaps between facets
      panel.spacing = unit(1, 'lines'),
      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = aoc_grey, size = 0.25),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      plot.background = element_rect(fill = aoc_black, colour = NA),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(
        size = rel(1.15),
        colour = aoc_dgreen,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = element_text(
        size = rel(0.9),
        colour = aoc_white,
        hjust = 0
      ),
      plot.caption = element_text(
        hjust = 0,
        colour = aoc_dgreen
      ),
      
      legend.background = element_rect(fill = aoc_black, colour = NA),
      legend.key = element_rect(fill = aoc_black, colour = NA),
      legend.position = 'top',
      legend.justification = 1,
      legend.text = element_text(
        colour = aoc_white, 
        margin = margin(r = 10, unit = "pt")
      ),
      legend.title = element_text(
        colour = aoc_white, 
        size = rel(0.8), 
        face = 'bold', 
        margin = margin(r = 10, unit = "pt")
      ),
      
      strip.text = element_text(
        colour = aoc_green, 
        size = rel(0.9), 
        face = "bold",
        hjust = 0
      ),
      strip.background = element_rect(
        colour = NA,
        fill = aoc_yellow,
        size = 0.35
      )
    )
}


theme_set(theme_aoc())

#-------------------------------------------------------------------------------
# null theme
#-------------------------------------------------------------------------------

theme_aoc_null <- function() {
  theme_aoc() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.title = element_blank()
  )
}

#-------------------------------------------------------------------------------
# heatmap theme
#-------------------------------------------------------------------------------

theme_aoc_heat <- function() {
  theme_aoc() +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(0.7)),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.key.size = unit(0.8, 'lines'), 
    legend.key.width = unit(3.5, 'lines')
  )
}


#-------------------------------------------------------------------------------

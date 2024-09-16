#' Custom ggplot2 Theme for Consistent Plot Styling
#'
#' This function creates a custom `ggplot2` theme for consistent plot styling across graphics.
#' It adjusts various elements of the plot including text, axes, legend, and background to provide
#' a clean and professional appearance. The theme can be applied to any `ggplot2` plot to ensure
#' uniform formatting.
#'
#' @param base_size A numeric value specifying the base font size for text elements. Default is 7.
#' @param base_family A character string specifying the base font family for text elements. Default is an empty string, which uses the default font.
#'
#' @return A `ggplot2` theme object with customized settings for plot elements.
#'
#' @details
#' The `theme_pers` function modifies the following elements:
#' \itemize{
#'   \item `line`: Line color, size, and style.
#'   \item `rect`: Rectangle background and border.
#'   \item `text`: Font family, size, and color for text elements.
#'   \item `axis.text`: Size of axis text.
#'   \item `axis.line`: Visibility of axis lines.
#'   \item `axis.ticks`: Style and size of axis ticks.
#'   \item `legend`: Customization for legend appearance and positioning.
#'   \item `panel`: Background, border, and grid lines of the plot panel.
#'   \item `strip`: Background and text styling for facet labels.
#'   \item `plot`: Overall background, title size, and margin settings.
#' }
#'

theme_pers <-function (base_size = 7, base_family = ""){
  theme(
    line = element_line(colour = "black", size = 0.3, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.3, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(size = rel(0.8)),
    axis.line = element_blank(),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.ticks = element_line(),
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    axis.ticks.length = unit(1, "mm"),
    axis.ticks.margin = unit(1, "mm"),
    legend.background = element_rect(colour = NA),
    legend.spacing = unit(-0.5, "lines"),
    legend.key = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(size = rel(0.8), hjust = 0),
    legend.title.align = 0.5,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(colour = NA),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "grey90", size = 0.2),
    panel.grid.minor = element_blank(),
    panel.margin = unit(0.25, "lines"),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    strip.background = element_blank(),
    strip.text = element_text(face="bold"),
    strip.text.x = element_text(face="bold"),
    strip.text.y = element_text(face="bold", angle = -90),
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2)),
    plot.margin=unit(rep(1, 4), "mm"),
    complete = TRUE)
}

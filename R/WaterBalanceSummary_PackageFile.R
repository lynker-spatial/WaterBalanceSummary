#' @importFrom arrow read_parquet
#' @importFrom aws.s3 s3read_using get_bucket_df save_object
#' @importFrom aws.signature signature_v4_auth
#' @importFrom cowplot plot_grid
#' @importFrom dplyr `%>%` distinct arrange where across bind_rows contains starts_with filter first if_else mutate lag select group_by summarize rename ungroup left_join inner_join recode case_when
#' @importFrom ggplot2 ggplot ggplotGrob rel element_line element_rect scale_fill_gradient geom_col theme_gray geom_bar geom_point geom_sf scale_color_manual xlab ylab coord_sf ggtitle aes stat facet_wrap scale_fill_manual labs theme element_text unit margin element_blank scale_y_continuous
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orienteering annotation_map_tile
#' @importFrom glue glue
#' @importFrom grid grid.draw grid.newpage
#' @importFrom gridExtra grid.arrange
#' @importFrom lubridate as_date month
#' @importFrom purrr map
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales pretty_breaks
#' @importFrom sf st_as_sf st_read st_transform st_intersection st_join st_centroid st_crs
#' @importFrom stats cor sd na.omit var
#' @importFrom stringr str_replace_all str_starts str_detect
#' @importFrom tidyr pivot_longer separate pivot_wider drop_na
#' @importFrom tools file_path_sans_ext
#' @importFrom utils str

WaterBalanceSummaryEnv <- new.env()


NULL













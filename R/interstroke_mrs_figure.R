#' Interstroke MRS Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param x_axis_variable A categorical variable that will be placed on the x-axis of the proportion figure.
#' @param dropped_prop_levels **optional** Named proportion levels to be excluded. Default includes all predictor levels. Format = c("x", "y", "z")
#' @param na_rm **optional** A logical scalar. Should NA values be removed?. Default is FALSE.
#' @param text_size **Optional** Assign value for text size.
#' @export
#' @return An Interstroke MRS ggplot figure
#' @examples
#' is_figure_mrs(interstroke_sleep, csdiffsl)
#' is_figure_mrs(interstroke_sleep %>% drop_na(csdiffsl) %>% filter(case== "Case"),csdiffsl)
#' is_figure_mrs(interstroke_proxy, regionnn7, highwhr)
#' is_figure_mrs(interstroke_proxy, regionnn7, highwhr, dropped_prop_levels = c("Low", "Moderate"), na_rm = TRUE)
is_figure_mrs <- function(interstroke_data, x_axis_variable,
                                 dropped_prop_levels = NULL, na_rm = FALSE, text_size = NULL) {

  # Quote arguments
  x_axis_variable_quo <- rlang::enquo(x_axis_variable)

  # Remove NA's if na_rm == TRUE
  if(na_rm == TRUE) {
    interstroke_data <- interstroke_data %>% tidyr::drop_na(!!x_axis_variable)
  }

  is_figure_mrs <-
    ggplot2::ggplot(interstroke_data, ggplot2::aes(x=!!x_axis_variable_quo, y=caseid, fill=mrscorec), na.rm = TRUE) +
    ggplot2::geom_bar(position="fill", stat="identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::labs(fill = "MRS", x ="", y = "" )+
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(color = "black"),
          axis.text.x = ggplot2::element_blank(),
          aspect.ratio = 1/2)

  return(is_figure_mrs)
}

#is_figure_mrs(interstroke_sleep %>% drop_na(csdiffsl) %>% filter(case== "Case"),csdiffsl)
#
# is_figure_proportion(interstroke_pollution, regionnn7, cooking_fuel_3_level)
#
# is_figure_proportion(interstroke_stress, education, depdosedef3)
#
# is_figure_proportion(interstroke_stress, regionnn7, depdosedef3)
#
# is_figure_proportion(interstroke_stress, regionnn7, depdosedef3)

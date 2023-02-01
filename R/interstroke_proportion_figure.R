#' Interstroke Proportion Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param x_axis_variable A categorical variable that will be placed on the x-axis of the proportion figure.
#' @param proportion_variable The predictor variable.
#' @param dropped_prop_levels **optional** Named proportion levels to be excluded. Default includes all predictor levels. Format = c("x", "y", "z")
#' @param na_rm **optional** A logical scalar. Should NA values be removed?. Default is FALSE.
#' @param text_size **Optional** Assign value for text size.
#' @export
#' @return An Interstroke proportion ggplot figure
#' @examples
#' is_figure_proportion(interstroke_bmi, regionnn7, bmi_3_level)
#' is_figure_proportion(interstroke_proxy, regionnn7, highwhr)
#' is_figure_proportion(interstroke_proxy, regionnn7, highwhr, dropped_prop_levels = c("Low", "Moderate"), na_rm = TRUE)
is_figure_proportion <- function(interstroke_data, x_axis_variable, proportion_variable,
                                 dropped_prop_levels = NULL, na_rm = FALSE, text_size = NULL) {

  # Quote arguments
  x_axis_variable_quo <- rlang::enquo(x_axis_variable)
  proportion_variable_quo <- rlang::enquo(proportion_variable)

  # Remove NA's if na_rm == TRUE
  if(na_rm == TRUE) {
    interstroke_data <- interstroke_data %>% tidyr::drop_na(!!proportion_variable_quo)
  }

  # Create data summary
  interstroke_data_summary <- interstroke_data %>%
    select(!!x_axis_variable_quo, case,!!proportion_variable_quo) %>%
    group_by(!!x_axis_variable_quo,case,!!proportion_variable_quo) %>%
    summarise(n = n()) %>%
    mutate(perc = ((n / sum(n)) * 100)) %>%
    mutate(x_axis_variable = !!x_axis_variable_quo)

  interstroke_data_summary_overall <- interstroke_data %>%
    select(case, !!proportion_variable_quo) %>%
    group_by(case, !!proportion_variable_quo) %>%
    summarise(n = n()) %>%
    mutate(x_axis_variable = "Overall",
           perc = ((n / sum(n)) * 100))

  #Append to dataframe
  interstroke_data_summary <- bind_rows(interstroke_data_summary, interstroke_data_summary_overall)

  #Drop unwanted variables
  if( exists("dropped_prop_levels") )
  {
    interstroke_data_summary <- filter(interstroke_data_summary, !(!!proportion_variable_quo %in% dropped_prop_levels))
  }

  #Assign levels to correct order
  interstroke_data_summary$x_axis_variable <- factor(interstroke_data_summary$x_axis_variable)

  interstroke_data_summary$x_axis_variable <- forcats::fct_relevel(interstroke_data_summary$x_axis_variable, "Overall", after = 0)

    #Prepare counts of case/controls for caption
  interstroke_counts <- interstroke_data_summary %>%
    select(x_axis_variable, case, !!proportion_variable_quo, n) %>%
    group_by(x_axis_variable, case) %>%
    summarise(n = sum(n), .groups = "drop_last") %>%
    tidyr::pivot_wider(names_from = case, values_from = n) %>%
    tidyr::unite("case_control", Case:Control, sep = "/")

  #Center allignment
  # library(stringr)
  interstroke_counts$case_control <- stringr::str_pad(interstroke_counts$case_control,
                                             # max(nchar(interstroke_counts$case_control)),
                                             11,
                                             side="both", pad=" ")

  #Add count labels to data summary df to allow facet_wrap labels
  interstroke_data_summary <- interstroke_data_summary %>% left_join(interstroke_counts)

  #Smart y-axis limits
  plot_height <- interstroke_data_summary %>%
    select(!!x_axis_variable_quo, case, !!proportion_variable_quo, perc) %>%
    group_by(!!x_axis_variable_quo, case) %>%
    summarise(max_stack = sum(perc)) %>%
    filter(max_stack == max(max_stack)) %>%
    transmute(max_stack = round((max_stack + 20), -1)) %>% # adjusts the height based on max value
    pull()

  #Set max plot height to 100 percent
  if (max(plot_height) >100) {
    plot_height <- 100
  }

  #Adjust text size of plots
  if(is.null(text_size)){
    text_size = 3.7
  }
  theme_text_size = (14/5) * text_size

  is_proportion_figure <- ggplot2::ggplot() +
    ggplot2::geom_col(data = interstroke_data_summary,
                      ggplot2::aes(x = case, y = perc, fill = !!proportion_variable_quo),
                      position='stack', width = 0.3) + #adjust width for bar thickness
    ggplot2::geom_text(data = data.frame(x_axis_variable = "Overall", label = "Cases/Controls"),
                       aes(x = -Inf, y = -Inf, label = label), vjust = 12, hjust = 1.2,
                       fontface = "bold", size = text_size) +
    ggplot2::geom_text(data = interstroke_counts,
                       mapping = aes(x = 0, y = -Inf, label = case_control),
                       # hjust   = -0.25,
                       hjust   = -0.15,
                       # vjust   = 12, fontface = "plain", size = text_size) +
                       vjust   = 12, fontface = "plain", size = text_size, check_overlap = TRUE) +
    ggplot2::facet_wrap( ~ x_axis_variable, strip.position = "bottom", nrow = 1,
                         labeller = ggplot2::label_wrap_gen(10)) + #adjust label_wrap_gen(x) to change allowed no. of characters
    ggplot2::scale_x_discrete(expand =  ggplot2::expansion(add = 1)) + #expand x axis to display continous x-axis line
    ggplot2::scale_y_continuous(limits=c(0, max(plot_height)), breaks = seq(0, max(plot_height), 10),
                                expand = c(0, 0)) +
    ggplot2::labs(y = "Percentage", x =  ggplot2::element_blank()) +
    ggplot2::scale_fill_viridis_d(begin = 0,
                                  end = 0.8,
                                  na.value = 1) +
    ggplot2::coord_cartesian(clip = 'off') + #Allow text outside plotting window
    ggplot2::theme(strip.background =  ggplot2::element_blank(),
                   strip.placement = "outside",
                   panel.background =  ggplot2::element_blank(),
                   panel.spacing = ggplot2::unit(0, "mm"),
                   plot.background =  ggplot2::element_rect(colour = "darkred", size = 1.5), #red border
                   text = ggplot2::element_text(size=theme_text_size),
                   axis.line =  ggplot2::element_line(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.ticks.length=ggplot2::unit(.25, "cm"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust=1,vjust=1.2),
                   axis.title.y = ggplot2::element_text(vjust=5)) + # Top, right, bottom, left
    if(!is.null(dropped_prop_levels)){
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.justification = c("left", "top"),
                     legend.background = ggplot2::element_blank(),
                     legend.key.height = ggplot2::unit(0.3, "cm"),
                     legend.text = ggplot2::element_text(size=theme_text_size),
                     legend.position = c(0.05, 1),
                     plot.margin = ggplot2::margin(0.5, 0.5, 2, 2.5, "cm")) # Top, right, bottom, left
    }
  else{
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank(),
                   legend.key.height = ggplot2::unit(0.3, "cm"),
                   legend.text = ggplot2::element_text(size=theme_text_size),
                   legend.position = "top",
                   plot.margin = ggplot2::margin(0.5, 0.5, 2, 2.5, "cm")) # Top, right, bottom, left
  }

  return(is_proportion_figure)
}

# is_figure_proportion(interstroke_bmi, regionnn7, bmi_3_level)
#
# is_figure_proportion(interstroke_pollution, regionnn7, cooking_fuel_3_level)
#
# is_figure_proportion(interstroke_stress, education, depdosedef3)
#
# is_figure_proportion(interstroke_stress, regionnn7, depdosedef3)
#
# is_figure_proportion(interstroke_stress, regionnn7, depdosedef3)

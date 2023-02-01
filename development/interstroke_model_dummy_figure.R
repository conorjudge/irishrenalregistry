#' Interstroke Model Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param x_axis_variable A categorical variable that will be placed on the x-axis of the proportion figure.
#' @param models A list of model formulas to include in model figure (see example)
#' @export
#' @return An Interstroke model figure
#' @examples
#' is_figure_models(interstroke_data=interstroke_bmi, x_axis_variable=bmi_quintiles, models=c(case_num ~ bmi_quintiles, case_num ~ bmi_quintiles+eage+esex))
is_figure_models <- function(interstroke_data, x_axis_variable, models) {

  interstroke_modelling_data <- tribble(
    ~term, ~model, ~estimate, ~conf.low, ~conf.high,
    "Q1", 1, 1, 0.9, 1.1,
    "Q1", 2, 1, 0.9, 1.1,
    "Q1", 3, 1, 0.85, 1.2,
    "Q2", 1, 1.1, 1.05, 1.2,
    "Q2", 2, 1.1, 1, 1.3,
    "Q2", 3, 1.1, 1, 1.2,
    "Q3", 1, 1.3, 1.2, 1.4,
    "Q3", 2, 1.2, 1.1, 1.3,
    "Q3", 3, 1.2, 1.1, 1.3,
    "Q4", 1, 1.4, 1.3, 1.5,
    "Q4", 2, 1.3, 1.2, 1.4,
    "Q4", 3, 1.2, 1.2, 1.4,
    "Q5", 1, 1.5, 1.4, 1.6,
    "Q5", 2, 1.4, 1.3, 1.5,
    "Q5", 3, 1.3, 1.2, 1.4,
  )
  interstroke_modelling_data$model <- as_factor(interstroke_modelling_data$model)

  #X-axis variable assignment
  interstroke_modelling_data <- interstroke_modelling_data %>% add_column(x_axis_variable = "BMI quintiles")

  #---- Multiple predictors

  interstroke_modelling_data2 <- tribble(
    ~term, ~model, ~estimate, ~conf.low, ~conf.high,
    "Q1", 1, 1, 0.9, 1.1,
    "Q1", 2, 1, 0.9, 1.1,
    "Q1", 3, 1, 0.85, 1.2,
    "Q2", 1, 1.1, 1.05, 1.2,
    "Q2", 2, 1.1, 1, 1.3,
    "Q2", 3, 1.1, 1, 1.2,
    "Q3", 1, 1.3, 1.2, 1.4,
    "Q3", 2, 1.2, 1.1, 1.3,
    "Q3", 3, 1.2, 1.1, 1.3,
    "Q4", 1, 1.4, 1.3, 1.5,
    "Q4", 2, 1.3, 1.2, 1.4,
    "Q4", 3, 1.2, 1.2, 1.4,
    "Q5", 1, 1.5, 1.4, 1.6,
    "Q5", 2, 1.4, 1.3, 1.5,
    "Q5", 3, 1.3, 1.2, 1.4,
  )

  interstroke_modelling_data2$model <- as_factor(interstroke_modelling_data2$model)

  #X-axis variable assignment
  interstroke_modelling_data2 <- interstroke_modelling_data2 %>% add_column(x_axis_variable = "WHR quintiles")

  interstroke_modelling_data <- rbind(interstroke_modelling_data, interstroke_modelling_data2)

  rm(interstroke_modelling_data2)


  #-------------------------

  plot1<-is_figure_models_ggplot(interstroke_modelling_data)

  return(plot1)

}


#' Interstroke Model ggplot
#'
#' @param interstroke_modelling_data
#' @return An Interstroke models ggplot figure
is_figure_models_ggplot <- function(interstroke_modelling_data) {

  #Plot
  interstroke_model_figure <- ggplot(interstroke_modelling_data, aes(term, estimate, ymin = conf.low, ymax = conf.high, group = model)) +
    # geom_pointrange(aes(shape = model, colour = model), position = position_dodge(width=0.30)) +
    geom_point(aes(shape = model, colour = model), size = 3.5, position = position_dodge(width=0.30)) +
    geom_linerange(position = position_dodge(width=0.30)) +
    facet_wrap(~ x_axis_variable, strip.position = "bottom") +
    scale_y_continuous(name = "OR (95% CI)",
                       # breaks = c(0.75, 1, 1.5, 2, 2.5, 3),
                       # limits = c(0.75, 3),
                       trans = "log10", expand = c(0,0)) +
    geom_hline(yintercept = 1, lty = 2) +
    ggplot2::scale_colour_viridis_d(end = 0.8) +
    theme(panel.background =  ggplot2::element_blank(),
          axis.ticks.x = element_blank(),
          axis.line =  ggplot2::element_line(),
          axis.ticks.length=ggplot2::unit(.25, "cm"),
          text= ggplot2::element_text(size=12),
          legend.position = "top",
          legend.justification='left',
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.direction = "vertical"
    )

  # #Plot
  # interstroke_model_figure <- ggplot(interstroke_modelling_data, aes(term, estimate, ymin = conf.low, ymax = conf.high, group = model)) +
  #   # geom_pointrange(aes(shape = model, colour = model), position = position_dodge(width=0.30)) +
  #   geom_point(aes(shape = model, colour = model), size = 3.5, position = position_dodge(width=0.30)) +
  #   geom_linerange(position = position_dodge(width=0.30)) +
  #   scale_y_continuous(name = "OR (95% CI)",
  #                      # breaks = c(0.75, 1, 1.5, 2, 2.5, 3),
  #                      # limits = c(0.75, 3),
  #                      trans = "log10", expand = c(0,0)) +
  #   geom_hline(yintercept = 1, lty = 2) +
  #   ggplot2::scale_colour_viridis_d(end = 0.8) +
  #   theme(panel.background =  ggplot2::element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line =  ggplot2::element_line(),
  #         axis.ticks.length=ggplot2::unit(.25, "cm"),
  #         text= ggplot2::element_text(size=12),
  #         legend.position = "top",
  #         legend.justification='left',
  #         legend.title=element_blank(),
  #         legend.key=element_blank(),
  #         legend.direction = "vertical"
  #   )

  return(interstroke_model_figure)

}


is_figure_models()


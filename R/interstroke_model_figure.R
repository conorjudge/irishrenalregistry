#' Interstroke Model Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param models A list of model formulas to include in model figure (see example)
#' @param regression_type either "unconditional" or "conditional". Conditional models require +strata(caseid) at end of model formula.
#' @param breaks axis tick mark parameters, defaults to pretty_breaks.
#' @export
#' @return An Interstroke model figure
#' @examples
#' is_figure_models(interstroke_data=interstroke_bmi, x_axis_variable=bmi_quintiles, models=c(case_num ~ bmi_quintiles, case_num ~ bmi_quintiles+eage+esex))
is_figure_models <- function(interstroke_data, models, breaks=NULL, regression_type = NULL) {

  # make a list if needed
  x_axis_variable_extract <- c(models)
  x_axis_variable <- labels(terms(x_axis_variable_extract[[1]]))[1]

  interstroke_data_expr <- rlang::enexpr(interstroke_data)
  x_axis_variable_quo <- rlang::enquo(x_axis_variable)
  x_axis_variable_sym <- rlang::ensym(x_axis_variable)

  if(regression_type == "unconditional" | regression_type == "conditional") {
    interstroke_modelling_data <- is_logistic_regression(interstroke_data=!!interstroke_data_expr,
                                                         regression_type = regression_type,
                                                                       models=models)
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  interstroke_modelling_data <- interstroke_modelling_data %>%
    filter(grepl(x_axis_variable_sym, term)) %>%
    mutate(term = stringr::str_replace(term,as.character(x_axis_variable_sym),""))

  #Model as factor
  interstroke_modelling_data$model <- forcats::as_factor(interstroke_modelling_data$model)

  # Reference x axis variable ---------------------------
  #Perform stratified analysis for each subgroup
  factor_levels <- interstroke_data %>%
    dplyr::pull(!!x_axis_variable_sym) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  for(i in seq_along(models)){
    formula <- models[[i]]
    interstroke_modelling_data <- interstroke_modelling_data %>%
      tibble::add_row(.before = 1, term = factor_levels[1], estimate = 1.0, conf.low = NA, conf.high = NA,
                      model = is_model_label(interstroke_data = interstroke_data, model_formula = formula))
  }

  interstroke_modelling_data$term <- factor(interstroke_modelling_data$term, levels = factor_levels)

  if(is.null(breaks)){
    breaks = scales::pretty_breaks()
  }

  plot1<-is_figure_models_ggplot(interstroke_modelling_data, breaks = breaks,
                                 x_axis_variable = x_axis_variable_sym, interstroke_data = interstroke_data)

  return(plot1)

}


# is_figure_models(interstroke_data=interstroke_bmi,
#                  models=c(case_num~bmi_quintiles+strata(caseid),case_num~bmi_quintiles+eage+esex+whr+strata(caseid)),
#                  regression_type = "conditional")
#
# is_figure_models(interstroke_data=interstroke_bmi,
#                  models=c(case_num~smoking,case_num~smoking+eage+esex+whr),
#                  regression_type = "unconditional")
#
# is_figure_models(interstroke_data=interstroke_bmi, x_axis_variable=smoking,
#                  models=c(case_num~smoking,case_num~smoking+eage+esex+whr,
#                  case_num~smoking+whr,case_num~smoking+eage, case_num~smoking+esex,
#                  case_num~smoking+eage+whr),
#                  regression_type = "unconditional")
#
# is_figure_models(interstroke_data=interstroke_bmi, x_axis_variable=bmi_quintiles,
#                  models=c(case_num~bmi_quintiles,case_num~bmi_quintiles+eage+esex+whr),
#                  regression_type = "unconditional")

#' Interstroke Model ggplot
#'
#' @param interstroke_modelling_data data imported from is figure models function.
#' @param breaks breaks imported from is figure models function.
#' @export
#' @return An Interstroke models ggplot figure
is_figure_models_ggplot <- function(interstroke_modelling_data, breaks = breaks, x_axis_variable = x_axis_variable,
                                    interstroke_data = interstroke_data) {

  #Plot
  interstroke_model_figure <- ggplot(interstroke_modelling_data, aes(term, estimate, ymin = conf.low, ymax = conf.high, group = model)) +
    geom_point(aes(shape = model, colour = model), size = 3.5, position = position_dodge(width=0.30)) +
    geom_linerange(position = position_dodge(width=0.30)) +
    scale_y_continuous(name = "OR (95% CI)",
                       breaks = breaks,
                       trans = "log10", expand = c(0,0)) +
    geom_hline(yintercept = 1, lty = 2) +
    xlab(is_variable_label(interstroke_data = interstroke_data, variable_name = !!x_axis_variable)) +
    ggplot2::scale_colour_viridis_d(end = 0.8) +
    scale_shape_manual(values = c(15:17, 6, 7, 13)) +
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

  return(interstroke_model_figure)

}

# is_figure_models(interstroke_data=interstroke_bmi,
#                  models=c(case_num~smoking,case_num~smoking+eage+esex+whr),
#                  regression_type = "unconditional")

#' Interstroke subtypes Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param model A list of model formulas to include in model figure (see example)
#' @param breaks Manually assign breaks for axis ticks.
#' @param regression_type Either "unconditional" or "conditional". Conditional models require +strata(caseid) at end of model formula.
#' @export
#' @return An Interstroke model figure
#' @examples
#' is_figure_subtypes(interstroke_data=interstroke_bmi, model=case_num ~ bmi_quintiles)
is_figure_subtypes <- function(interstroke_data, model, breaks=NULL, regression_type = NULL) {

  # make a list if needed
  x_axis_variable_extract <- c(model)
  x_axis_variable <- labels(terms(x_axis_variable_extract[[1]]))[1]

  interstroke_data_expr <- rlang::enexpr(interstroke_data)
  x_axis_variable_quo <- rlang::enquo(x_axis_variable)
  x_axis_variable_sym <- rlang::ensym(x_axis_variable)

  #Unconditional
  if(regression_type == "unconditional" | regression_type == "conditional") {
  interstroke_modelling_data_allstroke <- is_logistic_regression(interstroke_data=!!interstroke_data_expr,
                                                                 regression_type = regression_type,
                                                                     models=c(model)) %>%
                                                                     dplyr::mutate(model = "All stroke")

  interstroke_data_ischaemic <<- is_subset(interstroke_data, fstrktype == "Ischemic")

  interstroke_modelling_data_ischaemic <- is_logistic_regression(interstroke_data=interstroke_data_ischaemic,
                                                                 regression_type = regression_type,
                                                                     models=c(model)) %>%
                                                                     dplyr::mutate(model = "Ischaemic")

  interstroke_data_ich <<- is_subset(interstroke_data, fstrktype == "ICH")

  interstroke_modelling_data_ich <- is_logistic_regression(interstroke_data=interstroke_data_ich,
                                                           regression_type = regression_type,
                                                                     models=c(model)) %>%
                                                                     dplyr::mutate(model = "ICH")
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  interstroke_modelling_data <- rbind(interstroke_modelling_data_allstroke, interstroke_modelling_data_ischaemic,
                                      interstroke_modelling_data_ich)

  interstroke_modelling_data <- interstroke_modelling_data %>%
    filter(grepl(x_axis_variable_sym, term)) %>%
    mutate(term = stringr::str_replace(term,as.character(x_axis_variable_sym),""))

  #Model as factor
  interstroke_modelling_data$model <- forcats::as_factor(interstroke_modelling_data$model)

  # # Reference x axis variable ---------------------------
  #Perform stratified analysis for each subgroup
  factor_levels <- interstroke_data %>%
    dplyr::pull(!!x_axis_variable_sym) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  stroke_type <- c("All stroke", "Ischaemic", "ICH")
  for(j in stroke_type){
    interstroke_modelling_data <- interstroke_modelling_data %>%
      tibble::add_row(.before = 1, term = factor_levels[1], estimate = 1.0, conf.low = NA, conf.high = NA,
      model = j)
  }

  interstroke_modelling_data$term <- factor(interstroke_modelling_data$term, levels = factor_levels)

  #X-axis variable assignment
  interstroke_modelling_data <- interstroke_modelling_data %>%
    tibble::add_column(x_axis_variable = is_variable_label(interstroke_data, !!x_axis_variable_quo))

  if(is.null(breaks)){
    breaks = scales::pretty_breaks()
  }

  plot1<-is_figure_models_ggplot(interstroke_modelling_data, breaks = breaks,
                                 x_axis_variable = x_axis_variable_sym, interstroke_data = interstroke_data)

  return(plot1)

}

# is_figure_subtypes(interstroke_data=interstroke_bmi,
#                    model=case_num ~ bmi_quintiles,
#                    regression_type = "unconditional")
# is_figure_subtypes(interstroke_data=interstroke_bmi,
#                    model=case_num ~ smoking,
#                    regression_type = "unconditional")
# is_figure_subtypes(interstroke_data=interstroke_bmi,
#                    model=case_num ~ bmi_quintiles+strata(caseid),
#                    regression_type = "conditional")


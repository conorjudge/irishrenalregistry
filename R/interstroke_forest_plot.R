#' Interstroke stratified analysis for predictor
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param subgroups A list of subgroups for stratified analysis.
#' @param model Model formula for analysis. Predictor should be first independant variable.
#' @param limits **Optional** Limit the x axis in the format c(xmin, xmax).
#' @param p_sig **Optional** Set the level of significance to hide non significant values. Default displays all p values <1.
#' @param breaks **Optional** Set the sequence for breaks in the format seq(xmin, xmax, interval)
#' @param text_size **Optional** Assign value for text size.
#' @param drop_columns **Optional** Drop population and subgroup columns suitable for joint plots.
#' @param long_title **Optional** Set to false to just display predictor as title
#' @param regression_type either "unconditional" or "conditional". Conditional models require +strata(caseid) at end of model formula.
#' @export
#' @return A dataframe suitable for Interstroke forest ggplot function.
#' @examples is_figure_stratified_subgroups(interstroke_data=interstroke_bmi, subgroups=c("esex","smoking","alcohfreqcat","antihypertpre","cardiacrfcat"), model=case_num~whr+esex+smokingregression_type = "unconditional")
is_figure_stratified_subgroups <- function(interstroke_data,
                                           subgroups, model, limits=NULL,
                                           p_sig=NULL, breaks=NULL, text_size=NULL,
                                           drop_columns=NULL, include_overall = TRUE,
                                           long_title = TRUE, regression_type = NULL) {

  # make a list if needed
  predictor_extract <- c(model)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)

  # predictor_sym <- rlang::ensym(predictor)

  #Extract predictor label for plots
  predictor_label <- is_variable_label(interstroke_data=interstroke_data, !!predictor_quo)

  if(long_title == TRUE) {
    title <- paste("The association between", predictor_label,
                   "& stroke stratified by subgroups", sep = " ")
  } else {
    title <- predictor_label
  }

  if(regression_type == "unconditional" | regression_type == "conditional") {
    is_forest_data <- is_logistic_regression_stratified(interstroke_data=interstroke_data,
                                                                      regression_type = regression_type,
                                                                      subgroups=subgroups,
                                                                      model=model,
                                                                      include_overall = include_overall)
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  is_forest_data$subgroup <- forcats::as_factor(is_forest_data$subgroup)

  #Estimate formatted
  is_forest_data <- is_forest_data %>%
    mutate(estimate_formatted = paste0(format(round(estimate,digits=2), nsmall = 2), " (",
                                       format(round(conf.low,digits=2), nsmall = 2),"-",
                                       format(round(conf.high,digits=2), nsmall = 2),")"))

  #P value of significance
  if(is.null(p_sig)){
    p_sig = 1
  }

  # Only show bottom p result per subgroup
  p_id <- which(is_forest_data$subgroup[-1]
                != is_forest_data$subgroup
                [-length(is_forest_data$subgroup)])

  p_id <- append(p_id, length(is_forest_data$p_interaction), after = length(p_id))

  is_forest_data$p_interaction <- replace(is_forest_data$p_interaction, -p_id, NA)

  #Prep p values to show only if significant
  is_forest_data <- is_forest_data %>%
    mutate(p_interaction = ifelse(p_interaction > p_sig, NA, p_interaction))

  is_forest_data$p_interaction <- ifelse(is.na(is_forest_data$p_interaction) == TRUE, NA,
                                         paste0("p=", is_forest_data$p_interaction))

  #Cosmetic subgroups
  subgroup_id <- which(c(FALSE, tail(is_forest_data$subgroup,-1) != head(is_forest_data$subgroup,-1)))

  subgroup_id <- append(subgroup_id, 1, after = 0)

  is_forest_data$subgroup <- replace(is_forest_data$subgroup, -subgroup_id, NA)

  #Add cosmetic gap of NA's before every subgroup name
  sub_names <- is_forest_data %>% drop_na(subgroup) %>%
    select(subgroup) %>% pull()

  for(i in sub_names){
    addrow_id <- which(is_forest_data$subgroup == i)
    is_forest_data <- is_forest_data %>%
      tibble::add_row(.before = addrow_id)
  }

  #Make a blank row for table headers before rownumber to allow the rest of data to match
  is_forest_data <- is_forest_data %>%
    tibble::add_row(.before = 1)

  #Assign row number for positioning
  is_forest_data$rownumber = as.numeric(nrow(is_forest_data):1)

  #Set limits if not specified in function call
  if(is.null(limits)){
    limits <- c(min(as.numeric(is_forest_data$conf.low) - 0.05, na.rm = TRUE),
                max(as.numeric(is_forest_data$conf.high) + 0.2, na.rm = TRUE))
  }

  if(is.null(breaks)){
    breaks = scales::pretty_breaks()
  }

  plot1 <- is_figure_forest_ggplot(is_forest_data, breaks = breaks,
                                   title = title,
                                   long_title = long_title)

  return(plot1)

}

# is_figure_stratified_subgroups(interstroke_data = interstroke_pollution,
#                                         subgroups = c("esex","smoking","antihypertpre","cardiacrfcat"),
#                                         model = case_num~sysbp,
#                                         regression_type = "unconditional")
#
# is_figure_stratified_subgroups(interstroke_data = interstroke_pollution,
#                                subgroups = c("esex","smoking","antihypertpre","cardiacrfcat"),
#                                model = case_num~sysbp+strata(caseid),
#                                regression_type = "conditional")


#' Interstroke forest ggplot
#'
#' @param is_forest_data Data input from is_forest_figure
#' @param breaks manually assign break values for axis.
#' @param limits manually assign plot axis limits.
#' @param text_size **Optional** Assign value for text size.
#' @param title Character string for title of plot.
#' @param long_title default TRUE gives a descriptive title. Set to false for just the predictor as title.
#' @export
#' @return An Interstroke models ggplot figure
# Plot
is_figure_forest_ggplot <- function(is_forest_data, breaks = breaks, limits = NULL,
                                    text_size=NULL, title, long_title = long_title) {

  #P value of significance
  if(is.null(limits)){
    limits <- c(min(as.numeric(is_forest_data$conf.low) - 0.05, na.rm = TRUE),
                max(as.numeric(is_forest_data$conf.high) + 0.2, na.rm = TRUE))
  }

  #Parameter for text size
  if(is.null(text_size)){
    text_size <- 4
  }

  g1 = ggplot(is_forest_data, aes(x = as.numeric(estimate), xmin = as.numeric(conf.low), xmax  = as.numeric(conf.high),
                                  y = rownumber))+
    annotate("text", x = (as.numeric(max(is_forest_data$conf.high, na.rm = TRUE)) + 0.01), #Display p values
             y = is_forest_data$rownumber,
             label=is_forest_data$p_interaction, hjust=0, size=text_size)+
    geom_point(aes(size = n), shape=22, fill="darkblue")+
    geom_errorbarh(height=0.2) +
    geom_segment(aes(x = 1, xend=1, y = -Inf, yend=max(is_forest_data$rownumber - 1))) +
    scale_x_continuous(trans="log10", breaks= breaks, limits = limits,
                       expand = c(0,0))+
    xlab("Odds ratio (95% CI, log scale)")+
    ggplot2::coord_cartesian(clip = 'off') + #Allow text outside plotting window
    theme_classic()+
    theme(axis.title.x = element_text(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position="none",
          plot.margin = ggplot2::margin(0, 2, 0, 0, "cm"))

  #Text table
  #Assign spacing for 4 columns
  column_space <- c(-0.5, 0.1, 0.3, 0.7)

  t1 = ggplot(is_forest_data, aes(x = as.numeric(estimate), y = rownumber))+
    annotate("text", x = column_space[1], y = is_forest_data$rownumber, label=is_forest_data$subgroup, hjust=0, size=text_size)+
    annotate("text", x = column_space[2], y = is_forest_data$rownumber, label=is_forest_data$subgroup_level, hjust=1, size=text_size)+
    annotate("text", x = column_space[3], y = is_forest_data$rownumber, label=is_forest_data$n, hjust=1, size=text_size)+
    annotate("text", x = column_space[4], y = is_forest_data$rownumber, label=is_forest_data$estimate_formatted, hjust=1, size=text_size)+
    annotate("text", x = column_space[1], y = max(is_forest_data$rownumber), label="Population", hjust=0, size=text_size, fontface =2)+
    annotate("text", x = column_space[2], y = max(is_forest_data$rownumber), label="Subgroup", hjust=1, size=text_size, fontface =2)+
    annotate("text", x = column_space[3], y = max(is_forest_data$rownumber), label="N", hjust=1, size=text_size, fontface =2)+
    annotate("text", x = column_space[4], y = max(is_forest_data$rownumber), label="OR (95% CI)", hjust=1, size=text_size, fontface =2)+
    theme_classic()+
    theme(axis.title.x = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          line = element_blank())


  if(long_title == TRUE) {
  is_forest_ggplot <- gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
                                              top=grid::textGrob(title, x=0.4, y=0.2))
  } else {
  is_forest_ggplot <- gridExtra::grid.arrange(t1, g1, ncol=2, widths = c(3,2),
                                                top=grid::textGrob(title, x=0.1, y=0.2))
  }

  return(is_forest_ggplot)
}

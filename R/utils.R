#' Subset Interstroke and keep case/control match
#'
#' @param interstroke_data An Interstroke Dataset
#' @param subset_rule A subsetting rule e.g. "age <= 45"
#' @export
#' @return An Interstroke Dataset
#' @examples
#' is_subset(interstroke_data = interstroke_stress, subset_rule = eage <= 45)
is_subset <- function(interstroke_data, subset_rule) {

  subset_rule_quo <- rlang::enquo(subset_rule)

  interstroke_case <- interstroke_data %>%
    filter(case=="Case") %>%
    filter(!!subset_rule_quo)

  interstroke_control <- interstroke_data %>%
    filter(case=="Control") %>%
    filter(caseid %in% interstroke_case$id)

  return(sjlabelled::copy_labels(rbind(interstroke_case,interstroke_control),interstroke_data))

}

#' Extract variable name label
#'
#' @param interstroke_data An Interstroke Dataset
#' @param variable_name A variable name to extract the label for
#' @export
#' @return An variable label string
#' @examples
#' is_variable_label(interstroke_data = interstroke_bmi, variable_name = bmi)
is_variable_label <- function(interstroke_data, variable_name) {

  variable_name_quo <- rlang::enquo(variable_name)

  interstroke_data_variable_name <- interstroke_data %>%
    select(!!variable_name_quo)

  variable_label <- labelled::var_label(interstroke_data_variable_name)

  return(variable_label[[1]])

}

#' Create character string of model
#'
#' @param interstroke_data interstroke dataset
#' @param model_formula A formula to extract the label for
#' @export
#' @return An Interstroke Dataset
#' @examples
#' is_model_label(interstroke_data = interstroke_bmi, model_formula = case_num ~ cookplace+eage+esex)
#' is_model_label(interstroke_data = interstroke_bmi, model_formula = case_num ~ cookplace+eage+esex+strata(caseid))
is_model_label <- function(interstroke_data, model_formula) {

  test_model <- formula(model_formula)

  test_model_l <- length(test_model)

  formula_vars <- all.vars(test_model[-2])

  n_formula_vars <- length(formula_vars)

  if(formula_vars[n_formula_vars] == "caseid"){
    formula_vars <- formula_vars[-n_formula_vars]
  }

  for(i in 1:n_formula_vars){
    tmp <- formula_vars[i]
    if(tmp %in% colnames(interstroke_bmi)){
      formula_vars <- stringr::str_replace(formula_vars, tmp, is_variable_label(interstroke_bmi, tmp))
    }
  }

  formula_length <- length(formula_vars)

  formula_vars <- gsub("\\,.*", "", formula_vars)

  ## Conditional
  if(grepl("strata", test_model[test_model_l], fixed=TRUE)){
    if(formula_length == 1) {
      formula_vars <- "Univariate (conditional)"
    } else if(formula_length == 2){
      formula_vars <- paste("Adjusted for", tolower(toString(formula_vars[formula_length])), "(conditional)")
    } else {
      formula_vars <- paste("Adjusted for", tolower(toString(formula_vars[-c(1, formula_length)])),
                            "and", tolower(toString(formula_vars[formula_length])), "(conditional)")
    }
  } else {
    if(formula_length == 1) {
      formula_vars <- "Univariate (unconditional)"
    } else if(formula_length == 2){
      formula_vars <- paste("Adjusted for", tolower(toString(formula_vars[formula_length])), "(unconditional)")
    } else {
      formula_vars <- paste("Adjusted for", tolower(toString(formula_vars[-c(1, formula_length)])),
                            "and", tolower(toString(formula_vars[formula_length])), "(unconditional)")
    }
  }

  return(formula_vars)

}

#' Interstroke Logistic Regression
#'
#' @param interstroke_data An Interstroke Dataset
#' @param regression_type either "unconditional" or "conditional". Conditional models require +strata(caseid) at end of model formula.
#' @param models A list of models. Format = c(case_num~predictor+optional_covariate)
#' @export
#' @return A dataframe with model output
#' @examples
#' is_logistic_regression(interstroke_data=interstroke_bmi, regression_type = "unconditional", models=c(case_num~whr_z,case_num~whr_z+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp,case_num~whr_z+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+incomectry))
#' is_logistic_regression(interstroke_data=interstroke_pollution, regression_type = "conditional", models=c(case_num~cookplace+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry+strata(caseid)))
is_logistic_regression <- function(interstroke_data, regression_type, models){

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  for (i in seq_along(models)) {
    formula_i = models[[i]]

    formula_i_l <- length(formula_i)

    formula_label <- is_model_label(interstroke_data = !!interstroke_data_expr, model_formula = formula_i)

    if(regression_type == "unconditional") {
      # stopifnot(!grepl("strata", formula_i[formula_i_l], fixed = TRUE))
      if (grepl("strata", formula_i[formula_i_l], fixed = TRUE)) {
        print("Strata found in model formula. Remove strata or use regression_type = 'conditional'")
        stop()
      }

      lm_call <- rlang::expr(glm(formula_i, data = !!interstroke_data_expr, family="binomial"))
    } else if(regression_type == "conditional") {
      # stopifnot(grepl("strata", formula_i[formula_i_l], fixed = TRUE))
      if (!grepl("strata", formula_i[formula_i_l], fixed = TRUE)) {
        print("Strata not found in model formula. Add strata as covariate or use regression_type = 'unconditional'")
        stop()
      }
      lm_call <- rlang::expr(clogit(formula_i, data = !!interstroke_data_expr))
    } else {
      stop("regression type must be either unconditional or conditional")
    }


    model_tidy_text <- broom::tidy(rlang::eval_tidy(lm_call),exponentiate=TRUE,conf.int=TRUE) %>%
      select(term,estimate,conf.low,conf.high) %>%
      tibble::add_column(model=formula_label) %>%
      tibble::add_column(model_id=paste("Model", i))

    if(i==1) {
      final_table <- model_tidy_text
    } else {
      final_table <- final_table %>%
        bind_rows(model_tidy_text)
    }

  }

  return(final_table)
}

#' Interstroke Logistic Regression Stratified
#'
#' @param interstroke_data An Interstroke Dataset
#' @param regression_type "unconditional" or "conditional". Include +strata(caseid) in model formula for conditional analysis.
#' @param subgroups A list of subgroups for stratified analysis.
#' @param model A list of models for table 2
#' @param include_overall default is TRUE. Set to FALSE to exclude overall analysis.
#' @export
#' @return A dataframe with model output
#' @examples
#' is_logistic_regression_stratified(interstroke_data=interstroke_bmi, regression_type = "unconditional", subgroups=c("esex","smoking"), model=case_num~whr_z+eage)
#' is_logistic_regression_stratified(interstroke_data=interstroke_bmi, regression_type = "conditional", subgroups=c("esex","smoking"), model=case_num~bmi_z+eage+strata(caseid),include_overall = FALSE)
is_logistic_regression_stratified <- function(interstroke_data, regression_type, subgroups, model, include_overall = TRUE) {

  predictor_extract <- c(model)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)
  predictor_ensym <- rlang::ensym(predictor)

  final_table <- data.frame(model=character(),
                            term=character(),
                            estimate=double(),
                            conf.low=double(),
                            conf.high=double(),
                            subgroup=character(),
                            subgroup_level=character())

  for (i in seq_along(subgroups)) {
    subgroup = subgroups[[i]]

    #Perform stratified analysis for each subgroup
    factor_levels <- interstroke_data %>%
      dplyr::pull(subgroup) %>%
      forcats::fct_drop(.) %>%
      forcats::fct_unique(.) %>%
      as.character()

    #Perform stratified analysis for level of factor
    for (factor_level in factor_levels) {
      #Subgroup interstroke_data by factor level
      data_subgroup <<- interstroke_data %>%
        filter(!!sym(subgroup) == factor_level)
      subgroup_ensym <- rlang::ensym(subgroup)

      if(regression_type == "unconditional" | regression_type == "conditional") {
        model_tidy_text <- is_logistic_regression(interstroke_data=data_subgroup,
                                                  regression_type = regression_type,
                                                  models=c(model)) %>%
          tibble::add_column(subgroup=is_variable_label(interstroke_data, subgroup)) %>%
          tibble::add_column(subgroup_level=factor_level) %>%
          tibble::add_column(n=nrow(data_subgroup)) %>%
          tibble::add_column(p_interaction=is_tb_interaction(interstroke_data = interstroke_data,
                                                             regression_type = regression_type,
                                                             predictor = !!predictor_ensym,
                                                             interaction_variable = !!subgroup_ensym)) %>%
          filter(term==predictor_ensym)
      } else {
        stop("regression type must be either unconditional or conditional")
      }


      final_table <- final_table %>%
        bind_rows(model_tidy_text)

    }

  }
  if(include_overall == TRUE){
    model_tidy_text_overall <- is_logistic_regression(interstroke_data=!!interstroke_data,
                                                      regression_type = regression_type,
                                                      models=c(model)) %>%
      tibble::add_column(n=nrow(interstroke_data))%>%
      tibble::add_column(subgroup="Overall")%>%
      filter(term==predictor_ensym)

    final_table <- model_tidy_text_overall %>%
      bind_rows(final_table)
  }

  # P value rounding
  final_table <- final_table %>%
    mutate(across(p_interaction, as.numeric)) %>%
    mutate_at(vars(p_interaction), funs(round(., 3))) # last value determines decimel places

  final_table$p_interaction <- format(round(final_table$p_interaction,3), nsmall = 3)


  return(final_table)

}

#' is_tb_prevalence A function for adding prevalence to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @export
#' @return A tibble containing a count of Controls, Ischemic and ICH (strktype) at each level of a predictor
#' @examples
#' is_tb_prevalence(interstroke_pollution,polluteh)
is_tb_prevalence <- function(interstroke_data,predictor){

  predictor_sym <- rlang::ensym(predictor)
  predictor_label <- is_variable_label(interstroke_data = interstroke_data, variable_name = !!predictor_sym)

  interstroke_data %>%
    filter(!is.na({{predictor}})) %>%
    group_by(fstrktype) %>%
    count({{predictor}}) %>%
    tidyr::pivot_wider(names_from=fstrktype,values_from = n) %>%
    select(risk_factor={{predictor}},Controls,Ischemic,ICH) %>%
    add_row(risk_factor=predictor_label,.before=0)
}

#' is_tb_univariate A function for adding univariate analysis to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @param regression_type "unconditional" or "conditional"
#' @export
#' @return A tibble containing a count of Controls, Ischemic and ICH (strktype) at each level of a predictor
#' @examples
#' is_tb_univariate(interstroke_pollution,polluteh, regression_type = "unconditional")
#' is_tb_univariate(interstroke_pollution,polluteh, regression_type = "conditional")
is_tb_univariate <- function(interstroke_data,predictor,regression_type = NULL,env = rlang::caller_env()){

  predictor_sym <- rlang::ensym(predictor)
  predictor_label <- is_variable_label(interstroke_data = interstroke_data, variable_name = !!predictor_sym)

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  factor_levels <- interstroke_data %>%
    dplyr::pull(!!predictor_sym) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  if(regression_type == "unconditional") {
    univariate_call <- expr(glm(case_num ~ !!predictor_sym, data = !!interstroke_data_expr, family="binomial"))
  } else if(regression_type == "conditional") {
    univariate_call <- expr(clogit(case_num ~ !!predictor_sym + strata(caseid), data = !!interstroke_data_expr))
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  broom::tidy(eval(univariate_call, env),exponentiate=TRUE,conf.int=TRUE) %>%
    mutate(`Odds Ratio (95% CI)` = paste0(round(estimate,2)," (",round(conf.low,2),"-",round(conf.high,2),")"))%>%
    filter(term!="(Intercept)") %>%
    select(risk_factor=term,`Odds Ratio (95% CI)`) %>%
    add_row(risk_factor=factor_levels[1],`Odds Ratio (95% CI)`="1.0",.before = 1) %>%
    mutate_if(is.character,stringr::str_replace_all, pattern = predictor_label, replacement = "") %>%
    select(risk_factor,`Odds Ratio (95% CI)`)

}

#' is_tb_interaction A function for adding analysis P for interaction to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @param interaction_variable The interaction variable
#' @param regression_type "unconditional" or "conditional"
#' @export
#' @return The P for interaction
#' @examples
#' is_tb_interaction(interstroke_pollution,polluteh,esex, "unconditional")
#' is_tb_interaction(interstroke_pollution,polluteh,esex, "conditional")
is_tb_interaction <- function(interstroke_data, predictor, interaction_variable, regression_type, env = rlang::caller_env()){

  interstroke_data_expr <- enexpr(interstroke_data)
  predictor_expr <- enexpr(predictor)
  interaction_variable_expr <- enexpr(interaction_variable)

  if(regression_type == "unconditional") {
    model_without_interaction_call <- expr(glm(case_num ~ !!predictor_expr + !!interaction_variable_expr, data = !!interstroke_data_expr, family="binomial"))
    model_without_interaction <- eval(model_without_interaction_call, env)
    model_with_interaction_call <- expr(glm(case_num ~ !!predictor_expr*!!interaction_variable_expr, data = !!interstroke_data_expr, family="binomial"))
    model_with_interaction <- eval(model_with_interaction_call, env)
  } else if(regression_type == "conditional") {
    model_without_interaction_call <- expr(clogit(case_num ~ !!predictor_expr + !!interaction_variable_expr + strata(caseid), data = !!interstroke_data_expr))
    model_without_interaction <- eval(model_without_interaction_call, env)
    model_with_interaction_call <- expr(clogit(case_num ~ !!predictor_expr*!!interaction_variable_expr + strata(caseid), data = !!interstroke_data_expr))
    model_with_interaction <- eval(model_with_interaction_call, env)
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  return(format(broom::tidy(anova(model_without_interaction,model_with_interaction,test='Chisq'))$p.value[2],digits=2))
}

#' Interstroke Table one
#'
#' @param interstroke_data An Interstroke Dataset
#' @param model Standard R format formula e.g. predictor ~ covariate1 + covariate2
#' @param table_title Table title
#' @export
#' @return A table one flextable
#' @examples
#' is_table_one(interstroke_data=interstroke_pollution, model = polluteh~eage+bmi+sysbp, table_title="Table 1 - Baseline Characteristics by pollution level")
is_table_one <- function(interstroke_data, model, table_title = NULL, env = rlang::caller_env()) {

  vars.ex = all.vars(model)
  rhs = vars.ex[-1]
  resp   = vars.ex[1]

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  call <- rlang::expr(!!interstroke_data_expr %>%
    select(resp,rhs))

  final_table <- eval(call, environment())

  final_table_ft <- final_table %>%
      gtsummary::tbl_summary(
        by = resp,
        missing = "no",
        statistic = gtsummary::all_continuous() ~ "{mean} ({sd})"
      ) %>%
      gtsummary::modify_header(stat_by = gt::md("**{level}** \n N =  {n} ({style_percent(p)}%)")) %>%
      gtsummary::bold_labels() %>%
      gtsummary::add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = 2)) %>%
      gtsummary::as_flex_table() %>%
      flextable::line_spacing(space = 0.7, part = "all") %>%
      flextable::fontsize(size = 8) %>%
      flextable::fontsize(size = 8, part="header") %>%
      flextable::set_table_properties(layout = "autofit")

  return(final_table_ft)
}

#' Interstroke Table two
#'
#' @param interstroke_data An Interstroke Dataset
#' @param models A list of models for table 2
#' @param regression_type Type of regression, either conditional or unconditional
#' @param table_title Table title
#' @export
#' @return A table two flextable
#' @examples
#' is_table_two(interstroke_data=interstroke_pollution, regression_type="unconditional", models=c(case_num~cookplace,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry))
#' is_table_two(interstroke_data=interstroke_pollution, regression_type="conditional", models=c(case_num~cookplace+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry+strata(caseid)),table_title="Custom defined title using table_title")
is_table_two <- function(interstroke_data, models, regression_type=NULL, table_title = NULL) {

  interstroke_data_expr <- rlang::enexpr(interstroke_data)
  predictor_extract <- c(models)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)

  predictor_label <- is_variable_label(interstroke_data=interstroke_data, !!predictor_quo)

  predictor_label <- tolower(predictor_label)

  if(regression_type == "unconditional" | regression_type == "conditional") {
    final_table <- is_logistic_regression(interstroke_data = !!interstroke_data_expr,
                                          regression_type = regression_type, models = models)
  } else {
    stop("regression type must be either unconditional or conditional")
  }

  #Flip table around
  final_table <- final_table %>%
    filter(grepl(!!predictor_quo, term)) %>%
    mutate(estimate_formatted = paste0(format(round(estimate,2), nsmall = 2)," (",
                                       format(round(conf.low,2), nsmall = 2),"-",
                                       format(round(conf.high,2), nsmall = 2),")")) %>%
    select(model_id, term, estimate_formatted) %>%
    tidyr::pivot_wider(names_from=term, values_from=estimate_formatted)
  # mutate_if(is.character,stringr::str_replace_all, pattern = predictor_quo, replacement = "") #safe to replace?

  #stringr::str_replace_all(string = term, pattern = !!predictor_quo, replacement = "")

  factor_levels <- interstroke_data %>%
    dplyr::pull(!!predictor_quo) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  reference_factor <- factor_levels[1]

  #Add reference column to table and change the column name
  final_table <- final_table %>%
    dplyr::mutate(!!reference_factor := format(round(1, 2), nsmall = 2), .after = "model_id")

  final_table <- rename(final_table, "Model" = model_id)

  # Remove predictor from start of column names
  names(final_table) <- gsub(x = names(final_table), pattern = predictor, replacement = "")

  #Flextable
  final_table <- final_table %>%
    flextable::flextable() %>%
    flextable::autofit()

  model_footnotes <- NULL

  for(j in models){
    model_footnotes_j <- is_model_label(interstroke_data, model_formula = j)
    model_footnotes <- rbind(model_footnotes, model_footnotes_j)
  }

  final_table <- flextable::footnote(final_table, j = 1,
                                     value = flextable::as_paragraph(
                                       model_footnotes
                                     ),
                                     ref_symbols = 1:length(models),
                                     inline = TRUE, part = "body")

  if(is.null(table_title)){
    final_table <- flextable::add_header_lines(final_table,
                                               values = paste("The association of", predictor_label,
                                                              "with stroke", sep = " "),
                                               top = TRUE)
  } else {
    final_table <- flextable::add_header_lines(final_table, values = table_title, top = TRUE)
  }
  final_table <- flextable::valign(final_table, valign = "bottom", part = "header")
  final_table <- flextable::autofit(final_table)

  return(final_table)

}

#' Interstroke Restricted Cubic Splines
#'
#' @param interstroke_data An Interstroke Dataset
#' @param model Formula for model e.g. case_num ~ rcs(trafhmkm,3)
#' @export
#' @return A restricted cubic spline plot
#' @examples
#' is_spline(interstroke_bmi, case_num ~ whr_z)
is_spline <- function(interstroke_data, model, env = rlang::caller_env()) {

  model_expr <- rlang::enexpr(model)
  interstroke_data_quo <- rlang::enquo(interstroke_data)

  predictor_extract <- c(model)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)

  datadist_call <- expr(rms::datadist(!!interstroke_data_quo))
  dd <- eval(datadist_call, env = environment())
  options(datadist="dd")

  lm_call <- expr(rms::lrm(!!model_expr, data = !!interstroke_data_quo))
  model_formula <- eval(lm_call, env = environment())

  dataplot_call <- expr(Predict(model_formula,!!predictor_quo, ref.zero = TRUE, fun=exp))
  dataplot <- eval(dataplot_call, env = environment())

  plot <- ggplot(dataplot,aes(!!predictor_quo, yhat)) +
    theme_classic() +
    scale_y_continuous("Odds Ratio (95% CI)")+
    geom_hline(yintercept =1, linetype="dashed")

  return(plot)
}

#' Interstroke Conditonal Logistic Regression
#'
#' @param interstroke_data An Interstroke Dataset
#' @param models A list of models
#' @export
#' @return A dataframe with model output
#' @examples
#' is_conditional_logistic_regression(interstroke_data=interstroke_pollution,models=c(case_num~cookplace+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry+strata(caseid)))
is_conditional_logistic_regression <- function(interstroke_data, models){

  warning("is_conditional_logistic_regression is been replaced by is_logistic_regression. Provide regression_type as argument")
  print("is_conditional_logistic_regression is been replaced by is_logistic_regression. Provide regression_type as argument")

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  for (i in seq_along(models)) {
    formula = models[[i]]

    formula_label <- is_model_label(interstroke_data = !!interstroke_data_expr, model_formula = formula)

    lm_call <- rlang::expr(clogit(formula, data = !!interstroke_data_expr))

    formula_label <- is_model_label(interstroke_data = interstroke_data, model_formula = formula)


    model_tidy_text <- broom::tidy(eval(lm_call, env=environment()),exponentiate=TRUE,conf.int=TRUE) %>%
      select(term,estimate,conf.low,conf.high) %>%
      tibble::add_column(model=formula_label)

    if(i==1) {
      final_table <- model_tidy_text
    } else {
      final_table <- final_table %>%
        bind_rows(model_tidy_text)
    }

  }

  return(final_table)
}

# is_conditional_logistic_regression(interstroke_data=interstroke_pollution,models=c(case_num~cookplace+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+strata(caseid),case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry+strata(caseid)))

#' Interstroke Unconditonal Logistic Regression
#'
#' @param interstroke_data An Interstroke Dataset
#' @param models A list of models for table 2
#' @export
#' @return A dataframe with model output
#' @examples
#' is_unconditional_logistic_regression(interstroke_data=interstroke_pollution,models=c(case_num~cookplace,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry))
is_unconditional_logistic_regression <- function(interstroke_data, models){

  warning("is_unconditional_logistic_regression is been replaced by is_logistic_regression. Provide regression_type as argument")
  print("is_unconditional_logistic_regression is been replaced by is_logistic_regression. Provide regression_type as argument")

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  for (i in seq_along(models)) {
    formula = models[[i]]

    formula_label <- is_model_label(interstroke_data = !!interstroke_data_expr, model_formula = formula)

    lm_call <- rlang::expr(glm(!!formula, data = !!interstroke_data_expr, family="binomial"))

    model_tidy_text <- broom::tidy(rlang::eval_tidy(lm_call),exponentiate=TRUE,conf.int=TRUE) %>%
      tibble::add_column(model=formula_label) %>%
      select(model,term,estimate,conf.low,conf.high)

    if(i==1) {
      final_table <- model_tidy_text
    } else {
      final_table <- final_table %>%
        bind_rows(model_tidy_text)
    }

  }

  return(final_table)
}

# is_unconditional_logistic_regression(interstroke_data=interstroke_bmi,
#                                     models=c(case_num~bmi_quintiles,case_num~bmi_quintiles+eage,case_num~bmi_quintiles+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp,case_num~bmi_quintiles+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+incomectry))
# is_unconditional_logistic_regression(interstroke_data=interstroke_bmi,
#                                      models=c(case_num~bmi,case_num~bmi+eage,case_num~bmi+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp,case_num~bmi+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+incomectry))

#' is_tb_univariate_conditonal A function for adding univariate conditonal analysis to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @export
#' @return A tibble containing a count of Controls, Ischemic and ICH (strktype) at each level of a predictor
#' @examples
#' is_tb_univariate_conditonal(interstroke_pollution,polluteh)
is_tb_univariate_conditonal <- function(interstroke_data, predictor, env = rlang::caller_env()){

  warning("is_tb_univariate_conditonal is been replaced by is_tb_univariate. Provide regression_type as argument")
  print("is_tb_univariate_conditonal is been replaced by is_tb_univariate. Provide regression_type as argument")

  predictor_sym <- rlang::ensym(predictor)
  predictor_label <- is_variable_label(interstroke_data = interstroke_data, variable_name = !!predictor_sym)

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  # reference level code
  factor_levels <- interstroke_data %>%
    dplyr::pull(!!predictor_sym) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  clogit_call <- expr(clogit(case_num ~ !!predictor_sym + strata(caseid), data = !!interstroke_data_expr))

  broom::tidy(eval(clogit_call, env),exponentiate=TRUE,conf.int=TRUE) %>%
    mutate(`Odds Ratio (95% CI)` = paste0(round(estimate,2)," (",round(conf.low,2),"-",round(conf.high,2),")"))%>%
    filter(term!="(Intercept)") %>%
    select(risk_factor=term,`Odds Ratio (95% CI)`) %>%
    add_row(risk_factor=factor_levels[1],`Odds Ratio (95% CI)`="1.0",.before = 1) %>%
    mutate_if(is.character,stringr::str_replace_all, pattern = predictor_label, replacement = "") %>%
    select(risk_factor,`Odds Ratio (95% CI)`)

}

# is_tb_univariate_conditonal(interstroke_pollution,polluteh)

#' is_tb_univariate_unconditonal A function for adding univariate unconditonal analysis to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @export
#' @return A tibble containing a count of Controls, Ischemic and ICH (strktype) at each level of a predictor
#' @examples
#' is_tb_univariate_unconditonal(interstroke_pollution,polluteh)
is_tb_univariate_unconditonal <- function(interstroke_data,predictor,predictor_name,env = rlang::caller_env()){

  warning("is_tb_univariate_unconditonal is been replaced by is_tb_univariate. Provide regression_type as argument")
  print("is_tb_univariate_unconditonal is been replaced by is_tb_univariate. Provide regression_type as argument")

  predictor_sym <- rlang::ensym(predictor)
  predictor_label <- is_variable_label(interstroke_data = interstroke_data, variable_name = !!predictor_sym)

  interstroke_data_expr <- rlang::enexpr(interstroke_data)

  # reference level code
  #Perform stratified analysis for each subgroup
  factor_levels <- interstroke_data %>%
    dplyr::pull(!!predictor_sym) %>%
    forcats::fct_drop(.) %>%
    forcats::fct_unique(.) %>%
    as.character()

  clogit_call <- expr(glm(case_num ~ !!predictor_sym, data = !!interstroke_data_expr, family="binomial"))

  broom::tidy(eval(clogit_call, env),exponentiate=TRUE,conf.int=TRUE) %>%
    mutate(`Odds Ratio (95% CI)` = paste0(round(estimate,2)," (",round(conf.low,2),"-",round(conf.high,2),")"))%>%
    filter(term!="(Intercept)") %>%
    select(risk_factor=term,`Odds Ratio (95% CI)`) %>%
    add_row(risk_factor=factor_levels[1],`Odds Ratio (95% CI)`="1.0",.before = 1) %>%
    mutate_if(is.character,stringr::str_replace_all, pattern = predictor_label, replacement = "") %>%
    select(risk_factor,`Odds Ratio (95% CI)`)

}

# is_tb_univariate_unconditonal(interstroke_pollution,polluteh)

#' is_tb_conditional_interaction A function for adding conditonal analysis P for interaction to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @param interaction_variable The interaction variable
#' @export
#' @return The P for interaction
#' @examples
#' is_tb_conditional_interaction(interstroke_pollution,polluteh,esex)
is_tb_conditional_interaction <- function(interstroke_data, predictor, interaction_variable, env = rlang::caller_env()){

  warning("is_tb_conditional_interaction is been replaced by is_tb_interaction. Provide regression_type as argument")
  print("is_tb_conditional_interaction is been replaced by is_tb_interaction. Provide regression_type as argument")

  interstroke_data_expr <- enexpr(interstroke_data)
  predictor_expr <- enexpr(predictor)
  interaction_variable_expr <- enexpr(interaction_variable)

  model_without_interaction_call <- expr(clogit(case_num ~ !!predictor_expr + !!interaction_variable_expr + strata(caseid), data = !!interstroke_data_expr))
  model_without_interaction <- eval(model_without_interaction_call, env)

  model_with_interaction_call <- expr(clogit(case_num ~ !!predictor_expr*!!interaction_variable_expr + strata(caseid), data = !!interstroke_data_expr))
  model_with_interaction <- eval(model_with_interaction_call, env)

  return(format(broom::tidy(anova(model_without_interaction,model_with_interaction,test='Chisq'))$p.value[2],digits=2))
}

# is_tb_conditional_interaction(interstroke_pollution,polluteh,esex)

#' is_tb_unconditional_interaction A function for adding unconditonal analysis P for interaction to Interstroke tables
#'
#' @param interstroke_data An Interstroke Dataset
#' @param predictor The predictor variable
#' @param interaction_variable The interaction variable
#' @export
#' @return The P for interaction
#' @examples
#' is_tb_unconditional_interaction(interstroke_pollution,polluteh,esex)
is_tb_unconditional_interaction <- function(interstroke_data, predictor, interaction_variable, env = rlang::caller_env()){

  warning("is_tb_unconditional_interaction is been replaced by is_tb_interaction. Provide regression_type as argument")
  print("is_tb_unconditional_interaction is been replaced by is_tb_interaction. Provide regression_type as argument")

  interstroke_data_expr <- enexpr(interstroke_data)
  predictor_expr <- enexpr(predictor)
  interaction_variable_expr <- enexpr(interaction_variable)

  model_without_interaction_call <- expr(glm(case_num ~ !!predictor_expr + !!interaction_variable_expr, data = !!interstroke_data_expr, family="binomial"))
  model_without_interaction <- eval(model_without_interaction_call, env)

  model_with_interaction_call <- expr(glm(case_num ~ !!predictor_expr*!!interaction_variable_expr, data = !!interstroke_data_expr, family="binomial"))

  model_with_interaction <- eval(model_with_interaction_call, env)

  return(format(broom::tidy(anova(model_without_interaction,model_with_interaction,test='Chisq'))$p.value[2],digits=2))
}

# is_tb_unconditional_interaction(interstroke_pollution,polluteh,esex)

#' Interstroke Unconditonal Logistic Regression Stratified
#'
#' @param interstroke_data An Interstroke Dataset
#' @param subgroups A list of subgroups for stratified analysis.
#' @param model A list of models for table 2
#' @param include_overall default is TRUE. Set to FALSE to exclude overall analysis.
#' @export
#' @return A dataframe with model output
#' @examples
#' is_unconditional_logistic_regression_stratified(interstroke_data=interstroke_bmi,subgroups=c("esex","smoking"),model=case_num~bmi+esex+eage)
is_unconditional_logistic_regression_stratified <- function(interstroke_data, subgroups, model, include_overall = TRUE) {

  warning("is_unconditional_logistic_regression_stratified is been replaced by is_logistic_regression_stratified. Provide regression_type as argument")
  print("is_unconditional_logistic_regression_stratified is been replaced by is_logistic_regression_stratified. Provide regression_type as argument")

  predictor_extract <- c(model)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)
  predictor_ensym <- rlang::ensym(predictor)

  final_table <- data.frame(model=character(),
                            term=character(),
                            estimate=double(),
                            conf.low=double(),
                            conf.high=double(),
                            subgroup=character(),
                            subgroup_level=character())

  for (i in seq_along(subgroups)) {
    subgroup = subgroups[[i]]

    #Perform stratified analysis for each subgroup
    factor_levels <- interstroke_data %>%
      dplyr::pull(subgroup) %>%
      forcats::fct_drop(.) %>%
      forcats::fct_unique(.) %>%
      as.character()

    #Perform stratified analysis for level of factor
    for (factor_level in factor_levels) {
      #Subgroup interstroke_data by factor level
      data_subgroup <<- interstroke_data %>%
        filter(!!sym(subgroup) == factor_level)

      subgroup_ensym <- rlang::ensym(subgroup)
      model_tidy_text <- is_unconditional_logistic_regression(interstroke_data=data_subgroup,
                                           models=c(model)) %>%
        tibble::add_column(subgroup=is_variable_label(interstroke_data, subgroup)) %>%
        tibble::add_column(subgroup_level=factor_level) %>%
        tibble::add_column(n=nrow(data_subgroup)) %>%
        tibble::add_column(p_interaction=is_tb_unconditional_interaction(interstroke_data = interstroke_data,
                                                                         predictor = !!predictor_ensym,
                                                                         interaction_variable = !!subgroup_ensym)) %>%
        filter(term==predictor_ensym)


      final_table <- final_table %>%
          bind_rows(model_tidy_text)

    }

  }
  if(include_overall == TRUE){
    model_tidy_text_overall <- is_unconditional_logistic_regression(interstroke_data=!!interstroke_data,
                                                                    models=c(model)) %>%
      tibble::add_column(n=nrow(interstroke_data))%>%
      tibble::add_column(subgroup="Overall")%>%
      filter(term==predictor_ensym)

    final_table <- model_tidy_text_overall %>%
      bind_rows(final_table)
  }

  # P value rounding
  final_table <- final_table %>%
    mutate(across(p_interaction, as.numeric)) %>%
    mutate_at(vars(p_interaction), funs(round(., 3))) # last value determines decimel places

  # final_table$p_interaction <- format(round(final_table$p_interaction,3), nsmall = 3)

  return(final_table)

  # format(round(.,3), nsmall = 3)

}

# is_unconditional_logistic_regression_stratified(interstroke_data=interstroke_bmi,
#                                             subgroups=c("esex","smoking"),
#                                             model=case_num~whr+eage,
#                                             include_overall = FALSE)
#
# is_unconditional_logistic_regression_stratified(interstroke_data=interstroke_bmi,
#                                                 subgroups=c("esex","smoking"),
#                                                 model=case_num~bmi+eage)

#' Interstroke conditonal Logistic Regression Stratified
#'
#' @param interstroke_data An Interstroke Dataset
#' @param subgroups A list of subgroups for stratified analysis.
#' @param model A list of models for table 2
#' @param include_overall default is TRUE. Set to FALSE to exclude overall analysis.
#' @export
#' @return A dataframe with model output
#' @examples
#' is_conditional_logistic_regression_stratified(interstroke_data=interstroke_bmi, subgroups=c("esex","smoking"), model=case_num~whr+eage+strata(caseid),include_overall = FALSE)
is_conditional_logistic_regression_stratified <- function(interstroke_data, subgroups, model, include_overall = TRUE) {

  warning("is_conditional_logistic_regression_stratified is been replaced by is_logistic_regression_stratified. Provide regression_type as argument")
  print("is_conditional_logistic_regression_stratified is been replaced by is_logistic_regression_stratified. Provide regression_type as argument")


  predictor_extract <- c(model)
  predictor <- labels(terms(predictor_extract[[1]]))[1]

  predictor_quo <- rlang::enquo(predictor)
  predictor_ensym <- rlang::ensym(predictor)

  final_table <- data.frame(model=character(),
                            term=character(),
                            estimate=double(),
                            conf.low=double(),
                            conf.high=double(),
                            subgroup=character(),
                            subgroup_level=character())

  for (i in seq_along(subgroups)) {
    subgroup = subgroups[[i]]

    #Perform stratified analysis for each subgroup
    factor_levels <- interstroke_data %>%
      dplyr::pull(subgroup) %>%
      forcats::fct_drop(.) %>%
      forcats::fct_unique(.) %>%
      as.character()

    #Perform stratified analysis for level of factor
    for (factor_level in factor_levels) {
      #Subgroup interstroke_data by factor level
      data_subgroup <<- interstroke_data %>%
        filter(!!sym(subgroup) == factor_level)
      subgroup_ensym <- rlang::ensym(subgroup)
      model_tidy_text <- is_conditional_logistic_regression(interstroke_data=data_subgroup,
                                                              models=c(model)) %>%
        tibble::add_column(subgroup=is_variable_label(interstroke_data, subgroup)) %>%
        tibble::add_column(subgroup_level=factor_level) %>%
        tibble::add_column(n=nrow(data_subgroup)) %>%
        tibble::add_column(p_interaction=is_tb_conditional_interaction(interstroke_data = interstroke_data,
                                                                         predictor = !!predictor_ensym,
                                                                         interaction_variable = !!subgroup_ensym)) %>%
        filter(term==predictor_ensym)

      final_table <- final_table %>%
        bind_rows(model_tidy_text)

    }

  }
  if(include_overall == TRUE){
    model_tidy_text_overall <- is_conditional_logistic_regression(interstroke_data=!!interstroke_data,
                                                                    models=c(model)) %>%
      tibble::add_column(n=nrow(interstroke_data))%>%
      tibble::add_column(subgroup="Overall")%>%
      filter(term==predictor_ensym)

    final_table <- model_tidy_text_overall %>%
      bind_rows(final_table)
  }

  # P value rounding
  final_table <- final_table %>%
    mutate(across(p_interaction, as.numeric)) %>%
    mutate_at(vars(p_interaction), funs(round(., 3))) # last value determines decimel places

  return(final_table)

}

# is_conditional_logistic_regression_stratified(interstroke_data=interstroke_bmi,
#                                                 subgroups=c("esex","smoking"),
#                                                 model=case_num~whr+eage+strata(caseid),
#                                                 include_overall = FALSE)

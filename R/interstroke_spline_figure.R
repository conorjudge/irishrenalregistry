#' Interstroke Spline Figure
#'
#' @param interstroke_data Project specific Interstroke dataset.
#' @param formula Model formulas to include in figure (see example)
#' @param predictor Predictor variable.
#' @export
#' @return An Interstroke spline figure
#' @examples
#' is_figure_spline(interstroke_data=interstroke_pollution, formula=case_num ~ rcs(facthmkm,3),predictor=facthmkm)
is_figure_spline <- function(interstroke_data, formula, predictor, env = caller_env()) {

  print("here 1")
    interstroke_data <- enexpr(interstroke_data)
    print("here 2")
    formula <- enexpr(formula)
    print("here 3")
    predictor <- enexpr(predictor)
    print("here 4")

    lm_call <- expr(lrm(!!formula, data = !!interstroke_data))

    print("here 5")
    print(paste0("lm_call = ",lm_call))

    model <- eval(lm_call, env)
    print("here 6")

    dataplot_call <- expr(Predict(model,!!predictor, ref.zero = TRUE, fun=exp))
    print("here 7")
    dataplot <- eval(dataplot_call, env = environment())
    print("here 8")

    plot <- ggplot(dataplot,aes(!!predictor, yhat)) +
      theme_classic() +
      scale_y_continuous("Odds Ratio (95% CI)")+
      geom_hline(yintercept =1, linetype="dashed")

    print("here 9")

    return(plot)

}


#Setup for RCS
#dd <- rms::datadist(interstroke_pollution)
#options(datadist="dd")

#is_figure_spline(formula=case_num ~ rcs(facthmkm,3),interstroke_data=interstroke_pollution,predictor=facthmkm)

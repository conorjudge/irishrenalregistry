library(tidyverse)
library(labelled)
library(sjlabelled)
library(interstroke)
library(forcats)

#Example dropped variables to test
#BMI
# dropped_prop_vars <- c(
#   "BMI <25",
#   NA
#   )

#Cooking fuel
# dropped_prop_vars <- c(
#   "Clean fuel",
#   "Combined",
#   "Kerosene",
#   "Solid fuel",
#   NA
#   )

#depdosedef
# dropped_prop_vars <- c(
#   "Not depressed",
#   #"0-2 Items",
#   #"3-4 Items",
#   ">4 Items",
#   NA
# )



is_figure_proportion <- function(interstroke_data, x_axis_variable, proportion_variable) {

  x_axis_variable <- enquo(x_axis_variable)
  proportion_variable <- enquo(proportion_variable)

  interstroke_data_summary <- interstroke_data %>%
    select(!!x_axis_variable, case,!!proportion_variable) %>%
    group_by(!!x_axis_variable,case,!!proportion_variable) %>%
    summarise(n = n()) %>%
    mutate(perc = ((n / sum(n)) * 100)) %>%
    mutate(x_axis_variable = !!x_axis_variable)

  #Drop NA's
  # interstroke_data_summary <- drop_na(interstroke_data_summary)

  interstroke_data_summary_overall <- interstroke_data %>%
    select(case, !!proportion_variable) %>%
    group_by(case, !!proportion_variable) %>%
    summarise(n = n()) %>%
    mutate(x_axis_variable = "Overall",
           perc = ((n / sum(n)) * 100))

  #Drop NA's
  # interstroke_data_summary_overall <- drop_na(interstroke_data_summary_overall)

  #Append to dataframe
  interstroke_data_summary <- bind_rows(interstroke_data_summary, interstroke_data_summary_overall)

  #Drop unwanted variables
  if( exists("dropped_prop_vars") )
  {
    interstroke_data_summary <- filter(interstroke_data_summary, !(!!proportion_variable %in% dropped_prop_vars))
  }

  #Assign levels to correct order
  interstroke_data_summary$x_axis_variable <- factor(interstroke_data_summary$x_axis_variable)

  interstroke_data_summary$x_axis_variable <- fct_relevel(interstroke_data_summary$x_axis_variable, "Overall", after = 0)

  #Prepare counts of case/controls for caption
  interstroke_counts <- interstroke_data_summary %>%
    select(!!x_axis_variable, case, !!proportion_variable, n) %>%
    group_by(!!x_axis_variable, case) %>%
    summarise(n = sum(n))

  interstroke_counts <- interstroke_counts %>%
    pivot_wider(names_from = case, values_from = n) %>%
    unite("case_control", Case:Control, sep = "/")

  #Case/control caption string, adjust number of spaces in gsub to match plot
  case_control_label <- toString(interstroke_counts$case_control)
  case_control_label <- gsub(",", "  ", x = case_control_label)
  case_control_label <- paste0("Case/Control   ", case_control_label)

  #Smart y-axis limits
  plot_height <- interstroke_data_summary %>%
    select(!!x_axis_variable, case, !!proportion_variable, perc) %>%
    group_by(!!x_axis_variable, case) %>%
    summarise(max_stack = sum(perc)) %>%
    filter(max_stack == max(max_stack)) %>%
    transmute(max_stack = round((max_stack + 20), -1)) %>% # adjusts the height based on max value
    pull()

  #Set max plot height to 100 percent
  if (max(plot_height) >100) {
    plot_height <- 100
  }

  is_proportion_figure <- ggplot() +
    geom_col(data = interstroke_data_summary,
             aes(x = case, y = perc, fill = !!proportion_variable),
             position='stack', width = 0.3) + #adjust width for bar thickness
    facet_wrap( ~ x_axis_variable, strip.position = "bottom", nrow = 1,
                labeller = label_wrap_gen(10)) + #adjust label_wrap_gen(x) to change allowed no. of characters
    scale_x_discrete(expand = expansion(add = 1)) + #expand x axis to display continous x-axis line
    scale_y_continuous(limits=c(0, max(plot_height)), breaks = seq(0, max(plot_height), 10),
                       expand = c(0, 0)) +
    scale_fill_viridis_d(begin = 0,
                         end = 0.8,
                         na.value = 1) +
    labs(y = "Percentage", x = element_blank(),
         caption = case_control_label)+
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          panel.background = element_blank(),
          panel.spacing = unit(0, "mm"),
          plot.background = element_rect(colour = "darkred", size = 1.5), #red border
          text=element_text(size=12),
          axis.line = element_line(),
          axis.ticks.x = element_blank(),
          axis.ticks.length=unit(.25, "cm"),
          axis.text.x = element_text(angle = 45, hjust=1,vjust=1.2),
          axis.title.y = element_text(vjust=5),
          legend.title = element_blank(),
          legend.justification = c("left", "top"),
          legend.background = element_blank(),
          legend.key.height = unit(0.3, "cm"),
          legend.text = element_text(size=12),
          legend.position = c(0.05, 1),
          plot.margin = margin(0.5, 0.5, 0.5, 2, "cm")) # Top, right, bottom, left

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

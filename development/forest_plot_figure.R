library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)
library(flextable)
library(magick)

rm(list=ls())
forestdf <- read_csv("is_infection1.csv")

#is_forest_plot <- function()

###
# Create the left sided flextable
###

forest_table <- forestdf %>%
  select(Population,N,estimate,ci_lb,ci_ub) %>%
  mutate(estimate_formatted = paste0(sprintf('%.2f',estimate), " (",ci_lb,"-",ci_ub,")")) %>%
  mutate(estimate_formatted = ifelse(estimate_formatted=="NA (NA-NA)","NA",estimate_formatted)) %>%
  select(Population,N,estimate_formatted) %>%
  flextable() %>%
  autofit() %>%
  theme_booktabs() %>%
  set_header_labels(estimate_formatted = "OR (95% CI)") %>%
  border_remove() %>%
  bold(bold = TRUE, part = "header") %>%
  as_raster()

gg_forest_table <- ggplot() +
  theme_void() +
  annotation_custom(rasterGrob(forest_table), xmin=-Inf, xmax=Inf, ymin=.05, ymax=Inf)

###
# Create the right sided forest plot
###

forestdf$rownumber = as.numeric(nrow(forestdf):1)

#forestdf <- forestdf %>%
#  mutate(colour = rep(c("gray95", "white"), 9))

gg_forest_plot <- ggplot(forestdf, aes(x = estimate, y = rownumber, xmin = ci_lb, xmax = ci_ub)) +
  #geom_hline(aes(yintercept = rownumber, colour = colour), size = 10) +
  geom_pointrange(data = subset(forestdf, !is.na(estimate)),
                  shape = 22,
                  fill = "black") +
  geom_pointrange(data = subset(forestdf, is.na(estimate)),
                  aes(x = 1, y = rownumber), shape = 22, color = "white", fill = "white", size=0) +
  geom_point(shape = 22, fill = "black", aes(size=1/(ci_ub-ci_lb))) +
  geom_vline(xintercept = 1, linetype = 3) +
  xlab("OR (95% CI)") +
  theme_classic() +
  scale_colour_identity() +
  scale_x_continuous(limits = c(0.25, 3),
                breaks = c(0.25, 0.5, 1, 1.1, 2, 3),
                labels = c("0.25", "0.5", "1", "1.1","2", "3"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y=element_blank(),
        legend.position = "none") +
  ggtitle(" ")

###
# Combine left and right sides
###
cowplot::plot_grid(gg_forest_table, gg_forest_plot, nrow = 1, ncol=2, rel_width = c(1, 1) )

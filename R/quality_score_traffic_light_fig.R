# Quality score traffic light figure
# Figure 2 in manuscript


# Getting set up --------------------------------------------------------------
# install libraries (only needs to be run the first time you use this code)
install.packages("dplyr")
install.packages("tidyr")
install.packages("here")
install.packages("ggplot2")
install.packages("devtools")
library(devtools)
devtools::install_github("mcguinlu/robvis")
install.packages("pals")
install.packages("colorblindcheck")
install.packages("ggpubr")

# load libraries (run this each time you open R)
library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(robvis)
library(pals)
library(colorblindcheck)
library(ggpubr)


# Instructions for setting up a risk-of-bias traffic light figure:
# https://cran.r-project.org/web/packages/robvis/vignettes/Introduction_to_robvis.html

# load in the clean data (need to run the clean data code first) -------------------
Quality_scores <- read.csv(here("data_clean/Quality_scores_clean.csv"), header = T)


# set up data for traffic light figure using the ROB1 template (allows for flexibility in the number and names of the columns)

Traffic_light_data <- Quality_scores %>%
  select(Study_meta, Cohort_Selection_Risk:Overall_Risk) %>%
  rename(Study = Study_meta, Cohort.Selection = Cohort_Selection_Risk, Comparability = Comparability_Risk, Outcome = Outcome_Risk, Overall = Overall_Risk)
Traffic_light_data$Weight <- c(1)
Traffic_light_data <- as.data.frame(Traffic_light_data)
Traffic_light_data$Cohort.Selection <- factor(Traffic_light_data$Cohort.Selection, levels = c("High", "Some Concerns", "Low"))
Traffic_light_data$Comparability <- factor(Traffic_light_data$Comparability, levels = c("High", "Some Concerns", "Low"))
Traffic_light_data$Outcome <- factor(Traffic_light_data$Outcome, levels = c("High", "Some Concerns", "Low"))
Traffic_light_data$Overall <- factor(Traffic_light_data$Overall, levels = c("High", "Some Concerns", "Low"))


write.csv(Traffic_light_data, here("data_clean/Traffic_light_data.csv"))


# summary plot figure can also be created using the robvis tool found here using the generic template:
# https://mcguinlu.shinyapps.io/robvis/
# McGuinness, LA, Higgins, JPT. Risk-of-bias VISualization (robvis): An R package and Shiny web app for visualizing risk-of-bias assessments. Res Syn Meth. 2020; 1- 7. https://doi.org/10.1002/jrsm.1411



# Summary quality figure
summary_quality_fig <- rob_summary(Traffic_light_data, tool = "Generic", overall = TRUE) +
  scale_fill_manual(values = c("#BF0000", "#E2DF07", "#02C100"), labels = c("Low quality", "Fair quality", "High quality"), name = "Quality score")
summary_quality_fig
# ggsave(here("figures/summary_quality_fig.png"), plot = summary_quality_fig, dpi = 600, width = 7, height = 3.5, units = "in")
ggsave(here("figures/manuscript_figs/Fig2.tiff"), plot = summary_quality_fig, dpi = 600, width = 7, height = 3.5, units = "in", compression="lzw")



# this partially works, but the figure is not formatting correctly for some reason.
traffic_light_fig <- rob_traffic_light(Traffic_light_data, tool = "Generic", psize = 3.5) +
  theme(strip.text.y = element_text(angle = 90, size = 6))
traffic_light_fig
ggsave(here("figures/traffic_light_quality_fig.png"), plot = traffic_light_fig, dpi = 600, width = 7, height = 12, units = "in")


# This script reproduces the figures used in the paper
# I had to make a number of adjustments here to comply with journal recommendations
# See: https://www.springer.com/journal/10899/submission-guidelines

# We also changed the name of the "Clubs" condition to "Industry" for the paper.

# Packages ----------------------------------------------------------------------------------------------

# Admin
library(here)

# Copy/Paste Wizardry
library(datapasta)

# The basics
library(tidyverse)

# For plotting
library(cowplot)
library(magick)
library(rcartocolor)
library(ggridges)

# For modelling
library(tidymodels)
library(tidybayes)
library(tidybayes.rethinking)
library(rethinking)
library(brms)

# Load in Models and Data -------------------------------------------------------------------------------
# Load data
load(file = here("Analysis", "Data", "APPS_OSF-AnalysisData_2022-02-22.RData"))
# Load question dictionary
load(file = here("Analysis", "Data", "APPS_OSF-Questions_2022-02-22.RData"))

# Load model output see APPS_CM-Probit_Policy_2021-10.R for processing
models <- read_rds(file = here("Analysis", "Models", "Output", "CM-Probit", "Policy_CM-Probit_models.rds"))
posterior <- read_rds(file = here("Analysis", "Models", "Output", "CM-Probit", "Policy_CM-Probit_posterior.rds"))
posterior.sum <- read_rds(file = here("Analysis", "Models", "Output", "CM-Probit", "Policy_CM-Probit_posterior_summaries.rds"))

# For cumulative probability plot for National Standard Items.
reverse_NS <- readRDS(here("Analysis/Models/Output/CM-Probit/APPS_NS_Reverse_CM-Probit.rds"))

# Create Base Theme for ggplot --------------------------------------------------------------------------
theme_fix <- function() {
  theme_bw() %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          text = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0, margin = margin(b = 10)),
          plot.margin = margin(20, 10, 10, 10),
          plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10)),
          legend.position = "top",
          legend.background = element_rect(colour = "black", 
                                           linetype = "solid",
                                           size = 0.25),
          legend.justification = 0,
          legend.direction = "horizontal")
}

# Set this theme as base theme for the document.
theme_set(theme_fix())

# A colour scheme for plotting groups
APPS_ColourScheme <- c("#a8a8a8", "#f0a800", "#6090d8", "#c7254e")

# Compliance w/ Regulation ---------------------------------------------------------------------------------
# This figure was a little different from the ones that follow because of the values I chose to 
# display on the x-axis.

vars <- c("NS_Display",
          "NS_Fair",
          "NS_Title")

plot_titles <- c(
  "Limit by Postcode",
  "Ban in Pubs, Clubs and RSLs",
  "Complete Ban"
)

questions |> 
  select(contains("NS_")) |>
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Text") |> 
  pull(Text) -> plot_titles

reverse_NS <- 
  reverse_NS |>
  select(c_p, data_sum) |> 
  mutate(Item = factor(Item)) |> 
  mutate(Description = factor(Item, levels = c("Fair", "Display", "Mislead"), labels = plot_titles), .after = Item) |> 
  mutate(plot_sum = purrr::map(
    .x = c_p,
    .f = function(.x){
      .x |>
        group_by(Group, Response) |> 
        point_interval(c_p, .point = median, .interval = hdi)
    }))

# Rename "Clubs" to "Industry"
levels_old <- levels(reverse_NS$c_p[[1]]$Group)
levels_new <- c("Control", "Brain", "Design", "Industry")

# A function to Rename "Clubs" to "Industry"
rename_clubs <- function(.x) {
  .x |> 
    mutate(Group = factor(Group, 
                          levels = levels_old,
                          labels = levels_new))
}

reverse_NS <- reverse_NS |> 
  mutate(across(c(c_p, data_sum, plot_sum), .fns = ~purrr::map(.x = .x,.f = ~rename_clubs(.x))))

# I need to do the same for the posterior.summary cummulative estimates
posterior.sum <- posterior.sum |> mutate(across(c(c_p), .fns = ~purrr::map(.x = .x,.f = ~rename_clubs(.x))))

plot_NS <- function(plot_sum, data_sum) {
  ggplot(data = plot_sum) +
    # Draw a line a majority support
    geom_hline(yintercept = .5) +
    # Plot cumulative probability
    geom_errorbar(data = plot_sum,
                  mapping = aes(group = Group,
                                colour = Group,
                                x = Response,
                                ymin = .lower,
                                ymax = .upper),
                  width = .25,
                  position = position_dodge(width = .5)) +
    geom_point(data = plot_sum,
               mapping = aes(group = Group,
                             colour = Group,
                             fill = Group,
                             shape = Group,
                             x = Response,
                             y = c_p),
               size = 2.25,
               position = position_dodge(width = .5)) +
    # Plot observed cumulative proportions
    geom_point(data = data_sum,
               mapping = aes(x = Response, y = c_p, group = Group, shape = Group, fill = NULL),
               colour = "black",
               size = 2.25,
               position = position_dodge(width = .5)) +
    # Aesthetic Adjustments
    scale_y_continuous(breaks = (0:10)/10, limits = c(0, 1)) +
    scale_colour_manual(values = APPS_ColourScheme) +
    scale_fill_manual(values = APPS_ColourScheme) +
    scale_shape_manual(values = c(21, 22, 23, 24)) +
    labs(x = NULL,
         colour = NULL,
         fill = NULL,
         shape = NULL,
         y = NULL) +
    theme(plot.subtitle = element_text(size = 12),
          plot.margin = margin(0),
          aspect.ratio = 1)
}

reverse_NS <- 
  reverse_NS |> 
  mutate(plot = purrr::map2(
    .x = plot_sum,
    .y = data_sum,
    .f = plot_NS
  ))

# Truncate Axes
# This will produce a few warnings saying that I am dropping information from the plot
# Those warnings are just telling me that I am doing what I set out to do occurred
reverse_NS$plot[[1]] <- reverse_NS$plot[[1]] + 
  scale_x_discrete(limits = levels(reverse_NS$data_sum[[1]]$Response)[1:3],
                   labels = c("Strongly\nDisagree", "Disagree", "Slightly\nDisagree"))

reverse_NS$plot[[2]] <- reverse_NS$plot[[2]] + 
  scale_x_discrete(limits = levels(reverse_NS$data_sum[[2]]$Response)[1:3],
                   labels = c("Strongly\nDisagree", "Disagree", "Slightly\nDisagree"))

# Flip axis display order and truncate
reverse_NS$plot[[3]] <- reverse_NS$plot[[3]] + 
  scale_x_discrete(limits = levels(reverse_NS$data_sum[[3]]$Response)[1:3],
                   labels = c("Strongly\nAgree", "Agree", "Slightly\nAgree"))

legend <- 
  get_legend(
    reverse_NS$plot[[1]] + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom",
            legend.justification = .5, 
            legend.margin = margin(t = 15, b = 0),
            legend.background = element_blank())
  )

ylab <- get_plot_component(ggplot() + labs(y = "Cumulative Probability / Proportion"), "ylab-l")

plot <- 
  plot_grid(
    NULL,
    ylab,
    reverse_NS$plot[[1]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = "... are fair"), 
    reverse_NS$plot[[2]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) +  
      labs(subtitle = "... accurately display outcomes"),
    reverse_NS$plot[[3]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 15, t = 15, b = 0)) +
      labs(subtitle = "... are likely to mislead or decieve"),
    rel_widths = c(.05, .1, rep(1, 3)),
    nrow = 1)

plot <- plot_grid(legend, plot, NULL, ncol = 1, rel_heights = c(.15, 1, .05))

ggdraw(plot) 

## Plot saved from Rstudio as Figure_1.eps
# Width 900
# Height 380

# Legalisation ---------------------------------------------------------------------------------------------
vars <- c("Legal_Post", "Legal_Pubs", "Legal_All")

plot_titles <- c(
  "Limit by Postcode",
  "Ban in Pubs, Clubs and RSLs",
  "Complete Ban"
)

# I'll try to wrap some of these operations up in functions so that I can re-use them for subsequent 
# graphics

# Data wrangling function
prepare_plot_data <- function(d, posterior.sum, vars, plot_titles) {

d |> 
  # Subset Ban/Access Items
  select(Group, all_of(vars)) |>
  # Rename Clubs
  mutate(Group = factor(Group, 
                        levels = levels_old,
                        labels = levels_new)) |> 
  # Pivot long and nest
  pivot_longer(cols = all_of(vars), names_to = "Item", values_to = "Response") |> 
  mutate(Response = fct_rev(Response)) |> 
  group_by(Item) |> 
  nest(data = c(Group, Response)) -> d.summary

d.summary <- 
  d.summary |> 
  mutate(data_sum = purrr::map(
    .x = data,
    .f = function(.x) {
      .x |> 
        select(Group, Response) |> 
        group_by(Group) |> 
        count(Response) |> 
        mutate(Proportion = (n / sum(n)),
               c_p = cumsum(Proportion))
    }
  ))

plot_data <- 
  posterior.sum |> 
  filter(Item %in% vars) |> 
  select(Item, Description, c_p) |> 
  left_join(d.summary, by = "Item") |> 
  select(!data)

plot_data <- 
  plot_data |> 
  select(Item, c_p, data_sum) |> 
  mutate(Item = factor(Item)) |>  
  mutate(Description = factor(Item, levels = vars, labels = plot_titles), .after = Item)

return(plot_data)

}

# A plotting function.
multi_plot <- function(c_p, data_sum) {
  ggplot(data = c_p) +
    # Draw a line a majority support
    geom_hline(yintercept = .5) +
    scale_y_continuous(breaks = (0:10)/10, limits = c(0, 1)) +
    scale_colour_manual(values = APPS_ColourScheme) +
    scale_fill_manual(values = APPS_ColourScheme) +
    # Plot cumulative probability
    geom_errorbar(data = c_p,
                  mapping = aes(group = Group,
                                colour = Group,
                                x = Response,
                                ymin = .lower,
                                ymax = .upper),
                  width = .25,
                  position = position_dodge(width = .5)) +
    geom_point(data = c_p,
               mapping = aes(group = Group,
                             colour = Group,
                             fill = Group,
                             shape = Group,
                             x = Response,
                             y = c_p),
               size = 2.25,
               position = position_dodge(width = .5)) +
    geom_point(data = data_sum,
               mapping = aes(x = Response, y = c_p, group = Group, shape = Group, fill = NULL),
               colour = "black",
               size = 2.25,
               position = position_dodge(width = .5)) +
    scale_shape_manual(values = c(21, 22, 23, 24)) +
    labs(x = NULL,
         colour = NULL,
         fill = NULL,
         shape = NULL,
         y = NULL) +
    theme(plot.subtitle = element_text(size = 12),
          plot.margin = margin(0),
          aspect.ratio = 1) +
    # Truncate Axes
    scale_x_discrete(limits = levels(data_sum$Response)[1:3],
                     labels = c("Strongly\nAgree", "Agree", "Slightly\nAgree"))
}

plot_data <- prepare_plot_data(d, posterior.sum, vars, plot_titles)

plot_data <- 
  plot_data |> 
  mutate(plot = purrr::map2(
    .x = c_p,
    .y = data_sum,
    .f = multi_plot
  ))

legend <- 
  get_legend(
    plot_data$plot[[1]] + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom",
            legend.justification = .5, 
            legend.margin = margin(t = 15, b = 0),
            legend.background = element_blank())
  )

plot <- 
  plot_grid(
    NULL,
    ylab,
    plot_data$plot[[1]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[1]]),
    plot_data$plot[[2]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[2]]),
    plot_data$plot[[3]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 15, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[3]]),
    rel_widths = c(.05, .1, rep(1, 3)),
    nrow = 1)

# Re-use legend and caption from earlier.
plot <- plot_grid(legend, plot, NULL, ncol = 1, rel_heights = c(.15, 1, .05))

ggdraw(plot)

# Figure saved from R Studio as Figure_2.eps
# Width 900
# Height 380

# Self-Exclusion and Pre-Commitment ------------------------------------------------------------------------
vars <- c("PC_Pokies", "PC_All", "SE_Pokies", "SE_All")

plot_titles <- c(
  "Pre-Commitment\nEGMs only", 
  "Pre-Commitment\nAll gambling", 
  "Self-Exclusion\nEGMs only", 
  "Self-Exclusion\nAll Gambling")

plot_data <- prepare_plot_data(d, posterior.sum, vars, plot_titles)

plot_data <- 
  plot_data |> 
  mutate(plot = purrr::map2(
    .x = c_p,
    .y = data_sum,
    .f = multi_plot
  ))

plot <- 
  plot_grid(
    NULL,
    ylab,
    plot_data$plot[[1]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[1]]),
    plot_data$plot[[2]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[2]]),
    plot_data$plot[[3]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[3]]),
    plot_data$plot[[4]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[4]]),
    NULL,
    rel_widths = c(.05, .1, rep(1, 4), .05),
    nrow = 1)

# Re-use legend and caption from earlier.
plot <- plot_grid(legend, plot, NULL, ncol = 1, rel_heights = c(.15, 1, .05))

ggdraw(plot)

# Figure Saved as Figure_3.eps
# Width 1100
# Height 380

# Alternative layout:
plot2 <- 
  plot_grid(
    plot_data$plot[[1]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 7.5, r = 7.5, t = 15, b = 5)) + 
      labs(subtitle = plot_titles[[1]]),
    plot_data$plot[[2]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 15, t = 15, b = 5)) + 
      labs(subtitle = plot_titles[[2]]),
    plot_data$plot[[3]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 7.5, r = 7.5, t = 10, b = 7.5)) + 
      labs(subtitle = plot_titles[[3]]),
    plot_data$plot[[4]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 15, t = 10, b = 7.5)) + 
      labs(subtitle = plot_titles[[4]]),
    nrow = 2)

plot2 <- 
  plot_grid(
    NULL,
    ylab,
    plot2,
    rel_widths = c(.0375, .025, 1),
    nrow = 1)

# Re-use legend and caption from earlier.
plot2 <- plot_grid(legend, plot2, ncol = 1, rel_heights = c(.075, 1))

ggdraw(plot2)

# Saved as Figure_3a.eps
# Width 445
# Height 575

# Alternatively, just one example?
plot_data$plot[[1]] + 
  theme(legend.position = "none",
        plot.margin = margin(l = 0, r = 7.5, t = 10, b = 10)) + 
  labs(subtitle = plot_titles[[1]])

# saved as Figure3b.eps


# Remaining ---------------------------------------------------------------------------------------------

vars <- c("MaxBets",
          "Counselling_Treat",
          "MEDIA",
          "VenueInfo_Contact",
          "VenueInfo_Hourly",
          "ScreenMSG")

plot_titles <- c("$1 AUD Maximum Bets", 
                 "Free Treatment", 
                 "Media Campaigns", 
                 "Helpline Number and Warnings", 
                 "Expected Hourly Losses", 
                 "Onscreen Pop-Up Messages")

plot_data <- prepare_plot_data(d, posterior.sum, vars, plot_titles)

plot_data <- 
  plot_data |> 
  mutate(plot = purrr::map2(
    .x = c_p,
    .y = data_sum,
    .f = multi_plot
  ))

plot_row_1 <- 
  plot_grid(
    NULL,
    ylab,
    plot_data$plot[[1]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[1]]),
    plot_data$plot[[2]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[2]]),
    plot_data$plot[[3]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[3]]),
    NULL,
    rel_widths = c(.05, .1, rep(1, 3), .05),
    nrow = 1)

plot_row_2 <- 
  plot_grid(
    NULL,
    ylab,
    plot_data$plot[[4]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[4]]),
    plot_data$plot[[5]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[5]]),
    plot_data$plot[[6]] + 
      theme(legend.position = "none",
            plot.margin = margin(l = 0, r = 7.5, t = 15, b = 0)) + 
      labs(subtitle = plot_titles[[6]]),
    NULL,
    rel_widths = c(.05, .1, rep(1, 3), .05),
    nrow = 1)

# Re-use legend and caption from earlier.
plot <- plot_grid(legend, plot_row_1, plot_row_2, NULL, ncol = 1, rel_heights = c(.15, 1, 1, .05))

ggdraw(plot)

# Figure Saved as Figure_R.eps
# Width 900
# Height 720


# Functional
here::i_am("Analysis/Models/APPS_CM-Probit_Policy_2021-10.R")
library(here)      # Relative paths w R Projects
library(datapasta) # Probably not essential - used for super powered copy/paste

# The basics
library(tidyverse)

# For modelling
library(rethinking)
library(brms)
library(tidybayes)

# Plot output
library(svglite)

# Load data
load(file = here("Analysis", "Data", "APPS_OSF-AnalysisData_2022-02-22.RData"))
# Load question dictionary
load(file = here("Analysis", "Data", "APPS_OSF-Questions_2022-02-22.RData"))

# I like d for working data and data for full data set
data <- d

# Note that this script will take a long time to run in full. 
# There is iteration over every primary outcome measure in the paper (23), 
# Models are fit using brms and stan, which takes about 20 seconds per item to fit the model on my machine see
# README.md
# There are then numerous summarising operations performed over every model.

# A tutorial on this type of model:
# See Bürkner and Vuorre (2019) Ordinal Regression Models in Psychology: A Tutorial
# DOI: 10.1177/2515245918823199

# Wrangle data for analysis -----------------------------------------------------------------------------

# This section creates a large nested data frame
# I will work towards a structure where we have an Item identifier in col 1, a short plain text description
# in column 2. Column three will contain a subset of the data necessary to analyse that one item. Column 3 is
# a nested column, so that each element (cell), contains a whole data frame, rather than a single value.
# Each entry in the data column will contain numeric ID, Group identifier, as well as the Response (as factor),
# Response (as integer) for that particular item.

# The National Standard Items had to be treated differently because of the I don't know responses:
vars1 <- c("numericID",
           "Group",
           "NS_Fair",
           "NS_Display",
           "NS_Title")

d <- 
  data %>%
  select(all_of(vars1)) %>%
  pivot_longer(cols = !c(numericID, Group), values_to = "Response", names_to = "Item") %>%
  # Tidy up the item names
  mutate(Item = str_remove(Item, pattern = "NS_")) %>%
  mutate(Item = str_replace(Item, pattern = "Title", replacement = "Mislead")) %>%
  mutate(Response = factor(Response, exclude = "I Don't Know")) %>%
  # Drop I Don't Know Responses
  na.omit() %>%
  # I want "Strongly Agree" to be item 1, rather than 6.
  mutate(Response = fct_rev(Response)) %>%
  # Numeric Codes for convenience 
  mutate(Response_N = as.numeric(Response))

# Nest Data
d <- 
  d %>%
  group_by(Item) %>%
  nest()

# I want to save the scale levels (useful later)
this_scale <- levels(d$data[[1]]$Response)
these_groups <- levels(d$data[[1]]$Group)

# Now we can repeat with remaining Items
vars2 <- c("numericID",
           "Group",
           "Legal_Post",
           "Legal_Pubs",
           "Legal_All",
           "PC_Pokies",
           "PC_All",
           "SE_Pokies",
           "SE_All",
           "MaxBets",
           "Counselling_Treat",
           "MEDIA",
           "VenueInfo_Contact",
           "VenueInfo_Hourly",
           "ScreenMSG",
           "Resp_INDV", 
           "Resp_SNtwk", 
           "Resp_Design",
           "Resp_Venues",
           "Resp_Empl",
           "Resp_Gvmt",
           "Resp_Aust"
           )

d <- 
  data %>%
  select(all_of(vars2)) %>%
  pivot_longer(cols = !c(numericID, Group), values_to = "Response", names_to = "Item") %>%
  # I want "Strongly Agree" to be item 1, rather than 6.
  mutate(Response = fct_rev(Response)) %>%
  # Numeric Codes for convenience 
  mutate(Response_N = as.numeric(Response)) %>%
  group_by(Item) %>%
  nest() %>%
  # Bind with the National Standard Items
  bind_rows(d, .)

# Created with help from datapasta
# Create our description column
d$Description <- c("Poker machines are fair",
                   "Poker machines accurately display gambling outcomes",
                   "Poker machines are likely to mislead or deceive consumers",
                   "State governments should impose a limitation on the number of poker machines by postcode",
                   "Pokies gambling should be banned in pubs, clubs and RSLs.",
                   "Pokies gambling should be banned in all venues including casinos.",
                   "Mandotory pre-commitment program: for all pokies gambling venues",
                   "Mandotory pre-commitment program: for all gambling modes (including EGMS and online operators)",
                   "Self-exclusion program: for all pokies gambling venues",
                   "Self-exclusion program: for all gambling modes (including EGMS and online operators)",
                   "$1 Maximum Bets for Australian Pokies",
                   "Counselling and treatment for gambling addiction funded by taxes on gambling revenue.",
                   "Governments should run mass media campaigns that provide information about gambling harm",
                   "Prominent warnings and contact information for gambling counselling services inside gambling venues",
                   "Prominently display the average hourly losses of poker machines inside gambling venues",
                   "Pop-up messages when an individual has been using an EGM for an extended period of time.",
                   "The individual should be held responsible",
                   "The individual’s immediate family or close friends should be held responsible",
                   "The companies or individuals who design and sell poker machines to venues should be held responsible",
                   "The companies or individuals who own and profit from casinos and pokies venues should be held responsible",
                   "The individual employees who work in gambling venues, such as bar staff, floor managers, dealers or croupiers should be held responsible",
                   "State governments who legalise, regulate and permit gambling should be held responsible",
                   "Australian society or culture in general should be held responsible")

# Place this next to the shortened variable name:
d <- d %>% relocate(Description, .after = Item)

# Check priors ------------------------------------------------------------------------------------------

# This was just to confirm my thinking that a normal(0, 1) prior for the threshold values would be reasonably 
# evenly distributed across probability space. 

# As I was writing up this paper A. Solomon Kurz produced a great tutorial on the cumulative probit model. 
# How I wish this had of been available while I was learning to do this type of analysis!
# He recommends a different strategy for setting for setting priors. 
# Kurz's justification for these alternative priors can be found here.
# https://solomonkurz.netlify.app/post/2021-12-29-notes-on-the-bayesian-cumulative-probit/
# I did re-run this analysis at some point using these priors but it made little overall difference.
# Either way the data overwhelm the prior. So I've stuck with the original approach.

thisMany <- 1e5

tibble(SD   = c(10, 1.5, 1, 0.5, .25),
       samples = list(1:thisMany),
       alpha   = purrr::map(.x = SD, .f = ~rnorm(thisMany, 0, .x)),
       p       = purrr::map(alpha, pnorm)) %>%
  unnest(cols = c(!SD)) %>%
  mutate(SD = factor(SD)) %>%
  
  ggplot() +
  theme_bw() +
  geom_density(mapping = aes(x = p, group = SD, fill = SD, colour = SD),
               adjust = 1/10,
               alpha = .35,
               size = 1) +
  scale_x_continuous(limits = c(-0.025, 1.025)) +
  labs(title = "Some candidate priors")

# So SD = 1 looks like the least informative choice.

# Fit Models --------------------------------------------------------------------------------------------

# Now we are going to run the models (this will take a while). 
# The script below iterates through this data frame and saves the model output in another list column
# Each model takes ~ 20 seconds to set-up and then fit on my machine (see README.md)
d <- 
  d %>%
  mutate(Model = purrr::map(
    .x = data, 
    # Bayesian Ordered Probit Model + Unequal Variances (Probit)
    .f = ~brm(
      
      # Our linear model
      formula = bf(Response ~ 1 + Group) +
        # This allows us to model unequal variances between groups
        lf(disc ~ 0 + Group, cmc = FALSE),
      # Data iterated in using purrr
      data = .x,
      # Probit link 
      family = cumulative("probit"),
      # Set Priors
      prior = c(prior(normal(0, 1), class = Intercept),       # Flat prior across most of the probability space.
                prior(normal(0, 0.5), class = b),             # Mild regularisation, sceptical of very large effects > 1
                prior(normal(0, 1), class = b, dpar = disc)), # Prior for disc b values
      # MCMC set-up
      iter = 2250, warmup = 1000, cores = 8, chains = 8)
  ))

# Our data frame now includes a model fit for each item.

# A note on what comes next -----------------------------------------------------------------------------

# The next part of this script works by running the same processes over and over, generating different types of 
# output for each of the policy items. At the end of the process I will have a dataframe that has collated a 
# copy of this information for each item. That way I can access it from the console, or use it in a summary 
# RMarkdown document

# I will separate output into a "posterior" and "posterior_summaries" data frame.
# One contains all posterior draws related to a parameter or contrast of interest
# The other contains summaries of these posteriors.

# The structure of these data frames will be comparable the that used for the dataframe d.

# Posterior ---------------------------------------------------------------------------------------------

## Copy the first two cols from our df to label things
posterior <- d[1:2]

# Get posterior draws for each model
posterior$draws = purrr::map(.x = d$Model, 
                             .f = ~spread_draws(.x, # d$Model[[i]], # 
                                                b_Intercept[k], 
                                                # Difference group latent means and control
                                                b_GroupBrain, b_GroupDesign, b_GroupClubs,
                                                # We also want the SD for each latent distribution
                                                b_disc_GroupBrain, b_disc_GroupDesign, b_disc_GroupClubs))

# The column names are pretty messy.
names(posterior$draws[[1]])

# We can clean up these column names like so:
posterior <- 
  posterior %>% 
  mutate(
    draws = purrr::map(
      .x = draws,
      .f = function(.x) {
        .x %>% # Take posterior for item i
          # Drop preceeding brms noise
          rename_with(.cols = contains("b_Group"), .fn = ~str_remove(.x, "b_Group")) %>%
          rename_with(.cols = contains("b_disc"), .fn = ~paste(str_remove(.x, "b_disc_Group"), "_sd", sep = "")) %>%
          rename(cutpoints = "b_Intercept")
      }
    )
  )

# That's a little better:
names(posterior$draws[[1]])

# Get Standard Deviation --------------------------------------------------------------------------------

# The _disc parameter is the log of the inverse standard deviation. 
# To recover the SD we need to exponentiate the parameter and take it's inverse.
# See Bürkner and Vuorre (2019) Ordinal Regression Models in Psychology: A Tutorial
# DOI: 10.1177/2515245918823199
posterior <- 
  posterior %>% 
  mutate(
    draws = purrr::map(
      .x = draws,
      .f = function(.x) {
        .x %>% # Take posterior for item i
          mutate(across(.cols = contains("_sd"), .fns = ~(1 / exp(.x))))
      }
    )
  )


# Tidy up the posterior DF for each of access -----------------------------------------------------------

## Beta/ Mean of the Latent Distributions ---------------------------------------------------------------

posterior <- 
  posterior %>%
  mutate(
    # "Beta" can be understood as the difference between the latent mean of the reference group 
    # (0, the control group) and the experimental group
    latentMean = purrr::map(
      .x = draws,
      .f = function(.x) {
        .x %>%
          # Beta values are the same st each threshold point, so we need to filter the data to drop dupes
          filter(k == 1) %>%
          ungroup() %>%
          select(.draw:Clubs) %>%
          # It will be useful to have the Control group marked here too
          # The model assumes a standard distribution dnorm(0, 1) for the reference group.
          mutate(Control = 0, .before = Brain) %>%
          pivot_longer(cols = c(Control:Clubs),
                       names_to = "Group",
                       values_to = "latentMean") %>%
          mutate(Group = factor(Group, 
                                levels = c("Control", "Brain", "Design", "Clubs"),
                                ordered = T))
      }
    )
  )

# These Latent means are already relative to control, but we might be interested in the differences between
# our other groups:
posterior <- 
  posterior %>%
  mutate(
    LM_diffs = purrr::map(
      .x = latentMean,
      .f = function(.x) {
        .x %>%
          pivot_wider(names_from = Group, values_from = latentMean) %>%
          mutate(across(Brain:Design, .fns = ~(.x - Clubs), .names = "{col}_Clubs")) %>%
          mutate(Design_Brain = Design - Brain) %>%
          select(!Control:Clubs) %>%
          pivot_longer(!.draw, names_to = "Contrast", values_to = "diff") %>% 
          mutate(Contrast = factor(Contrast, 
                                   levels = c("Brain_Clubs",
                                              "Design_Clubs",
                                              "Design_Brain"),
                                   ordered = T))
      }
    )
  )


# Standard Deviation of the Latent Distributions --------------------------------------------------------

posterior <- 
  posterior %>%
  mutate(
    SD = purrr::map(
      .x = draws,
      .f = function(.x) {
        .x %>%
          # SD values are the same at each threshold point, so we need to filter the data to drop dupes
          filter(k == 1) %>%
          # Drop k grouping
          ungroup() %>%
          # Select cols
          select(.draw, Brain_sd:Clubs_sd) %>%
          # Again this is a feature of the model
          mutate(Control_sd = 1, .before = Brain_sd) %>%
          # Long form is more useful for summaries and plotting
          pivot_longer(cols = c(Control_sd:Clubs_sd),
                       names_to = "Group",
                       # This will drop the suffix "_sd"
                       names_pattern = "(.*)(?=_sd)",
                       values_to = "SD") %>%
          # Re factor the data
          mutate(Group = factor(Group, 
                                levels = c("Control", "Brain", "Design", "Clubs"), 
                                ordered = T))
      }
    )
  )


# Contrasts for SDs -------------------------------------------------------------------------------------

posterior <- 
  posterior %>%
  mutate(
    SD_diffs = purrr::map(
      .x = SD,
      .f = function(.x) {
        .x %>%
          pivot_wider(names_from = Group, values_from = SD) %>%
          mutate(across(Brain:Clubs, .fns = ~(.x - Control), .names = "{col}_Control")) %>%
          mutate(across(Brain:Design, .fns = ~(.x - Clubs), .names = "{col}_Clubs")) %>%
          mutate(Design_Brain = Design - Brain) %>%
          select(!Control:Clubs) %>%
          pivot_longer(cols = 2:7, names_to = "Contrast", values_to = "diff") %>% 
          mutate(Contrast = factor(Contrast, 
                                   levels = c("Brain_Control",
                                              "Design_Control",
                                              "Clubs_Control",
                                              "Brain_Clubs",
                                              "Design_Clubs",
                                              "Design_Brain"),
                                   ordered = T))
      }
    )
  )


# Thresholds or cutpoints along the Latent Distributions  -----------------------------------------------
# (estimated points between response levels)

posterior <- 
  posterior %>%
  mutate(
    # "Beta" can be understood as the difference between the latent mean of the reference group
    # and the experimental group
    cutpoints = purrr::map(
      .x = draws,
      .f =  function(.x) {
        .x %>%
          select(.draw, k, cutpoints) %>%
          ungroup() %>%
          arrange(.draw)
      }
    )
  )


# Probability of Each Response Level by Group -----------------------------------------------------------

# I also want the posterior probabilities of choosing each response level.
# This takes quite a lot of data wrangling

# Old / Test code
# posterior$draws[[1]] %>%
#   ungroup() %>%
#     mutate(Control_q = pnorm(cutpoints, 0, 1),
#            Brain_q   = pnorm(cutpoints, mean = Brain, sd = Brain_sd),
#            Design_q  = pnorm(cutpoints, mean = Design, sd = Design_sd),
#            Clubs_q   = pnorm(cutpoints, mean = Clubs, sd = Clubs_sd)) %>%
#     # This nests the cols and prepares them for another iteration through purrr
#     group_by(.draw) %>%
#     summarise(Control = list(Control_q),
#               Brain = list(Brain_q),
#               Design = list(Design_q),
#               Clubs = list(Clubs_q)) 

# Organise the data frame with list cols to pump it through purrr.
posterior <- 
  posterior %>%
  mutate(q_k = purrr::map(
    .x = draws,
    .f = function(.x) {
      .x %>%
        ungroup() %>%
        mutate(Control_q = pnorm(cutpoints, 0, 1),
               Brain_q   = pnorm(cutpoints, mean = Brain, sd = Brain_sd),
               Design_q  = pnorm(cutpoints, mean = Design, sd = Design_sd),
               Clubs_q   = pnorm(cutpoints, mean = Clubs, sd = Clubs_sd)) %>%
        # This nests the cols and prepares them for another iteration through purrr
        group_by(.draw) %>%
        summarise(Control = list(Control_q),
                  Brain = list(Brain_q),
                  Design = list(Design_q),
                  Clubs = list(Clubs_q)) 
    }
  )
  )

# Each of the values in this column represents the probability value of selecting at or below each threshold
# Each uses a different mean and standard deviation for the relevant group.
# I've nested over these by draw
posterior$q_k[[1]]

# Each cell contains 5 cumulative probability values.
posterior$q_k[[1]]$Control[[1]]

# Cumulative Probs --------------------------------------------------------------------------------------

# We know that the probability of selecting the top item or less, is the same as selecting any item
# So this must be 1, that gives us the full set of cumulative probabilities.
# So the next step is to add that
# This takes a moment as there are two levels of iteration and a substantial amount of info

posterior <- 
  posterior %>%
  mutate(
    c_p = purrr::map(
      .x = q_k,
      .f = function(.x) {
        
        .x %>%
          pivot_longer(Control:Clubs, names_to = "Group", values_to = "q_k") %>%
          group_by(.draw, Group) %>%
          mutate(
            # Add 1, to final position for cumulative probabilities:
            c_p = purrr::map(.x = q_k, .f = ~c(.x, 1)),
          ) %>%
          mutate(Group = factor(Group, levels = these_groups, ordered = T), .after = .draw) %>%
          select(.draw, Group, c_p) %>%
          ungroup()
      })
  )

# Probability by response level ---------------------------------------------------------------------------

# So we now have all 6 cumulative probabilities:
# But we want the probabilities of selecting *between* each of these thresholds, this will give us the 
# probability of each response level by group
# This can be achieved by subtracting the cumulative value at k-1, from the value at k
# Or 0 in the case of the first value.
# In rough pseudo code something like p_{k} - p_{k-1} | if (k == 1) (p_{k} - 0)
## Again there is nested iteration here so it takes a while to run
posterior <- 
  posterior %>%
  mutate(
    p = purrr::map(
      .x = q_k,
      .f = function(.x) {
        .x %>%
          pivot_longer(Control:Clubs, names_to = "Group", values_to = "q_k") %>%
          group_by(.draw, Group) %>%
          # p_{k} - p_{k-1} | if (k == 1) (p_{k} - 0)
          mutate(p   = purrr::map(.x = q_k, .f = ~(c(.x, 1) - c(0, .x)))) %>%  
          mutate(Group = factor(Group, levels = these_groups, ordered = T), .after = .draw) %>%
          select(.draw, Group, p) %>%
          ungroup()
      }
    )
  )

# Add response level to p and c_p -----------------------------------------------------------------------

# I'd like to add the response values to each of the c_p and p values. 
# This will make lining them up while plotting a little easier.
# I also unnest here which can be a little slow.

# Cumulative Probs
posterior <- 
  posterior %>%
  mutate(
    c_p = purrr::map(
      .x = c_p,
      .f = function(.x) {
        .x %>% 
          mutate(Response_N = list(1:6),
                 Response = list(factor(1:6, levels = 1:6, labels = this_scale, ordered = T)),
                 .after = .draw) %>%
          unnest(cols = c(c_p, Response_N, Response))
      }
    )
  )

# Probs
posterior <- 
  posterior %>%
  mutate(
    p = purrr::map(
      .x = p,
      .f = function(.x) {
        .x %>% 
          mutate(Response_N = list(1:6),
                 Response = list(factor(1:6, levels = 1:6, labels = this_scale, ordered = T)),
                 .after = .draw) %>%
          unnest(cols = c(p, Response_N, Response))
      }
    )
  )

# Expected Value by Group -------------------------------------------------------------------------------

# Now we can compute the expected value for each draw by group:
posterior <- 
  posterior %>%
  mutate(
    EV = purrr::map(
      .x = p,
      .f = function(.x) {
        .x %>%
          mutate(EV = p * Response_N) %>%
          group_by(.draw, Group) %>%
          summarise(EV = sum(EV), .groups = "drop")
      }
    )
  )

# We can also get contrasts for these
posterior <- 
  posterior %>%
  mutate(
    EV_diffs = purrr::map(
      .x = EV,
      .f = function(.x) {
        .x %>%
          pivot_wider(names_from = Group, values_from = EV) %>%
          mutate(across(Brain:Clubs, .fns = ~(.x - Control), .names = "{col}_Control")) %>%
          mutate(across(Brain:Design, .fns = ~(.x - Clubs), .names = "{col}_Clubs")) %>%
          mutate(Design_Brain = Design - Brain) %>%
          select(!Control:Clubs) %>%
          pivot_longer(cols = 2:7, names_to = "Contrast", values_to = "diff") %>% 
          mutate(Contrast = factor(Contrast, 
                                   levels = c("Brain_Control",
                                              "Design_Control",
                                              "Clubs_Control",
                                              "Brain_Clubs",
                                              "Design_Clubs",
                                              "Design_Brain"),
                                   ordered = T))
      }
    )
  )

# Effect Sizes ------------------------------------------------------------------------------------------

# This is the primary between group measure I report in the paper.

# Create a function for Cohen's d with pooled standard dev
effSize <- function(m1, m2, sd1, sd2) {
  (m1 - m2) / sqrt((sd1^2 + sd2^2)/2)
}

posterior <- 
  posterior |> 
  mutate(ES = purrr::map(
    .x = draws,
    .f = function(x) {
      x |> 
        ungroup() |> 
        filter(k == 1) |> 
        mutate(
          Control_Brain  = effSize(m1 = 0, m2 = Brain,  sd1 = 1, sd2 = Brain_sd),
          Control_Design = effSize(m1 = 0, m2 = Design, sd1 = 1, sd2 = Design_sd),
          Control_Clubs  = effSize(m1 = 0, m2 = Clubs,  sd1 = 1, sd2 = Clubs_sd),
          Clubs_Brain    = effSize(m1 = Clubs, m2 = Brain , sd1 = Clubs_sd, sd2 = Brain_sd),
          Clubs_Design   = effSize(m1 = Clubs, m2 = Design, sd1 = Clubs_sd, sd2 = Design_sd),
          Brain_Design   = effSize(m1 = Brain, m2 = Design, sd1 = Brain_sd, sd2 = Design_sd)) |> 
        select(!c(k, cutpoints, .iteration, .chain, Brain:Clubs_sd)) |> 
        pivot_longer(cols = Control_Brain:Brain_Design, names_to = "Contrast", values_to = "ES") |> 
        mutate(Contrast = factor(Contrast, levels = c("Control_Brain", 
                                                      "Control_Design", 
                                                      "Control_Clubs", 
                                                      "Clubs_Brain", 
                                                      "Clubs_Design", 
                                                      "Brain_Design")))
    }
  ))

# Model Summary Tables --------------------------------------------------------------------------------------

# Next I want to fill out all the summary tables:
posterior %>% glimpse()

# We have four types of summary patterns.
# 1. Estimates grouped by experimental group and control (EV) (4 rows)
# 2. Estimates grouped by experimental group, where control is set (e.g. Mean, and SD) (3 rows)
# 3. Estimates grouped by contrast (6 rows)
# 4. Estimates grouped by experimental group AND item response level (e.g p, & c_p) 4 x 6 = 24 rows

posterior %>%
  # Pattern 1
  mutate(EV = purrr::map(
    .x = EV,
    .f = ~.x %>% 
      group_by(Group) %>% 
      point_interval(.interval = hdi))) %>%
  # Pattern 2
  mutate(
    across(
      .cols = c(latentMean, SD),
      .fns = ~purrr::map(.x = .x, 
                         .f = ~.x %>%
                           filter(Group != "Control") %>% 
                           group_by(Group) %>% 
                           median_hdi(.width = .95)))) %>%
  # Pattern 3
  mutate(
    across(
      .cols = contains("_diffs"),
      .fns = ~purrr::map(.x = .x, 
                         .f = ~.x %>%
                           group_by(Contrast) %>% 
                           median_hdi(.width = .95)))) %>%
  # Pattern 4
  mutate(
    across(
      .cols = c(c_p, p),
      .fns = ~purrr::map(.x = .x, 
                         .f = ~.x %>%
                           select(!Response_N) %>%
                           group_by(Group, Response) %>% 
                           median_hdi(.width = .95)))) %>%
  select(Item, Description, latentMean, SD, EV, contains("_diffs"), c_p, p) -> posterior_summaries

# Confirm row order is the same
# all(posterior$Item == posterior_summaries$Item)

# Now standard effect size.
posterior %>%
  # Pattern 1
  mutate(ES = purrr::map(
    .x = ES,
    .f = function(x) {
      x |> 
        group_by(Contrast) |> 
        median_hdi(ES, .width = .95)
    })) |> 
  pull(ES) -> posterior_summaries$ES

# Confirm row order is the same
# all(posterior$Item == posterior_summaries$Item)

# Plots -------------------------------------------------------------------------------------------------

# Note that these plots were primary used for early data exploration and interpretation
# In the end I came up with something a little different for the paper

# To generate the plots used in the paper see this script:
here("Analysis/Results/Plots/APPS_Plots.R")

# Edits to ggplot theme
theme_fix <- function() {
  theme_bw() %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          text = element_text(size = 16),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title = element_text(size = 13),
          plot.title = element_text(hjust = 0, margin = margin(b = 10)),
          plot.subtitle = element_text(hjust = 0, margin = margin(b = 10), size = 14),
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
rm("theme_fix")

# A colour scheme for plotting groups
APPS_ColourScheme <- list(
  
  # Experimental Groups
  Control = "grey40",
  Brain = "goldenrod1", 
  Design = "skyblue1",
  Clubs =  "firebrick1",
  
  # Contrast Colour Scheme
  Brain_Control = "goldenrod1", 
  Design_Control = "skyblue1",
  Clubs_Control =  "firebrick1",
  Brain_Clubs = "#FF6F00",
  Design_Clubs =  "#8E24AA",
  Design_Brain =   "#8BC34A",
  
  # Contrast Colour Scheme
  Control_Brain = "goldenrod1", 
  Control_Design = "skyblue1",
  Control_Clubs =  "firebrick1",
  Clubs_Brain = "#FF6F00",
  Clubs_Design =  "#8E24AA",
  Brain_Design =   "#8BC34A")

## Generate Threshold Plots ----------------------------------------------------------------------------

posterior_plots <- d[, c(1, 2)]

posterior_plots$ThresholdPlot <-
  purrr::map2(
    .x = posterior$draws,
    .y = posterior$Description,
    .f = function(.x, .y) {
      # Create a data frame containing threshold plot parameters:
      tibble(group = factor(1:4, levels = 1:4, labels = c("Control", "Brain", "Design", "Clubs"), ordered = T),
             # I've used the median to get the point estimate
             # 0 is latent mean for the control group (comes from model structure).
             mean  = c(0, median(.x$Brain), median(.x$Design), median(.x$Clubs)),
             sd    = c(1, median(.x$Brain_sd), median(.x$Design_sd), median(.x$Clubs_sd))) %>%
        # Create a grid to sample over
        mutate(x = list(seq(-3, 3, length.out = 100))) %>%
        mutate(y = purrr::pmap(.l = list(x, mean, sd), dnorm)) %>%
        unnest(cols = c(x, y)) -> plotDATA
      
      # Threshold plot with latent mean
      ggplot(data = plotDATA) +
        xlim(-3, 3) +
        geom_vline(xintercept = .x %>% group_by(k) %>% point_interval(cutpoints) %>% pull(cutpoints),
                   linetype = "dotted") +
        geom_line(mapping = aes(x = x, y = y, colour = group), 
                  size = 1) +
        # Geom line hates lists
        scale_colour_manual(values = unlist(APPS_ColourScheme[1:4])) +
        labs(title = "Thresholds and Latent Mean",
             subtitle = paste(strwrap(.y, width = 90), collapse = "\n"),
             colour = NULL,
             caption = "Dotted lines display response level cutpoints",
             x = "Standardised Units",
             y = "Density") -> output
      
      return(output)
    }
  )

## Parameter Plots -------------------------------------------------------------------------------------

# Latent Mean
posterior %>%
  mutate(latentMean = purrr::map(
    .x = latentMean,
    .f = function(.x) {
      
      .x %>%
        filter(Group != "Control") %>% 
        ggplot() +
        stat_slab(mapping = aes(x = latentMean, 
                                y = Group, 
                                group = Group, 
                                fill = Group),
                  alpha = .5) +
        geom_pointrange(data = . %>% 
                          group_by(Group) %>%
                          point_interval(latentMean, .width = c(.95), .interval = hdi),
                        mapping = aes(x = latentMean, xmin = .lower, xmax = .upper, y = Group)) +
        geom_linerange(data = . %>% 
                         group_by(Group) %>%
                         point_interval(latentMean, .width = c(.67), .interval = hdi),
                       mapping = aes(xmin = .lower, xmax = .upper, y = Group),
                       size = 1.25) +
        scale_fill_manual(values = APPS_ColourScheme[2:4])
    }
  )
  ) %>%
  select(Item, Description, latentMean) %>%
  left_join(posterior_plots, .) -> posterior_plots

## Standard Deviation
posterior %>%
  mutate(SD = purrr::map(
    .x = SD,
    .f = function(.x) {
      
      .x %>%
        filter(Group != "Control") %>%
        ggplot() +
        stat_slab(mapping = aes(x = SD, 
                                y = Group, 
                                group = Group, 
                                fill = Group),
                  alpha = .5) +
        geom_pointrange(data = . %>% 
                          group_by(Group) %>%
                          point_interval(SD, .width = c(.95), .interval = hdi),
                        mapping = aes(x = SD, xmin = .lower, xmax = .upper, y = Group)) +
        geom_linerange(data = . %>% 
                         group_by(Group) %>%
                         point_interval(SD, .width = c(.67), .interval = hdi),
                       mapping = aes(xmin = .lower, xmax = .upper, y = Group),
                       size = 1.25) +
        scale_fill_manual(values = APPS_ColourScheme[2:4])
    }
  )
  ) %>%
  select(Item, Description, SD) %>%
  left_join(posterior_plots, .) -> posterior_plots

### Expected Value by Group

# Expected Value
posterior %>%
  mutate(EV = purrr::map(
    .x = EV,
    .f = function(.x) {
      
      .x %>%
        ggplot() +
        stat_slab(mapping = aes(x = EV, 
                                y = Group, 
                                group = Group, 
                                fill = Group),
                  alpha = .5) +
        geom_pointrange(data = . %>% 
                          group_by(Group) %>%
                          point_interval(EV, .width = c(.95), .interval = hdi),
                        mapping = aes(x = EV, xmin = .lower, xmax = .upper, y = Group)) +
        geom_linerange(data = . %>% 
                         group_by(Group) %>%
                         point_interval(EV, .width = c(.67), .interval = hdi),
                       mapping = aes(xmin = .lower, xmax = .upper, y = Group),
                       size = 1.25) +
        scale_fill_manual(values = APPS_ColourScheme[1:4])
    }
  )
  ) %>%
  select(Item, Description, EV) %>%
  left_join(posterior_plots, .) -> posterior_plots

## Plot Contrasts ----------------------------------------------------------------------------------------

# Diffs
posterior %>%
  mutate(
    across(
      .cols = contains("_diffs"),
      .fns = ~purrr::map(.x = .x, 
                         .f = ~.x %>%
                           ggplot() +
                           stat_slab(mapping = aes(x = diff, 
                                                   y = Contrast, 
                                                   group = Contrast, 
                                                   fill = Contrast),
                                     show.legend = FALSE,
                                     alpha = .5) +
                           geom_pointrange(data = . %>% 
                                             group_by(Contrast) %>%
                                             point_interval(diff, .width = c(.95), .interval = hdi),
                                           mapping = aes(x = diff, xmin = .lower, xmax = .upper, y = Contrast)) +
                           geom_linerange(data = . %>% 
                                            group_by(Contrast) %>%
                                            point_interval(diff, .width = c(.67), .interval = hdi),
                                          mapping = aes(xmin = .lower, xmax = .upper, y = Contrast),
                                          size = 1.25) +
                           scale_fill_manual(values = APPS_ColourScheme)
      )
    )
  ) %>%
  select(Item, Description, contains("_diffs")) %>%
  left_join(posterior_plots, .) -> posterior_plots

# Effect Size
posterior %>%
  mutate(ES = purrr::map(.x = ES, 
                         .f = ~.x %>%
                           ggplot() +
                           theme_bw() +
                           stat_slab(mapping = aes(x = ES, 
                                                   y = Contrast, 
                                                   group = Contrast, 
                                                   fill = Contrast),
                                     show.legend = FALSE,
                                     alpha = .5) +
                           geom_pointrange(data = . %>% 
                                             group_by(Contrast) %>%
                                             median_hdi(ES, .width = c(.95)),
                                           mapping = aes(x = ES, xmin = .lower, xmax = .upper, y = Contrast)) +
                           geom_linerange(data = . %>% 
                                            group_by(Contrast) %>%
                                            median_hdi(ES, .width = c(.67)),
                                          mapping = aes(xmin = .lower, xmax = .upper, y = Contrast),
                                          size = 1.25) +
                           scale_fill_manual(values = APPS_ColourScheme))
  ) %>%
  select(Item, Description, ES) %>%
  left_join(posterior_plots, .) -> posterior_plots

## Add titles to all plots -------------------------------------------------------------------------------

posterior_plots <- 
  posterior_plots %>%
  mutate(across(.cols = latentMean:ES, 
                .fns = ~purrr::map2(
                  .x = .x,
                  .y = Description,
                  .f = function(.x, .y) {
                    
                    thisTitle <- 
                      case_when(
                        cur_column() == "latentMean" ~ "Posterior Estimate: Latent Mean",
                        cur_column() == "SD" ~ "Posterior Estimate: Latent Standard Deviation",
                        cur_column() == "EV" ~ "Posterior Estimate: Group Expected Value (Outcome Scale)",
                        cur_column() == "LM_diffs" ~ "Contrast Posterior Estimate: Latent Mean",
                        cur_column() == "SD_diffs" ~ "Contrast Posterior Estimate: Standard Deviation",
                        cur_column() == "EV_diffs" ~ "Contrast Posterior Estimate: Expected Value",
                        cur_column() == "ES" ~ "Contrast Posterior Estimate: Effect Size"
                      )
                    
                    output <- .x +
                      labs(title = thisTitle,
                           subtitle = paste(strwrap(.y, width = 90), collapse = "\n"),
                           x = NULL,
                           y = NULL,
                           colour = NULL,
                           fill = NULL,
                           caption = "
                           Error bars represent 67% and 95% highest density posterior interval
                           Point estimate is the median")
                    
                    return(output)
                    
                  }
                )
  )
  )

## Probability Plots -------------------------------------------------------------------------------------

# For the next two plots I want to include the observed proportion alongside the model estimates
# We can derive these as follows:

# Observed Response Proportions by Group 
d <- 
  d %>%
  mutate(Summary = purrr::map(
    .x = data,
    .f = function(.x) {
      .x %>%
        select(Group, Response) %>%
        group_by(Group) %>%
        count(Response) %>%
        mutate(Proportion = (n / sum(n)),
               Cummulative_Proportion = cumsum(Proportion))
    }
  ))

# Probability at each Response Levels by Group
posterior_plots$p <- 
  purrr::pmap(
    .l = list(.x = posterior_summaries$p, .y = d$Summary, .t = posterior$Description),
    .f = function(.x, .y, .t) {
      ggplot() +
        geom_pointrange(data = .x, 
                        mapping = aes(y = p, ymin = .lower, ymax = .upper, x = Response, group = Group, colour = Group),
                        position = position_dodge(width = .25)) +
        geom_point(data = .y,
                   mapping = aes(group = Group, x = Response, y = Proportion),
                   position = position_dodge(width = .25),
                   shape = 4) +
        scale_colour_manual(values = c("grey", "goldenrod1", "skyblue1", "firebrick1")) +
        labs(title = "Posterior Estimates for Probability at each Response Level",
             subtitle = paste(strwrap(.t, width = 90), collapse = "\n"),
             x = NULL,
             y = "Probability / Proportion",
             colour = NULL,
             caption = "
         Error bars represent 95% highest posterior density interval
         Point estimate is the median
         Black cross indicates observed proportion
         ") 
    }
  )

# Cumulative Probability
posterior_plots$c_p <- 
  purrr::pmap(
    .l = list(.x = posterior_summaries$c_p, .y = d$Summary, .t = posterior$Description),
    .f = function(.x, .y, .t) {
      ggplot() +
        # Draw a line a majority support
        geom_hline(yintercept = .5, colour = "grey") +
        scale_y_continuous(breaks = (0:10)/10, limits = c(0, 1)) +
        # geom line was picky about lists >:(
        scale_colour_manual(values = c(APPS_ColourScheme$Control, 
                                       APPS_ColourScheme$Brain, 
                                       APPS_ColourScheme$Design, 
                                       APPS_ColourScheme$Clubs)) +
        # Plot cumulative probability
        geom_line(data = .x,
                  mapping = aes(colour = Group,
                                group = Group,
                                x = Response,
                                y = c_p),
                  position = position_dodge(width = .2)) +
        geom_errorbar(data = .x,
                      mapping = aes(group = Group,
                                    colour = Group,
                                    x = Response,
                                    ymin = .lower,
                                    ymax = .upper),
                      width = .1,
                      position = position_dodge(width = .2)) +
        geom_point(data = .x,
                   mapping = aes(group = Group,
                                 colour = Group,
                                 x = Response,
                                 y = c_p),
                   size = 2.25,
                   position = position_dodge(width = .2)) +
        geom_point(data = .y,
                   mapping = aes(x = Response, y = Cummulative_Proportion, group = Group),
                   colour = "black",
                   size = 2.25,
                   position = position_dodge(width = .2),
                   shape = 1) +
        labs(title = "Posterior Estimates of Cummulative Probabilities",
             subtitle = paste(strwrap(.t, width = 90), collapse = "\n"),
             x = NULL,
             colour = NULL,
             y = "Cummulative Probability / Proportion",
             caption = "
       Error bars = 95% highest posterior density interval.
       Coloured point estimate = median of posterior estimates.
       Black = observed proportion in data.
       ") 
    }
  )

models <- d %>% select(!data)

write_rds(x = models, file = here("Analysis/Models/Output/CM-Probit/Policy_CM-Probit_models.rds"))
write_rds(x = posterior, file = here("Analysis/Models/Output/CM-Probit/Policy_CM-Probit_posterior.rds"))
write_rds(x = posterior_summaries, file = here("Analysis/Models/Output/CM-Probit/Policy_CM-Probit_posterior_summaries.rds"))

# Exporting the plots tibble results in an enormous (> 10 GB) data file. So I'll opt to save some vector 
# graphics instead

count_item <- length(unique(posterior_plots$Item))

posterior_plots$Item_N <- 1:nrow(posterior_plots)

posterior_plots <- 
  pivot_longer(posterior_plots, 
               cols = c("ThresholdPlot", "latentMean", "SD", "EV", "LM_diffs", "EV_diffs", "p", "c_p", "ES"),
               names_to = "Param",
               values_to = "Plot")

filenames <- apply(posterior_plots, MARGIN = 1, FUN = function(x) {
  paste(x$Item_N, x$Item, x$Param, Sys.Date(), sep = "_")
  })

path <- here("Analysis/Models/Output/Plots/")

lapply(seq_along(posterior_plots$Plot), function(i) {
  ggsave(path = path,
         filename = paste0(filenames[i], ".svg"),
         plot = posterior_plots$Plot[[i]],
         device = "svg") #use your settings here
})

# Reverse scoring for national standard items -----------------------------------------------------------

# This script re-analyses two national standard items with the response variable reverse coded (i.e. starting at
# Strongly Disagree). This was primarily useful for plotting, and does not substantially change inferences. 
# It's mostly re-used code from the sections above so the commenting below might be a little off

# Wrangle data for analysis -----------------------------------------------------------------------------

d <- data

# The National Standard Items had to be treated differently because of the I don't know responses:
vars1 <- c("numericID",
           "Group",
           "NS_Fair",
           "NS_Display",
           "NS_Title")

models <- 
  d %>%
  select(all_of(vars1)) %>%
  pivot_longer(cols = !c(numericID, Group), values_to = "Response", names_to = "Item") %>%
  # Tidy up the item names
  mutate(Item = str_remove(Item, pattern = "NS_")) %>%
  mutate(Item = str_replace(Item, pattern = "Title", replacement = "Mislead")) %>%
  mutate(Response = factor(Response, exclude = "I Don't Know")) %>%
  # Drop I Don't Know Responses
  na.omit()

# Nest Data
models <- 
  models %>%
  group_by(Item) %>%
  nest()

models[3, ]$data[[1]]

# Re-order Mislead Item
models <- 
  models |> 
  mutate(data = purrr::map2(
    .x = Item, 
    .y = data,
    .f = ~if(.x == "Mislead") {
      .y$Response <- fct_rev(.y$Response)
      .y$Response_N <- as.numeric(.y$Response)
      .y
    } else {
      .y$Response_N <- as.numeric(.y$Response)
      .y
    }
  ))

# Fit Models --------------------------------------------------------------------------------------------

# Now we are going to run the models (this will take a long time). 
# I won't write a parallel loop for this script, because I want to be able to use all cores while fitting models.
# The script below iterates through this data frame and saves the model output in another list column
# Each model takes ~ 30 seconds to set-up and then fit (on my machine).
models <- 
  models%>%
  mutate(
    Model = purrr::map(
      .x = data, 
      # Bayesian Ordered Probit Model + Unequal Variances (Probit)
      .f = ~brm(
        # Our linear model
        formula = bf(Response ~ 1 + Group) +
          # This allows us to model unequal variances between groups
          lf(disc ~ 0 + Group, cmc = FALSE),
        # Data iterated in using purrr
        data = .x,
        # Probit link 
        family = cumulative("probit"),
        # Set Priors
        prior = c(prior(normal(0, 1), class = Intercept),       # Flat prior across most of the probability space.
                  prior(normal(0, 0.5), class = b),             # Mild regularisation, sceptical of very large effects
                  prior(normal(0, 1), class = b, dpar = disc)), # Prior for disc b values
        # MCMC set-up
        iter = 2250, warmup = 1000, cores = 8, chains = 8)
    )
  )

models <- 
  models %>% 
  mutate(
    # Unequal Variance Model
    Model = purrr::map(
      .x = Model, 
      .f = ~add_criterion(.x, overwrite = TRUE, criterion = "loo")))

# Posterior draws ---------------------------------------------------------------------------------------
# Get posterior draws
models <- 
  models %>%
  mutate(posterior = purrr::map(
    .x = Model,
    .f = ~spread_draws(.x$fit,
                       b_Intercept[k],
                       b_GroupBrain, 
                       b_GroupDesign, 
                       b_GroupClubs, 
                       b_disc_GroupBrain, 
                       b_disc_GroupDesign, 
                       b_disc_GroupClubs)
  ))

# Tidy column names
models <- 
  models %>% 
  mutate(
    posterior = purrr::map(
      .x = posterior,
      .f = function(.x) {
        .x %>% # Take posterior for item i
          # Drop preceeding brms noise
          rename_with(.cols = contains("b_Group"), .fn = ~str_remove(.x, "b_Group")) %>%
          rename_with(.cols = contains("b_disc"), .fn = ~paste(str_remove(.x, "b_disc_Group"), "_sd", sep = "")) %>%
          rename(cutpoints = "b_Intercept")
      }
    )
  )

# Get Standard Deviation --------------------------------------------------------------------------------

# The _disc parameter is the log of the inverse standard deviation. 
# To recover the SD we need to exponentiate the parameter and take it's inverse.

models <- 
  models %>% 
  mutate(
    posterior = purrr::map(
      .x = posterior,
      .f = function(.x) {
        .x %>% # Take posterior for item i
          mutate(across(.cols = contains("_sd"), .fns = ~(1 / exp(.x))))
      }
    )
  )

## Beta / Mean of the Latent Distributions ---------------------------------------------------------------
models$posterior[[1]] %>%
  # Beta values are the same st each threshold point, so we need to filter the data to drop dupes
  filter(k == 1) %>%
  ungroup() %>%
  # These estimates represent differences from the Control mean, so we want to reverse the sign
  mutate(across(Brain:Clubs, .fns = ~ .x * - 1)) %>% 
  select(.draw:Clubs) %>%
  # It will be useful to have the Control group marked here too
  # The model assumes a standard distribution dnorm(0, 1) for the reference group.
  mutate(Control = 0, .before = Brain) %>%
  pivot_longer(cols = c(Control:Clubs),
               names_to = "Group",
               values_to = "Mean") %>%
  mutate(Group = factor(Group, 
                        levels = c("Control", "Brain", "Design", "Clubs"), 
                        ordered = T))

models <- 
  models %>%
  mutate(
    # "Beta" can be understood as the difference between the latent mean of the reference group
    # and the experimental group
    long = purrr::map(
      .x = posterior,
      .f = function(.x) {
        .x %>%
          # Beta values are the same st each threshold point, so we need to filter the data to drop dupes
          filter(k == 1) %>%
          ungroup() %>%
          select(.draw:Clubs) %>%
          # It will be useful to have the Control group marked here too
          # The model assumes a standard distribution dnorm(0, 1) for the reference group.
          mutate(Control = 0, .before = Brain) %>%
          pivot_longer(cols = c(Control:Clubs),
                       names_to = "Group",
                       values_to = "Mean") %>%
          mutate(Group = factor(Group, 
                                levels = c("Control", "Brain", "Design", "Clubs"), 
                                ordered = T))
      }
    )
  )

# Standard Deviation of the Latent Distributions --------------------------------------------------------

models <- 
  models %>%
  mutate(
    long = purrr::map2(
      .x = posterior,
      .y = long,
      .f = function(.x, .y) {
        .x %>%
          # SD values are the same st each threshold point, so we need to filter the data to drop dupes
          filter(k == 1) %>%
          # Drop k grouping
          ungroup() %>%
          # Select cols
          select(.draw, Brain_sd:Clubs_sd) %>%
          # Again this is a feature of the model
          mutate(Control_sd = 1, .before = Brain_sd) %>%
          # Long form is more useful for summaries and plotting
          pivot_longer(cols = c(Control_sd:Clubs_sd),
                       names_to = "Group",
                       # This will drop the suffix "_sd"
                       names_pattern = "(.*)(?=_sd)",
                       values_to = "SD") %>%
          # Re factor the data
          mutate(Group = factor(Group, 
                                levels = c("Control", "Brain", "Design", "Clubs"), 
                                ordered = T)) -> f.sds
        
        return(left_join(.y, f.sds, by = c(".draw", "Group")))
      }
    )
  )

# Effect Size estimates -----------------------------------------------------------
# We can compute a standardised effect size using the pooled SD as follows:
# (mu1 - mu2) / sqrt((SD_1^2 + SD_1^2)/2)
# Our control mean is 0 and sd is 1.

f.ES <- function(mu1, sd1, mu2 = 0, sd2 = 1) {
  (mu1 - mu2) / sqrt((sd1^2 + sd2^2)/2)
}

models <- 
  models %>% 
  mutate(long = purrr::map(
    .x = long,
    .f = function(.x) {
      .x %>% 
        mutate(ES = purrr::map2_dbl(
          .x = Mean,
          .y = SD,
          .f = ~f.ES(.x, .y)
        ))
    }
  ))

# Thresholds or cutpoints along the Latent Distributions  -----------------------------------------------
# (estimated points between response levels)

models <- 
  models %>%
  mutate(
    # "Beta" can be understood as the difference between the latent mean of the reference group
    # and the experimental group
    cutpoints = purrr::map(
      .x = posterior,
      .f =  function(.x) {
        .x %>%
          select(.draw, k, cutpoints) %>%
          ungroup() %>%
          arrange(.draw)
      }
    )
  )

# Probability of Each Response Level by Group -----------------------------------------------------------
models <- 
  models %>%
  mutate(q_k = purrr::map(
    .x = posterior,
    .f = function(.x) {
      .x %>%
        ungroup() %>%
        mutate(Control_q = pnorm(cutpoints, 0, 1),
               Brain_q   = pnorm(cutpoints, mean = Brain, sd = Brain_sd),
               Design_q  = pnorm(cutpoints, mean = Design, sd = Design_sd),
               Clubs_q   = pnorm(cutpoints, mean = Clubs, sd = Clubs_sd)) %>%
        # This nests the cols and prepares them for another iteration through purrr
        group_by(.draw) %>%
        summarise(Control = list(Control_q),
                  Brain = list(Brain_q),
                  Design = list(Design_q),
                  Clubs = list(Clubs_q)) 
    }
  )
  )

# Each of the values in this column represents the probability value of selecting at or below each threshold
# Each uses a different mean and standard deviation for the relevant group.
# I've nested over these by draw
models$q_k[[1]]

# Each cell contains 5 cumulative probability values.
models$q_k[[1]]$Control[[1]]

# Cumulative Probs --------------------------------------------------------------------------------------

# We know that the probability of selecting the top item or less, is the same as selecting any item
# So this must be 1, that gives us the full set of cumulative probabilities.
# So the first step is to add that information
# This takes a moment as there are two levels of iteration and a substantial amount of info

models <- 
  models %>%
  mutate(
    c_p = purrr::map(
      .x = q_k,
      .f = function(.x) {
        .x %>%
          pivot_longer(Control:Clubs, names_to = "Group", values_to = "q_k") %>%
          group_by(.draw, Group) %>%
          mutate(
            # Add 1, to final position for cumulative probabilities:
            c_p = purrr::map(.x = q_k, .f = ~c(.x, 1)),
          ) %>%
          mutate(Group = factor(Group, levels = c("Control", "Brain", "Design", "Clubs"), ordered = T), .after = .draw) %>%
          select(.draw, Group, c_p) %>%
          ungroup()
      })
  )

this_scale <- levels(models$data[[1]]$Response)

# Add Response Level
models[1:2, ] <- 
  models[1:2, ] %>%
  mutate(
    c_p = purrr::map(
      .x = c_p,
      .f = function(.x) {
        .x %>% 
          mutate(Response_N = list(1:6),
                 Response = list(factor(1:6, levels = 1:6, labels = this_scale, ordered = T)),
                 .after = .draw) %>%
          unnest(cols = c(c_p, Response_N, Response))
      }
    )
  )

# Reversed levels
models[3, ] <- 
  models[3, ] %>%
  mutate(
    c_p = purrr::map(
      .x = c_p,
      .f = function(.x) {
        .x %>% 
          mutate(Response_N = list(1:6),
                 Response = list(factor(1:6, levels = 6:1, labels = this_scale, ordered = T)),
                 .after = .draw) %>%
          unnest(cols = c(c_p, Response_N, Response))
      }
    )
  )


models <- 
  models %>% 
  mutate(
    c_p = purrr::map(
      .x = c_p,
      .f = function(.x) {
        .x %>%
          group_by(.draw, Group) %>% 
          summarise(
            Response_N = list(Response_N) , 
            Response = list(Response), 
            c_p = list(c_p)) %>% 
          # p_{k}
          mutate(p   = purrr::map(.x = c_p, .f = ~(c(.x) - c(0, .x[1:5])))) %>%  
          unnest(cols = c(Response_N, Response, c_p, p)) %>% 
          # Reverse cumulative prob
          mutate(c_pr = (1 - c_p) + p) %>% 
          ungroup()
      }
    )
  )

# Observed Data Distribution ----------------------------------------------------------------------------
models <- 
  models %>%
  mutate(data_sum = purrr::map(
    .x = data,
    .f = function(.x) {
      .x %>%
        select(Group, Response) %>%
        group_by(Group) %>%
        count(Response) %>%
        mutate(p = (n / sum(n)),
               c_p = cumsum(p),
               c_pr = (1 - c_p) + p)
    }
  ))

models <- models %>% select(Item, Model, posterior, long, c_p, data_sum)

# Save Models and Output
write_rds(x = models, file = here("Analysis/Models/Output/CM-Probit/APPS_NS_Reverse_CM-Probit.rds"))

# And we're done.
# Last run 2022-02-22
# That spread_draws() bug doesn't seem to be effecting brms model output. 


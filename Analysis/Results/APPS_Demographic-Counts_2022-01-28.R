# Compose a table for participant demographics

# Functional
library(here)
library(datapasta)

# The basics
library(tidyverse)

# Data wrangling
library(data.table)

# For nice tables
library(knitr)
library(huxtable)

# Load data
load(file = here("Analysis", "Data", "APPS_OSF-AnalysisData_2022-02-22.RData"))
# Load question dictionary
load(file = here("Analysis", "Data", "APPS_OSF-Questions_2022-02-22.RData"))

setDT(d)

glimpse(questions)
questions$Gambl_12mnts
# names(questions) |> dpasta()

# I'll only need a few of these columns
theseCols <- c("Age_quota",
               "Gender",
               "State2",
               "LocalArea",
               "Education",
               "Employment",
               "Income",
               "Pokies",
               "None",
               "PGSI_class")

# Drop unwanted cols
d[, names(questions)[!names(questions) %in% theseCols] := NULL]

# Reverse "No last year gambling"
d[, "Last_Year_Gambling" := !None]
d[, "None" := NULL]

# Factorise last year pokies use.
d$Pokies <- case_when(
  d$Pokies == 0 ~ "None",
  between(d$Pokies, 1, 10) == 1 ~ "1-10",
  between(d$Pokies, 11, 20) == 1 ~ "11-20",
  between(d$Pokies, 21, 49) == 1 ~ "21-49",
  d$Pokies >= 50 ~ "50+")

levels <- c("None","1-10","11-20","21-49","50+")
  
d[, "Pokies" := factor(Pokies, levels = levels, ordered = T)]

# warning here is fine, it's just telling us that it's coercing all values to character type
count <- melt(d, measure.vars = names(d))

count <- count[, .N, by = .(variable, value)][, "Percent" := round(N / nrow(d) * 100, 1)]

# Rename columns
setnames(count, names(count), c("Demographic", "Value", "Count", "Percent"))

# Reorder Demographics
levels <- c("State2",
            "LocalArea",
            "Age_quota",
            "Gender",
            "Education",
            "Employment",
            "Income",
            "Last_Year_Gambling",
            "Pokies",
            "PGSI_class")

count[, Demographic := factor(Demographic, levels = levels, ordered = T)]

count <- count[order(Demographic)]

kable(count)

# I'll format and order this table in word

count[Demographic == "Income", 
      .(Demographic,
        "Value" = factor(Value, 
                     levels = c("Negative Income",
                                "Nil Income",
                                "$1 - $99",
                                "$100 - $199",
                                "$200 - $299",
                                "$300 - $399",
                                "$400 - $599",
                                "$600 - $799",
                                "$800 - $999",
                                "$1,000 - $1,249",
                                "$1,250 - $1,499",
                                "$1,500 - $1,999",
                                "$2,000 - $2,499",
                                "$3,000 - $3,499",
                                "$3,500 - $3,999",
                                "$4,000 - $4,999",
                                "$5,000 or more",
                                "Prefer not to say"),
                     ordered = T),
        Count, Percent)][order(Value)] |> view()

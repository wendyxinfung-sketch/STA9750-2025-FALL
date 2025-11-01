# Acquire Data
if(!dir.exists(file.path("data", "mp01"))){
  dir.create(file.path("data", "mp01"), showWarnings=FALSE, recursive=TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")

if(!file.exists(GLOBAL_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv", 
                destfile=GLOBAL_TOP_10_FILENAME)
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

if(!file.exists(COUNTRY_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv", 
                destfile=COUNTRY_TOP_10_FILENAME)
}

# Data Import and Preparation
if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)

str(GLOBAL_TOP_10)

glimpse(GLOBAL_TOP_10)

# Data Cleaning 
GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title %in% c("N/A", ""), 
                                NA_character_, 
                                season_title))

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME, na = "N/A", show_col_types = FALSE) |>
  mutate(season_title = if_else(season_title == "", NA_character_, season_title))

format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

total_countries <- COUNTRY_TOP_10 |> distinct(country_iso2) |> nrow()
date_range <- paste(min(GLOBAL_TOP_10$week), "to", max(GLOBAL_TOP_10$week))
total_shows <- GLOBAL_TOP_10 |> distinct(show_title) |> nrow()

library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

# Initial Data Exploration
library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

GLOBAL_TOP_10 |> 
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

# Drop season_tile
GLOBAL_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

# Convert to minutes
GLOBAL_TOP_10 |> 
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

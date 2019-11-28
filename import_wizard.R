library(googledrive)
library(keyring)
library(readxl)
library(yaml)
library(readr)
library(lubridate)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)

# dit programma ... -------------------------------------------------------

winterhh_pgm <-
  winterhh_pgm <- read_delim(
    "~/winterhh_pgm.txt",
    "\t",
    escape_double = FALSE,
    locale = locale(date_names = "nl",
                    tz = "Europe/Amsterdam"),
    trim_ws = TRUE
  ) %>%
  mutate(pgm_start = ymd_hms(pgmStart, tz = "Europe/Amsterdam"),
         pgm_stop = ymd_hms(pgmStop, tz = "Europe/Amsterdam")
  ) %>% 
  select(pgm_start, pgm_stop, everything(), -pgmStart, -pgmStop)

# wordt een herhaling van -------------------------------------------------

winterhh_wordt_hhvan <- read_delim(
  "~/winterhh_hhvan.txt",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "nl",
                  tz = "Europe/Amsterdam"),
  trim_ws = TRUE
) %>%
  mutate(pgm_start = ymd_hms(pgmStart, tz = "Europe/Amsterdam"),
         pgm_stop = ymd_hms(pgmStop, tz = "Europe/Amsterdam")
  ) %>% 
  select(pgm_start, pgm_stop, everything(), -pgmStart, -pgmStop)

# de koppeling is ------------------------------------------------------------

winterhh_link <- read_delim(
  "~/winterhh_link.txt",
  "\t",
  escape_double = FALSE,
  locale = locale(date_names = "nl",
                  tz = "Europe/Amsterdam"),
  trim_ws = TRUE
) %>%
  mutate(
    pgm_start_link_pgm = 
      ymd_hm(paste0(datum, " ", str_sub(uren, 1, 5)), tz = "Europe/Amsterdam"),
    pgm_stop_link_pgm = 
      ymd_hm(paste0(datum, " ", str_sub(uren, 9)), tz = "Europe/Amsterdam") - minutes(1L),
    pgm_start_link_hh = 
      ymd_hm(paste0(`herhaling van`, " ", str_sub(uren, 1, 5)), tz = "Europe/Amsterdam"),
    pgm_stop_link_hh = 
      ymd_hm(paste0(`herhaling van`, " ", str_sub(uren, 9)), tz = "Europe/Amsterdam") - minutes(1L)
  ) %>%
  select(-datum, -uren, -`herhaling van`)

# welke pgms zitten in de hh-periode? ------------------------------------------------------

dt_winterhh_pgm <- as.data.table(winterhh_pgm)
dt_winterhh_pgm[ , end := pgm_start]
dt_winterhh_link <- as.data.table(winterhh_link)

setkey(dt_winterhh_link, pgm_start_link_pgm, pgm_stop_link_pgm)

wi_pgms <-
  foverlaps(
    dt_winterhh_pgm,
    dt_winterhh_link,
    by.x = c("pgm_start", "end"),
    type = "within"
  )

setDF(wi_pgms)

wi_pgms_matched <- wi_pgms %>% 
  filter(!is.na(pgm_start_link_pgm)) %>% 
  select(pgm_start_link_pgm, pgm_start_link_hh, pgm_start, pgm_stop, pgmTaal, pgmID, pgmReplayOf, pgmTitel)
  

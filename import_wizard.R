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

get_hh_date <- function(pgm_date, hh_date) {
  result <-
    pgm_date %>% update(
      year = year(hh_date),
      month = month(hh_date),
      mday = day(hh_date),
      tz = "Europe/Amsterdam"
    )
}

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
  mutate(pgm_start_hh = ymd_hms(pgmStart, tz = "Europe/Amsterdam"),
         pgm_stop_hh = ymd_hms(pgmStop, tz = "Europe/Amsterdam"),
         pgm_taal_hh = pgmTaal
  ) %>% 
  select(pgm_start_hh,
         pgm_stop_hh,
         pgm_taal_hh,
         everything(),
         -pgmStart,
         -pgmStop,
         -pgmTaal
  )

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
  select(
    pgm_start_link_hh,
    pgm_start,
    pgm_stop,
    pgmTaal,
    pgmID,
    pgmReplayOf,
    pgmTitel
  ) %>% mutate(pgm_start_hh = get_hh_date(pgm_start, pgm_start_link_hh),
               pgm_stop_hh = pgm_start_hh + hours(pgm_stop - pgm_start),
               pgm_taal_hh = pgmTaal
  )

# voeg pgms en hh-pgms samen ----------------------------------------------

fmt_cz_date <- stamp("za. 2019-11-30, 14:00")

wi_pgms_joint <- wi_pgms_matched %>% 
  left_join(winterhh_wordt_hhvan, by = c("pgm_start_hh" = "pgm_start_hh", 
                                         "pgm_stop_hh" = "pgm_stop_hh",
                                         "pgm_taal_hh" = "pgm_taal_hh")) %>% 
  mutate(pgm_start_fmt = fmt_cz_date(pgm_start),
         pgm_stop_fmt = fmt_cz_date(pgm_stop),
         pgm_start_hh_fmt = fmt_cz_date(pgm_start_hh),
         pgm_stop_hh_fmt = fmt_cz_date(pgm_stop_hh))

# lijst voor GD -----------------------------------------------------------

wi_pgms_gd <- wi_pgms_joint %>% 
  filter(pgmTaal == "NL") %>% 
  select(pgm_start_fmt, pgmTitel.x, pgm_start_hh_fmt, pgmTitel.y)

# write_tsv(x = wi_pgms_gd, path = "~/wi_pgms_gd.tsv")

# sql-stm voor gids-update -----------------------------

wi_pgms_sqlcmd <- wi_pgms_joint %>%
  select(pgmID.x, pgmReplayOf.x, pgmID.y, pgmReplayOf.y) %>%
  mutate(
    sql_cmd = paste0(
      "update wp_postmeta set meta_value = ",
      if_else(pgmReplayOf.y > 0, pgmReplayOf.y, pgmID.y),
      " where post_id = ",
      pgmID.x,
      " and meta_key = 'pr_metadata_orig';"
    )
  ) %>% select(-starts_with("pgm"))

# write_tsv(x = wi_pgms_sqlcmd, path = "~/wi_pgms_sqlcmd.tsv")

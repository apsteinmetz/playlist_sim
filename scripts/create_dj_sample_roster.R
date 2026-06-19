#!/usr/bin/env Rscript

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

required_packages <- c("nanoparquet", "tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
suppressPackageStartupMessages(library(tidyverse))

input_path <- file.path("data", "playlists.parquet")
output_path <- file.path("data", "processed", "dj_sample_roster.csv")
sample_size <- 100L

if (!file.exists(input_path)) {
  stop("Source file not found: ", input_path, call. = FALSE)
}

normalize_track_key <- function(artist, title) {
  paste(tolower(str_trim(artist)), tolower(str_trim(title)), sep = "\r")
}

roster <- nanoparquet::read_parquet(
  input_path,
  col_select = c("DJ", "AirDate", "Artist", "Title")
) |>
  as_tibble() |>
  mutate(
    across(c(DJ, Artist, Title), str_trim),
    track_key = normalize_track_key(Artist, Title),
    airdate_num = as.numeric(AirDate)
  ) |>
  drop_na(DJ, Artist, Title, AirDate) |>
  filter(DJ != "", Artist != "", Title != "", !is.na(track_key)) |>
  arrange(DJ, desc(airdate_num)) |>
  distinct(DJ, track_key, .keep_all = TRUE) |>
  group_by(DJ) |>
  slice_head(n = sample_size) |>
  ungroup() |>
  mutate(dj_track_rank = row_number(), .by = DJ) |>
  select(DJ, dj_track_rank, AirDate, Artist, Title, track_key)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write_csv(roster, output_path)

message("Wrote ", output_path)
message("DJs: ", n_distinct(roster$DJ))
message("Tracks in roster: ", nrow(roster))
message("Unique track keys: ", n_distinct(roster$track_key))

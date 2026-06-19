#!/usr/bin/env Rscript

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

required_packages <- c("tidyverse")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
suppressPackageStartupMessages(library(tidyverse))

source(file.path("R", "dj_similarity.R"), local = FALSE)

roster_path <- file.path("data", "processed", "dj_sample_roster.csv")
master_path <- file.path("data", "processed", "master_track_features.csv")
profile_path <- file.path("data", "processed", "dj_profiles.csv")
long_path <- file.path("output", "dj_pairwise_similarity.csv")
matrix_path <- file.path("output", "dj_pairwise_similarity_matrix.csv")

if (!file.exists(roster_path)) {
  stop("Roster file not found: ", roster_path, call. = FALSE)
}
if (!file.exists(master_path)) {
  stop("Master feature file not found: ", master_path, call. = FALSE)
}

roster <- readr::read_csv(roster_path, show_col_types = FALSE) |>
  as_tibble() |>
  mutate(across(c(DJ, Artist, Title, track_key), as.character)) |>
  mutate(across(c(DJ, Artist, Title, track_key), stringr::str_trim))

master <- readr::read_csv(master_path, show_col_types = FALSE) |>
  as_tibble() |>
  mutate(across(c(track_key, spotify_id, reccobeats_status), as.character))

profiles <- build_dj_profiles(roster, master) |>
  arrange(DJ)

similarity <- compute_profile_similarity_matrix(profiles)

dir.create(dirname(profile_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(long_path), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(profiles, profile_path)
readr::write_csv(similarity$similarity_long, long_path)
readr::write_csv(
  tibble::rownames_to_column(as.data.frame(similarity$similarity_matrix), "DJ"),
  matrix_path
)

message("Wrote ", profile_path)
message("Wrote ", long_path)
message("Wrote ", matrix_path)
message("DJs: ", nrow(profiles))
message("Pairwise comparisons: ", nrow(similarity$similarity_long))

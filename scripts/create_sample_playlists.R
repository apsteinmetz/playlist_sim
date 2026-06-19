#!/usr/bin/env Rscript

# Create two reproducible, recency-ordered candidate playlists from the source
# Parquet data.

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  stop(
    "Package 'nanoparquet' is required. Install it with: ",
    "install.packages('nanoparquet', lib = 'renv/library')",
    call. = FALSE
  )
}

required_packages <- c("dplyr", "readr", "stringr", "tidyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "), ".",
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
seed <- if (length(args) >= 1L) as.integer(args[[1L]]) else 20260618L

if (is.na(seed)) {
  stop("The optional seed must be an integer.", call. = FALSE)
}

input_path <- file.path("data", "playlists.parquet")
output_paths <- file.path("data", c("playlist_1.csv", "playlist_2.csv"))
sample_size <- 1000L

if (!file.exists(input_path)) {
  stop("Source file not found: ", input_path, call. = FALSE)
}

playlists <- nanoparquet::read_parquet(
  input_path,
  col_select = c("DJ", "AirDate", "Artist", "Title")
)

required_columns <- c("DJ", "AirDate", "Artist", "Title")
missing_columns <- setdiff(required_columns, names(playlists))
if (length(missing_columns) > 0L) {
  stop(
    "Source data is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

playlists <- playlists |>
  dplyr::mutate(
    dplyr::across(c("DJ", "Artist", "Title"), stringr::str_trim)
  ) |>
  tidyr::drop_na(dplyr::all_of(required_columns)) |>
  dplyr::filter(
    DJ != "",
    Artist != "",
    Title != ""
  ) |>
  dplyr::mutate(
    AirDate_num = as.numeric(AirDate),
    track_key = paste(tolower(DJ), tolower(Artist), tolower(Title), sep = "\r")
  ) |>
  # Sort first so repeated songs retain their most recent airdate.
  dplyr::arrange(DJ, dplyr::desc(AirDate_num)) |>
  dplyr::distinct(track_key, .keep_all = TRUE) |>
  dplyr::select(dplyr::all_of(required_columns))

eligible_djs <- playlists |>
  dplyr::count(DJ, name = "track_count") |>
  dplyr::filter(track_count >= sample_size) |>
  dplyr::pull(DJ)

if (length(eligible_djs) < 2L) {
  stop(
    "At least two DJs must have ", sample_size,
    " distinct, complete artist/title pairs.",
    call. = FALSE
  )
}

set.seed(seed)
selected_djs <- sample(eligible_djs, size = 2L, replace = FALSE)

for (i in seq_along(selected_djs)) {
  sample_playlist <- playlists |>
    dplyr::filter(DJ == selected_djs[[i]]) |>
    dplyr::arrange(dplyr::desc(AirDate)) |>
    dplyr::slice_head(n = sample_size) |>
    dplyr::slice_sample(prop = 1)

  readr::write_csv(sample_playlist, output_paths[[i]])
}

tibble::tibble(
  seed = seed,
  playlist_1_dj = selected_djs[[1L]],
  playlist_2_dj = selected_djs[[2L]],
  output_1 = output_paths[[1L]],
  output_2 = output_paths[[2L]]
)

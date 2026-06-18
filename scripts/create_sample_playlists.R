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

text_columns <- c("DJ", "Artist", "Title")
playlists[text_columns] <- lapply(playlists[text_columns], trimws)
complete_rows <- stats::complete.cases(playlists[required_columns]) &
  playlists$DJ != "" & playlists$Artist != "" & playlists$Title != ""
playlists <- playlists[complete_rows, required_columns, drop = FALSE]

# Sort first so repeated songs retain their most recent airdate.
playlists <- playlists[
  order(playlists$DJ, -as.numeric(playlists$AirDate)),
  ,
  drop = FALSE
]
track_key <- paste(
  tolower(playlists$DJ),
  tolower(playlists$Artist),
  tolower(playlists$Title),
  sep = "\r"
)
playlists <- playlists[!duplicated(track_key), , drop = FALSE]

tracks_per_dj <- table(playlists$DJ)
eligible_djs <- names(tracks_per_dj[tracks_per_dj >= sample_size])

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
  dj_tracks <- playlists[playlists$DJ == selected_djs[[i]], , drop = FALSE]
  dj_tracks <- dj_tracks[
    order(dj_tracks$AirDate, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  recent_tracks <- utils::head(dj_tracks, sample_size)
  randomized_rows <- sample.int(
    nrow(recent_tracks),
    size = nrow(recent_tracks),
    replace = FALSE
  )
  sample_playlist <- recent_tracks[randomized_rows, , drop = FALSE]

  utils::write.csv(sample_playlist, output_paths[[i]], row.names = FALSE)
}

message("Seed: ", seed)
message(
  "Playlist 1 DJ: ", selected_djs[[1L]],
  " (1,000 most recent tracks, randomized order)"
)
message(
  "Playlist 2 DJ: ", selected_djs[[2L]],
  " (1,000 most recent tracks, randomized order)"
)
message("Wrote: ", paste(output_paths, collapse = ", "))

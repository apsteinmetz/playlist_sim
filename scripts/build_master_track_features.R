#!/usr/bin/env Rscript

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

required_packages <- c("tidyverse", "jsonlite", "curl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}
suppressPackageStartupMessages(library(tidyverse))

source(file.path("R", "track_feature_resolver.R"), local = FALSE)

credential_files <- c(
  ".Renviron",
  Sys.getenv("PLAYLIST_SIM_ENV_FILE", unset = ""),
  "G:/My Drive/Projects/R/.Renviron"
)
credential_files <- credential_files[nzchar(credential_files)]
credential_file <- credential_files[file.exists(credential_files)][1L]
if (!is.na(credential_file)) readRenviron(credential_file)

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) {
  stop("Spotify credentials were not found in the configured environment files.",
       call. = FALSE)
}

roster_path <- file.path("data", "processed", "dj_sample_roster.csv")
output_path <- file.path("data", "processed", "master_track_features.csv")
detail_path <- file.path("output", "master_track_feature_detail.csv")
batch_size <- 25L

if (!file.exists(roster_path)) {
  stop("Roster file not found: ", roster_path, call. = FALSE)
}

roster <- readr::read_csv(roster_path, show_col_types = FALSE) |>
  as_tibble() |>
  mutate(across(c(DJ, Artist, Title, track_key), as.character)) |>
  mutate(across(c(DJ, Artist, Title, track_key), str_trim)) |>
  filter(!is.na(track_key), track_key != "")

existing <- if (file.exists(output_path)) {
  readr::read_csv(output_path, show_col_types = FALSE) |>
    as_tibble()
} else {
  tibble()
}

track_table <- roster |>
  distinct(track_key, .keep_all = TRUE) |>
  select(track_key, Artist, Title) |>
  arrange(track_key)

if (nrow(existing) > 0L && "track_key" %in% names(existing)) {
  track_table <- track_table |>
    anti_join(existing |> distinct(track_key), by = "track_key")
}

if (nrow(track_table) == 0L) {
  message("No unresolved tracks remain. Master feature file is already complete.")
  quit(save = "no", status = 0L)
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(detail_path), recursive = TRUE, showWarnings = FALSE)

spotify_interval <- max(
  0.25, as.numeric(Sys.getenv("SPOTIFY_MIN_INTERVAL_SECONDS", "0.25"))
)
reccobeats_interval <- max(
  0.50, as.numeric(Sys.getenv("RECCOBEATS_MIN_INTERVAL_SECONDS", "0.50"))
)

token <- spotify_token(client_id, client_secret)
auth_header <- c(Authorization = paste("Bearer", token))
playlist_name <- "master_track_features"

master <- existing
if (nrow(master) == 0L) {
  master <- tibble(
    track_key = character(),
    Artist = character(),
    Title = character(),
    spotify_status = character(),
    spotify_id = character(),
    matched_artist = character(),
    matched_title = character(),
    artist_score = numeric(),
    title_score = numeric(),
    match_score = numeric(),
    reccobeats_status = character(),
    reccobeats_id = character(),
    acousticness = numeric(),
    danceability = numeric(),
    energy = numeric(),
    instrumentalness = numeric(),
    key = numeric(),
    liveness = numeric(),
    loudness = numeric(),
    mode = numeric(),
    speechiness = numeric(),
    tempo = numeric(),
    valence = numeric()
  )
}
seen_spotify_ids <- master |>
  filter(reccobeats_status == "matched", !is.na(spotify_id), spotify_id != "") |>
  pull(spotify_id) |>
  unique()

feature_fields <- c(
  "reccobeats_status", "reccobeats_id", "acousticness", "danceability", "energy",
  "instrumentalness", "key", "liveness", "loudness", "mode", "speechiness",
  "tempo", "valence"
)

fill_from_lookup <- function(df) {
  lookup <- df |>
    filter(reccobeats_status == "matched", !is.na(spotify_id), spotify_id != "") |>
    select(spotify_id, all_of(feature_fields)) |>
    distinct(spotify_id, .keep_all = TRUE) |>
    rename_with(~ paste0(.x, "_lookup"), -spotify_id)

  if (nrow(lookup) == 0L) return(df)

  df |>
    left_join(lookup, by = "spotify_id") |>
    mutate(
      reccobeats_status = coalesce(reccobeats_status_lookup, reccobeats_status),
      reccobeats_id = coalesce(reccobeats_id_lookup, reccobeats_id),
      acousticness = coalesce(acousticness_lookup, acousticness),
      danceability = coalesce(danceability_lookup, danceability),
      energy = coalesce(energy_lookup, energy),
      instrumentalness = coalesce(instrumentalness_lookup, instrumentalness),
      key = coalesce(key_lookup, key),
      liveness = coalesce(liveness_lookup, liveness),
      loudness = coalesce(loudness_lookup, loudness),
      mode = coalesce(mode_lookup, mode),
      speechiness = coalesce(speechiness_lookup, speechiness),
      tempo = coalesce(tempo_lookup, tempo),
      valence = coalesce(valence_lookup, valence)
    ) |>
    select(-ends_with("_lookup"))
}

for (batch_start in seq.int(1L, nrow(track_table), by = batch_size)) {
  batch_end <- min(batch_start + batch_size - 1L, nrow(track_table))
  batch <- track_table |> slice(batch_start:batch_end)

  batch_resolved <- pmap_dfr(
    batch,
    function(track_key, Artist, Title) {
      resolved <- resolve_spotify_track(
        Artist, Title, playlist_name, batch_start, auth_header, spotify_interval
      )
      tibble(
        track_key = track_key,
        Artist = Artist,
        Title = Title,
        spotify_status = resolved$status %||% "missing",
        spotify_id = resolved$spotify_id %||% NA_character_,
        matched_artist = resolved$matched_artist %||% NA_character_,
        matched_title = resolved$matched_title %||% NA_character_,
        artist_score = resolved$artist_score %||% NA_real_,
        title_score = resolved$title_score %||% NA_real_,
        match_score = resolved$match_score %||% NA_real_,
        reccobeats_status = if_else(
          resolved$status %in% c("exact", "matched"), "missing", "not_requested"
        ),
        reccobeats_id = NA_character_,
        acousticness = NA_real_,
        danceability = NA_real_,
        energy = NA_real_,
        instrumentalness = NA_real_,
        key = NA_real_,
        liveness = NA_real_,
        loudness = NA_real_,
        mode = NA_real_,
        speechiness = NA_real_,
        tempo = NA_real_,
        valence = NA_real_
      )
    }
  )

  request_rows <- batch_resolved |>
    filter(
      reccobeats_status == "missing",
      !is.na(spotify_id),
      spotify_id != "",
      !spotify_id %in% seen_spotify_ids
    ) |>
    distinct(spotify_id, .keep_all = TRUE)

  if (nrow(request_rows) > 0L) {
    response <- fetch_reccobeats_features(
      request_rows$spotify_id, playlist_name, batch_start, reccobeats_interval
    )

    if (!response$ok) {
      batch_resolved <- batch_resolved |>
        mutate(
          reccobeats_status = if_else(
            reccobeats_status == "missing",
            paste0("error_", response$status %||% "network"),
            reccobeats_status
          )
        )
    } else {
      features <- map_dfr(response$body$content %||% list(), function(feature) {
        spotify_id <- sub(".*/", "", feature$href %||% "")
        tibble(
          spotify_id = spotify_id,
          reccobeats_status = "matched",
          reccobeats_id = feature$id %||% NA_character_,
          acousticness = feature$acousticness %||% NA_real_,
          danceability = feature$danceability %||% NA_real_,
          energy = feature$energy %||% NA_real_,
          instrumentalness = feature$instrumentalness %||% NA_real_,
          key = feature$key %||% NA_real_,
          liveness = feature$liveness %||% NA_real_,
          loudness = feature$loudness %||% NA_real_,
          mode = feature$mode %||% NA_real_,
          speechiness = feature$speechiness %||% NA_real_,
          tempo = feature$tempo %||% NA_real_,
          valence = feature$valence %||% NA_real_
        )
      }) |>
        distinct(spotify_id, .keep_all = TRUE)

      if (nrow(features) > 0L) {
        batch_resolved <- batch_resolved |>
          left_join(features, by = "spotify_id", suffix = c("", "_feat")) |>
          mutate(
            reccobeats_status = coalesce(reccobeats_status_feat, reccobeats_status),
            reccobeats_id = coalesce(reccobeats_id_feat, reccobeats_id),
            acousticness = coalesce(acousticness_feat, acousticness),
            danceability = coalesce(danceability_feat, danceability),
            energy = coalesce(energy_feat, energy),
            instrumentalness = coalesce(instrumentalness_feat, instrumentalness),
            key = coalesce(key_feat, key),
            liveness = coalesce(liveness_feat, liveness),
            loudness = coalesce(loudness_feat, loudness),
            mode = coalesce(mode_feat, mode),
            speechiness = coalesce(speechiness_feat, speechiness),
            tempo = coalesce(tempo_feat, tempo),
            valence = coalesce(valence_feat, valence)
          ) |>
          select(-ends_with("_feat"))
      }
    }
  }

  seen_spotify_ids <- union(
    seen_spotify_ids,
    batch_resolved |>
      filter(reccobeats_status == "matched", !is.na(spotify_id), spotify_id != "") |>
      pull(spotify_id)
  )

  master <- bind_rows(master, batch_resolved) |> fill_from_lookup()

  readr::write_csv(master, output_path)
  readr::write_csv(master, detail_path)

  message(
    "Processed ", nrow(master), " tracks; matched ",
    sum(master$reccobeats_status == "matched", na.rm = TRUE), " / ", nrow(track_table)
  )
}

message("Wrote ", output_path)
message("Wrote ", detail_path)

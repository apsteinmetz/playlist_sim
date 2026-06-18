#!/usr/bin/env Rscript

# Search recency-ordered candidate queues until 100 complete ReccoBeats feature
# records have been collected for each DJ, or 1,000 candidates are exhausted.

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

credential_files <- c(
  ".Renviron",
  Sys.getenv("PLAYLIST_SIM_ENV_FILE", unset = ""),
  "G:/My Drive/Projects/R/.Renviron"
)
credential_files <- credential_files[nzchar(credential_files)]
credential_file <- credential_files[file.exists(credential_files)][1L]
if (!is.na(credential_file)) readRenviron(credential_file)

required_packages <- c("curl", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

`%||%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}

normalize_text <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[^[:alnum:]]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

text_similarity <- function(x, y) {
  x <- normalize_text(x)
  y <- normalize_text(y)
  denominator <- max(nchar(x), nchar(y), 1L)
  max(0, 1 - as.numeric(utils::adist(x, y)) / denominator)
}

header_value <- function(headers, name) {
  lines <- unlist(strsplit(rawToChar(headers), "\\r?\\n"))
  hits <- grep(paste0("^", name, ":"), lines, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0L) return(NULL)
  trimws(sub("^[^:]+:", "", tail(hits, 1L)))
}

request_state <- new.env(parent = emptyenv())

wait_for_slot <- function(service, min_interval) {
  last_request <- request_state[[service]]
  if (!is.null(last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
    if (elapsed < min_interval) Sys.sleep(min_interval - elapsed)
  }
}

read_cache <- function(cache_file, request_url) {
  if (!file.exists(cache_file)) return(NULL)
  cached <- tryCatch(
    jsonlite::read_json(cache_file, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(cached) || !identical(cached$request_url, request_url)) return(NULL)
  cached$response
}

write_cache <- function(cache_file, request_url, response) {
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(request_url = request_url, response = response),
    cache_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
}

get_json <- function(url, service, min_interval, headers = character(),
                     cache_file, max_attempts = 5L) {
  cached <- read_cache(cache_file, url)
  if (!is.null(cached)) {
    return(list(ok = TRUE, status = 200L, body = cached, cached = TRUE))
  }

  for (attempt in seq_len(max_attempts)) {
    wait_for_slot(service, min_interval)
    handle <- curl::new_handle(
      useragent = "playlist-sim/0.1 (playlist similarity research)"
    )
    if (length(headers) > 0L) {
      do.call(curl::handle_setheaders, c(list(handle = handle), as.list(headers)))
    }
    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = identity
    )
    request_state[[service]] <- Sys.time()

    if (inherits(response, "error")) {
      if (attempt == max_attempts) {
        return(list(
          ok = FALSE, status = NA_integer_, error = conditionMessage(response),
          body = NULL
        ))
      }
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }

    status <- response$status_code
    body_text <- rawToChar(response$content)
    body <- tryCatch(
      jsonlite::fromJSON(body_text, simplifyVector = FALSE),
      error = function(e) list(raw_body = body_text)
    )
    if (status >= 200L && status < 300L) {
      write_cache(cache_file, url, body)
      return(list(ok = TRUE, status = status, body = body, cached = FALSE))
    }

    if (status == 429L && attempt < max_attempts) {
      retry_after <- suppressWarnings(as.numeric(
        header_value(response$headers, "Retry-After") %||% NA_character_
      ))
      wait_seconds <- if (is.na(retry_after)) min(60, 2^(attempt - 1L)) else retry_after
      Sys.sleep(max(wait_seconds, min_interval) + stats::runif(1L, 0, 0.25))
      next
    }
    if (status >= 500L && attempt < max_attempts) {
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }
    return(list(ok = FALSE, status = status, error = paste("HTTP", status), body = body))
  }
}

spotify_token <- function(client_id, client_secret) {
  credentials <- gsub(
    "\\s", "",
    jsonlite::base64_enc(charToRaw(paste0(client_id, ":", client_secret)))
  )
  handle <- curl::new_handle(
    useragent = "playlist-sim/0.1 (playlist similarity research)",
    postfields = "grant_type=client_credentials"
  )
  curl::handle_setheaders(
    handle,
    Authorization = paste("Basic", credentials),
    `Content-Type` = "application/x-www-form-urlencoded"
  )
  response <- curl::curl_fetch_memory(
    "https://accounts.spotify.com/api/token", handle = handle
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop("Spotify authentication failed with HTTP ", response$status_code, ".",
         call. = FALSE)
  }
  jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)$access_token
}

score_spotify_items <- function(items, artist, title) {
  if (length(items) == 0L) return(NULL)
  rows <- lapply(items, function(item) {
    artist_names <- vapply(item$artists %||% list(), function(x) x$name, character(1))
    artist_scores <- vapply(artist_names, text_similarity, numeric(1), y = artist)
    artist_score <- if (length(artist_scores) == 0L) 0 else max(artist_scores)
    title_score <- text_similarity(item$name, title)
    data.frame(
      spotify_id = item$id %||% NA_character_,
      matched_artist = paste(artist_names, collapse = "; "),
      matched_title = item$name %||% NA_character_,
      artist_score = artist_score,
      title_score = title_score,
      match_score = 0.4 * artist_score + 0.6 * title_score,
      popularity = item$popularity %||% NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

classify_match <- function(scored) {
  if (is.null(scored) || nrow(scored) == 0L) return("missing")
  scored <- scored[order(-scored$match_score, -scored$popularity), , drop = FALSE]
  best <- scored[1L, , drop = FALSE]
  second_score <- if (nrow(scored) >= 2L) scored$match_score[[2L]] else 0
  margin <- best$match_score[[1L]] - second_score
  if (best$title_score >= 0.98 && best$artist_score >= 0.98) return("exact")
  if (best$match_score >= 0.85 && margin >= 0.08) return("matched")
  if (best$match_score >= 0.70) return("ambiguous")
  "missing"
}

resolve_spotify <- function(row, playlist_name, rank, auth_header, interval) {
  cache_dir <- file.path("data", "cache", "spotify_recent", playlist_name)
  strict_query <- paste0(
    "track:\"", row$Title, "\" artist:\"", row$Artist, "\""
  )
  strict_url <- paste0(
    "https://api.spotify.com/v1/search?q=",
    utils::URLencode(strict_query, reserved = TRUE), "&type=track&limit=5"
  )
  response <- get_json(
    strict_url, "spotify", interval, auth_header,
    file.path(cache_dir, sprintf("%04d_strict.json", rank))
  )
  if (!response$ok) {
    return(list(status = paste0("error_", response$status %||% "network")))
  }
  scored <- score_spotify_items(
    response$body$tracks$items %||% list(), row$Artist, row$Title
  )
  best_score <- if (is.null(scored)) 0 else max(scored$match_score)

  if (best_score < 0.70) {
    broad_query <- paste(row$Artist, row$Title)
    broad_url <- paste0(
      "https://api.spotify.com/v1/search?q=",
      utils::URLencode(broad_query, reserved = TRUE), "&type=track&limit=5"
    )
    broad_response <- get_json(
      broad_url, "spotify", interval, auth_header,
      file.path(cache_dir, sprintf("%04d_broad.json", rank))
    )
    if (broad_response$ok) {
      broad_scored <- score_spotify_items(
        broad_response$body$tracks$items %||% list(), row$Artist, row$Title
      )
      if (!is.null(broad_scored)) {
        scored <- if (is.null(scored)) broad_scored else rbind(scored, broad_scored)
        scored <- scored[!duplicated(scored$spotify_id), , drop = FALSE]
      }
    }
  }

  status <- classify_match(scored)
  if (is.null(scored) || nrow(scored) == 0L) return(list(status = status))
  scored <- scored[order(-scored$match_score, -scored$popularity), , drop = FALSE]
  best <- scored[1L, , drop = FALSE]
  list(
    status = status,
    spotify_id = best$spotify_id[[1L]],
    matched_artist = best$matched_artist[[1L]],
    matched_title = best$matched_title[[1L]],
    artist_score = best$artist_score[[1L]],
    title_score = best$title_score[[1L]],
    match_score = best$match_score[[1L]]
  )
}

feature_columns <- c(
  "acousticness", "danceability", "energy", "instrumentalness", "key",
  "liveness", "loudness", "mode", "speechiness", "tempo", "valence"
)

empty_attempts <- function(candidates, playlist_name) {
  x <- candidates[0, c("DJ", "AirDate", "Artist", "Title"), drop = FALSE]
  x$playlist <- character()
  x$candidate_rank <- integer()
  x$spotify_status <- character()
  x$spotify_id <- character()
  x$matched_artist <- character()
  x$matched_title <- character()
  x$artist_score <- numeric()
  x$title_score <- numeric()
  x$match_score <- numeric()
  x$reccobeats_status <- character()
  x$reccobeats_id <- character()
  for (column in feature_columns) x[[column]] <- numeric()
  x$selected <- logical()
  x
}

collect_playlist <- function(path, playlist_name, auth_header,
                             spotify_interval, reccobeats_interval,
                             target = 100L, batch_size = 25L) {
  candidates <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("DJ", "AirDate", "Artist", "Title")
  if (!all(required %in% names(candidates))) {
    stop(path, " must contain DJ, AirDate, Artist, and Title.", call. = FALSE)
  }
  candidates <- utils::head(candidates[required], 1000L)
  attempts <- empty_attempts(candidates, playlist_name)
  seen_spotify_ids <- character()

  for (batch_start in seq.int(1L, nrow(candidates), by = batch_size)) {
    if (sum(attempts$reccobeats_status == "matched") >= target) break
    batch_end <- min(batch_start + batch_size - 1L, nrow(candidates))
    indices <- seq.int(batch_start, batch_end)
    batch <- candidates[indices, , drop = FALSE]
    batch$playlist <- playlist_name
    batch$candidate_rank <- indices
    batch$spotify_status <- NA_character_
    batch$spotify_id <- NA_character_
    batch$matched_artist <- NA_character_
    batch$matched_title <- NA_character_
    batch$artist_score <- NA_real_
    batch$title_score <- NA_real_
    batch$match_score <- NA_real_
    batch$reccobeats_status <- "not_requested"
    batch$reccobeats_id <- NA_character_
    for (column in feature_columns) batch[[column]] <- NA_real_
    batch$selected <- FALSE

    for (j in seq_len(nrow(batch))) {
      resolved <- resolve_spotify(
        batch[j, , drop = FALSE], playlist_name, batch$candidate_rank[[j]],
        auth_header, spotify_interval
      )
      batch$spotify_status[[j]] <- resolved$status
      for (column in c(
        "spotify_id", "matched_artist", "matched_title", "artist_score",
        "title_score", "match_score"
      )) {
        batch[[column]][[j]] <- resolved[[column]] %||% NA
      }
      if (batch$spotify_status[[j]] %in% c("exact", "matched")) {
        if (batch$spotify_id[[j]] %in% seen_spotify_ids) {
          batch$spotify_status[[j]] <- "duplicate_spotify_id"
        } else {
          seen_spotify_ids <- c(seen_spotify_ids, batch$spotify_id[[j]])
          batch$reccobeats_status[[j]] <- "missing"
        }
      }
    }

    requested <- which(batch$reccobeats_status == "missing")
    if (length(requested) > 0L) {
      id_query <- paste0(
        "ids=", utils::URLencode(batch$spotify_id[requested], reserved = TRUE),
        collapse = "&"
      )
      url <- paste0("https://api.reccobeats.com/v1/audio-features?", id_query)
      response <- get_json(
        url, "reccobeats", reccobeats_interval, character(),
        file.path(
          "data", "cache", "reccobeats_recent", playlist_name,
          sprintf("batch_%04d.json", batch_start)
        )
      )
      if (!response$ok) {
        batch$reccobeats_status[requested] <- paste0(
          "error_", response$status %||% "network"
        )
      } else {
        for (feature in response$body$content %||% list()) {
          spotify_id <- sub(".*/", "", feature$href %||% "")
          j <- match(spotify_id, batch$spotify_id)
          if (is.na(j)) next
          batch$reccobeats_status[[j]] <- "matched"
          batch$reccobeats_id[[j]] <- feature$id %||% NA_character_
          for (column in feature_columns) {
            batch[[column]][[j]] <- feature[[column]] %||% NA_real_
          }
        }
      }
    }

    attempts <- rbind(attempts, batch)
    selected_rows <- which(attempts$reccobeats_status == "matched")
    attempts$selected <- FALSE
    attempts$selected[utils::head(selected_rows, target)] <- TRUE

    utils::write.csv(
      attempts,
      file.path("output", paste0(playlist_name, "_collection_detail.csv")),
      row.names = FALSE
    )
    utils::write.csv(
      attempts[attempts$selected, ],
      file.path("data", "processed", paste0(playlist_name, "_features.csv")),
      row.names = FALSE
    )
    message(
      playlist_name, ": attempted ", nrow(attempts), ", complete matches ",
      sum(attempts$reccobeats_status == "matched"), "/", target
    )
  }
  attempts
}

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) {
  stop("Spotify credentials were not found in the configured environment files.",
       call. = FALSE)
}

spotify_interval <- max(
  0.25, as.numeric(Sys.getenv("SPOTIFY_MIN_INTERVAL_SECONDS", "0.25"))
)
reccobeats_interval <- max(
  0.50, as.numeric(Sys.getenv("RECCOBEATS_MIN_INTERVAL_SECONDS", "0.50"))
)
dir.create(file.path("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

token <- spotify_token(client_id, client_secret)
auth_header <- c(Authorization = paste("Bearer", token))
input_paths <- file.path("data", c("playlist_1.csv", "playlist_2.csv"))
playlist_names <- tools::file_path_sans_ext(basename(input_paths))

all_attempts <- lapply(seq_along(input_paths), function(i) {
  collect_playlist(
    input_paths[[i]], playlist_names[[i]], auth_header,
    spotify_interval, reccobeats_interval
  )
})
names(all_attempts) <- playlist_names
detail <- do.call(rbind, all_attempts)
row.names(detail) <- NULL

summary <- do.call(rbind, lapply(names(all_attempts), function(name) {
  x <- all_attempts[[name]]
  selected <- x[x$selected, , drop = FALSE]
  data.frame(
    playlist = name,
    DJ = unique(x$DJ)[[1L]],
    candidates_available = 1000L,
    candidates_attempted = nrow(x),
    spotify_resolved = sum(x$spotify_status %in% c("exact", "matched")),
    reccobeats_matches = sum(x$reccobeats_status == "matched"),
    selected_tracks = nrow(selected),
    target_reached = nrow(selected) == 100L,
    newest_selected_airdate = if (nrow(selected)) max(selected$AirDate) else NA,
    oldest_selected_airdate = if (nrow(selected)) min(selected$AirDate) else NA,
    stringsAsFactors = FALSE
  )
}))
row.names(summary) <- NULL

utils::write.csv(detail, file.path("output", "feature_collection_detail.csv"),
                 row.names = FALSE)
utils::write.csv(summary, file.path("output", "feature_collection_summary.csv"),
                 row.names = FALSE)
print(summary, row.names = FALSE)
message("Wrote output/feature_collection_detail.csv")
message("Wrote output/feature_collection_summary.csv")


#!/usr/bin/env Rscript

# Resolve artist/title pairs with Spotify Search, then retrieve acoustic
# features from ReccoBeats. Successful responses are cached so reruns do not
# consume API capacity unnecessarily.

project_library <- file.path("renv", "library")
if (dir.exists(project_library)) {
  .libPaths(c(normalizePath(project_library), .libPaths()))
}

# --vanilla avoids unrelated startup code. Load credentials from a project-local
# file when present, otherwise use the environment file referenced by this
# machine's user-level .Rprofile.
credential_files <- c(
  ".Renviron",
  Sys.getenv("PLAYLIST_SIM_ENV_FILE", unset = ""),
  "G:/My Drive/Projects/R/.Renviron"
)
credential_files <- credential_files[nzchar(credential_files)]
credential_file <- credential_files[file.exists(credential_files)][1L]
if (!is.na(credential_file)) {
  readRenviron(credential_file)
}

required_packages <- c("curl", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "), ". ",
    "Install them with install.packages(c('curl', 'jsonlite'), ",
    "lib = 'renv/library').",
    call. = FALSE
  )
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
  text <- rawToChar(headers)
  lines <- unlist(strsplit(text, "\\r?\\n"))
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
  if (is.null(cache_file) || !file.exists(cache_file)) return(NULL)
  cached <- tryCatch(
    jsonlite::read_json(cache_file, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(cached) || !identical(cached$request_url, request_url)) return(NULL)
  cached$response
}

write_cache <- function(cache_file, request_url, response) {
  if (is.null(cache_file)) return(invisible(NULL))
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
                     cache_file = NULL, max_attempts = 5L) {
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
      do.call(
        curl::handle_setheaders,
        c(list(handle = handle), as.list(headers))
      )
    }

    response <- tryCatch(
      curl::curl_fetch_memory(url, handle = handle),
      error = identity
    )
    request_state[[service]] <- Sys.time()

    if (inherits(response, "error")) {
      if (attempt == max_attempts) {
        return(list(
          ok = FALSE, status = NA_integer_, body = NULL,
          error = conditionMessage(response), cached = FALSE
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

    if (status == 429L) {
      retry_after <- suppressWarnings(as.numeric(
        header_value(response$headers, "Retry-After") %||% NA_character_
      ))
      wait_seconds <- if (is.na(retry_after)) {
        min(60, 2^(attempt - 1L))
      } else {
        retry_after
      }
      if (attempt < max_attempts) {
        Sys.sleep(max(wait_seconds, min_interval) + stats::runif(1L, 0, 0.25))
        next
      }
    }

    if (status >= 500L && attempt < max_attempts) {
      Sys.sleep(min(60, 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25))
      next
    }

    return(list(
      ok = FALSE, status = status, body = body,
      error = paste("HTTP", status), cached = FALSE
    ))
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
    "https://accounts.spotify.com/api/token",
    handle = handle
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

input_paths <- file.path("data", c("playlist_1.csv", "playlist_2.csv"))
missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0L) {
  stop("Missing input files: ", paste(missing_inputs, collapse = ", "), call. = FALSE)
}

client_id <- Sys.getenv("SPOTIFY_CLIENT_ID")
client_secret <- Sys.getenv("SPOTIFY_CLIENT_SECRET")
if (!nzchar(client_id) || !nzchar(client_secret)) {
  stop(
    "Spotify credentials are required to resolve artist/title pairs. ",
    "Copy .Renviron.example to .Renviron, add SPOTIFY_CLIENT_ID and ",
    "SPOTIFY_CLIENT_SECRET, or set PLAYLIST_SIM_ENV_FILE to an existing ",
    "environment file, then rerun this script with --vanilla.",
    call. = FALSE
  )
}

spotify_interval <- max(0.25, as.numeric(
  Sys.getenv("SPOTIFY_MIN_INTERVAL_SECONDS", "0.25")
))
reccobeats_interval <- max(0.50, as.numeric(
  Sys.getenv("RECCOBEATS_MIN_INTERVAL_SECONDS", "0.50")
))

dir.create(file.path("data", "cache", "spotify"), recursive = TRUE,
           showWarnings = FALSE)
dir.create(file.path("data", "cache", "reccobeats"), recursive = TRUE,
           showWarnings = FALSE)
dir.create(file.path("data", "processed"), recursive = TRUE,
           showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

playlist_rows <- do.call(rbind, lapply(input_paths, function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("DJ", "Artist", "Title")
  if (!all(needed %in% names(x))) {
    stop(path, " must contain DJ, Artist, and Title columns.", call. = FALSE)
  }
  x$playlist <- tools::file_path_sans_ext(basename(path))
  x[needed <- c("playlist", needed)]
}))
row.names(playlist_rows) <- NULL

results <- playlist_rows
results$spotify_status <- NA_character_
results$spotify_id <- NA_character_
results$matched_artist <- NA_character_
results$matched_title <- NA_character_
results$artist_score <- NA_real_
results$title_score <- NA_real_
results$match_score <- NA_real_
results$reccobeats_status <- "not_requested"
results$reccobeats_id <- NA_character_

token <- spotify_token(client_id, client_secret)
auth_header <- c(Authorization = paste("Bearer", token))

message("Resolving ", nrow(results), " tracks with Spotify Search...")
for (i in seq_len(nrow(results))) {
  strict_query <- paste0(
    "track:\"", results$Title[[i]], "\" artist:\"", results$Artist[[i]], "\""
  )
  strict_url <- paste0(
    "https://api.spotify.com/v1/search?q=",
    utils::URLencode(strict_query, reserved = TRUE),
    "&type=track&limit=5"
  )
  response <- get_json(
    strict_url, service = "spotify", min_interval = spotify_interval,
    headers = auth_header,
    cache_file = file.path("data", "cache", "spotify", sprintf("%03d.json", i))
  )

  if (!response$ok) {
    results$spotify_status[[i]] <- paste0("error_", response$status %||% "network")
    next
  }

  items <- response$body$tracks$items %||% list()
  scored <- score_spotify_items(items, results$Artist[[i]], results$Title[[i]])

  # Fielded search is precise but can return no results for catalog spellings
  # that differ slightly. Try one broader, still bounded query before declaring
  # the track missing. This response is cached independently.
  best_strict_score <- if (is.null(scored) || nrow(scored) == 0L) {
    0
  } else {
    max(scored$match_score)
  }
  if (best_strict_score < 0.70) {
    broad_query <- paste(results$Artist[[i]], results$Title[[i]])
    broad_url <- paste0(
      "https://api.spotify.com/v1/search?q=",
      utils::URLencode(broad_query, reserved = TRUE),
      "&type=track&limit=5"
    )
    broad_response <- get_json(
      broad_url, service = "spotify", min_interval = spotify_interval,
      headers = auth_header,
      cache_file = file.path(
        "data", "cache", "spotify", sprintf("%03d_broad.json", i)
      )
    )
    if (broad_response$ok) {
      broad_items <- broad_response$body$tracks$items %||% list()
      broad_scored <- score_spotify_items(
        broad_items, results$Artist[[i]], results$Title[[i]]
      )
      if (!is.null(broad_scored)) {
        scored <- if (is.null(scored)) broad_scored else rbind(scored, broad_scored)
        scored <- scored[
          !duplicated(scored$spotify_id),
          ,
          drop = FALSE
        ]
      }
    }
  }

  status <- classify_match(scored)
  results$spotify_status[[i]] <- status

  if (!is.null(scored) && nrow(scored) > 0L) {
    scored <- scored[order(-scored$match_score, -scored$popularity), , drop = FALSE]
    best <- scored[1L, , drop = FALSE]
    for (column in c("spotify_id", "matched_artist", "matched_title",
                     "artist_score", "title_score", "match_score")) {
      results[[column]][[i]] <- best[[column]][[1L]]
    }
  }

  if (i %% 10L == 0L) message("  Spotify: ", i, "/", nrow(results))
}

usable <- which(results$spotify_status %in% c("exact", "matched"))
results$reccobeats_status[usable] <- "missing"
feature_columns <- c(
  "acousticness", "danceability", "energy", "instrumentalness", "key",
  "liveness", "loudness", "mode", "speechiness", "tempo", "valence"
)
for (column in feature_columns) results[[column]] <- NA_real_

if (length(usable) > 0L) {
  batch_size <- 25L
  batches <- split(usable, ceiling(seq_along(usable) / batch_size))
  message("Retrieving ReccoBeats features in ", length(batches), " batches...")

  for (batch_number in seq_along(batches)) {
    indices <- batches[[batch_number]]
    id_query <- paste0(
      "ids=", utils::URLencode(results$spotify_id[indices], reserved = TRUE),
      collapse = "&"
    )
    url <- paste0("https://api.reccobeats.com/v1/audio-features?", id_query)
    response <- get_json(
      url, service = "reccobeats", min_interval = reccobeats_interval,
      cache_file = file.path(
        "data", "cache", "reccobeats", sprintf("batch_%03d.json", batch_number)
      )
    )

    if (!response$ok) {
      results$reccobeats_status[indices] <- paste0(
        "error_", response$status %||% "network"
      )
      next
    }

    features <- response$body$content %||% list()
    for (feature in features) {
      spotify_id <- sub(".*/", "", feature$href %||% "")
      row_index <- indices[match(spotify_id, results$spotify_id[indices])]
      if (is.na(row_index)) next
      results$reccobeats_status[[row_index]] <- "matched"
      results$reccobeats_id[[row_index]] <- feature$id %||% NA_character_
      for (column in feature_columns) {
        results[[column]][[row_index]] <- feature[[column]] %||% NA_real_
      }
    }
    message("  ReccoBeats batch: ", batch_number, "/", length(batches))
  }
}

summarize_coverage <- function(x, label) {
  data.frame(
    playlist = label,
    total_tracks = nrow(x),
    spotify_exact = sum(x$spotify_status == "exact", na.rm = TRUE),
    spotify_matched = sum(x$spotify_status == "matched", na.rm = TRUE),
    spotify_ambiguous = sum(x$spotify_status == "ambiguous", na.rm = TRUE),
    spotify_missing = sum(x$spotify_status == "missing", na.rm = TRUE),
    spotify_errors = sum(grepl("^error_", x$spotify_status)),
    reccobeats_features = sum(x$reccobeats_status == "matched"),
    usable_coverage_pct = round(
      100 * sum(x$reccobeats_status == "matched") / nrow(x), 1
    ),
    stringsAsFactors = FALSE
  )
}

summary_rows <- lapply(split(results, results$playlist), function(x) {
  summarize_coverage(x, unique(x$playlist))
})
summary_rows[[length(summary_rows) + 1L]] <- summarize_coverage(results, "overall")
coverage_summary <- do.call(rbind, summary_rows)
row.names(coverage_summary) <- NULL

utils::write.csv(
  results, file.path("output", "api_coverage_detail.csv"), row.names = FALSE
)
utils::write.csv(
  coverage_summary, file.path("output", "api_coverage_summary.csv"), row.names = FALSE
)
utils::write.csv(
  results[results$reccobeats_status == "matched", ],
  file.path("data", "processed", "sample_playlist_features.csv"),
  row.names = FALSE
)

print(coverage_summary, row.names = FALSE)
message("Wrote output/api_coverage_detail.csv")
message("Wrote output/api_coverage_summary.csv")
message("Wrote data/processed/sample_playlist_features.csv")
